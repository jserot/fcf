open Fcf
open Printf
   
type cfg = {
  mutable avs_slave_id: string;
  mutable avs_addr_width: int;
  mutable avs_data_width: int;
  mutable ext_tcl_idx: int;
  mutable ip_group: string;
  mutable ip_rel_dir: string;
  mutable default_int_size: int;
  compiler: string;
  mutable base_support_unit: string;
  mutable globals_unit: string;
  mutable heap_support_unit: string;
  }

let cfg = {
  avs_slave_id = "s0";
  avs_addr_width = 4;
  avs_data_width = 32;
  ext_tcl_idx = 10000000;
  ip_group = "my_ips";
  ip_rel_dir = "../ip";
  default_int_size = 32;
  compiler = "fcfc";
  base_support_unit = "utils.vhd";
  globals_unit = "globals.vhd";
  heap_support_unit = "values.vhd"
  }
    
exception Too_many_ios of int * int
exception Illegal_type of string * string

(* Misc *)

let bin_addr v = Misc.bits_of_int cfg.avs_addr_width v

let full_name dir pfx sfx = dir ^ Filename.dir_sep ^ pfx ^ "." ^ sfx

let ios_of m = 
  let inps = List.filter (fun (n,_) -> n <> "start") m.Vhdl.v_inps in
  let outps = List.filter (fun (n,_) -> n <> "rdy") m.Vhdl.v_outps in
  let inp_offset = 1 in
  let outp_offset = List.length inps + 1 in
  inps,
  outps,
  (fun i -> bin_addr (inp_offset+i)),
  (fun i -> bin_addr (outp_offset+i))
                        
let size_of_type where t =
  let open Types in
  let err () = raise (Illegal_type (where, string_of_type t)) in
  let int_size sz = match real_attr sz with
    | Const (Width s) -> s
    | Const (Range (lo,hi)) -> Misc.bits_from_card (hi-lo+1)
    | _ -> cfg.default_int_size in
  match real_type t with
  | TyBool -> 1
  | TyInt (_, sz) -> int_size sz
  | _ -> err ()

let from_std_logic_vector src dst_ty = match Vhdl_types.vhdl_type_of dst_ty with
  | Vhdl_types.Unsigned sz -> Printf.sprintf "resize(unsigned(%s),%d)" src sz
  | Vhdl_types.Signed sz -> Printf.sprintf "resize(signed(%s),%d)" src sz
  | Vhdl_types.Integer _ -> Printf.sprintf "to_integer(unsigned(%s))" src
  | Vhdl_types.Std_logic -> Printf.sprintf "(%s).(0)" src
  | Vhdl_types.Variant vd -> src (* Values are already encoded as 32-bit std_logic_vectors - cf values.vhd *)
  | Vhdl_types.Litteral "block_t" -> src (* This is a hack *) 
  | ty -> failwith ("QSys backend: cannot convert type std_logic_vector to type " ^ (Vhdl_types.string_of_vhdl_type ty))

let to_std_logic_vector src src_ty sz = match Vhdl_types.vhdl_type_of src_ty with
  | Vhdl_types.Unsigned _ -> Printf.sprintf "std_logic_vector(resize(%s,%d))" src sz
  | Vhdl_types.Signed _ -> Printf.sprintf "std_logic_vector(resize(%s,%d))" src sz
  | Vhdl_types.Integer _ -> Printf.sprintf "std_logic_vector(to_unsigned(%s,%d))" src sz
  | Vhdl_types.Variant vd -> src (* Values are already encoded as 32-bit std_logic_vectors - cf values.vhd *)
  | Vhdl_types.Std_logic -> String.make (sz-1) '0' ^ src 
  | Vhdl_types.Litteral "block_t" -> src (* This is a hack *) 
  | ty -> failwith ("QSys backend: cannot convert type " ^ Vhdl_types.string_of_vhdl_type ty ^ " to type std_logic_vector")
    
(* Custom Component Avalon wrapper *)

let dump_cc_intf oc m = 
  let modname = m.Vhdl.v_name in
  let inps, outps, inp_addr, outp_addr = ios_of m in
  let nios = List.length inps + List.length outps + 1 in
  if nios > Misc.pow2 cfg.avs_addr_width then raise (Too_many_ios (nios,cfg.avs_addr_width));
  fprintf oc "entity %s_cc is\n" modname;
  fprintf oc "	port (\n";
  fprintf oc "		avs_%s_address     : in  std_logic_vector(%d downto 0)  := (others => '0');\n"
    cfg.avs_slave_id
    (cfg.avs_addr_width-1);
  fprintf oc "        -- %s : control/status register (b1=start, b0=rdy)\n" (bin_addr 0);
  List.iteri (fun i (n,_) -> fprintf oc "        -- %s : arg%d (%s)\n" (inp_addr i) (i+1) n) inps;
  List.iteri (fun i (n,_) -> fprintf oc "        -- %s : res%d (%s)\n" (outp_addr i) (i+1) n) outps;
  fprintf oc "		avs_%s_read        : in  std_logic                     := '0';\n" cfg.avs_slave_id;
  fprintf oc "		avs_%s_readdata    : out std_logic_vector(31 downto 0);\n" cfg.avs_slave_id;
  fprintf oc "		avs_%s_write       : in  std_logic                     := '0';\n" cfg.avs_slave_id;
  fprintf oc "		avs_%s_writedata   : in  std_logic_vector(31 downto 0) := (others => '0');\n" cfg.avs_slave_id;
  fprintf oc "		clock_clk          : in  std_logic                     := '0';\n";
  fprintf oc "		reset_reset        : in  std_logic                     := '0'\n";
  fprintf oc "	);\n";
  fprintf oc "end entity;\n";
  fprintf oc "\n"
    
let dump_cc_arch oc m = 
  let modname = m.Vhdl.v_name in
  let inps, outps, inp_addr, outp_addr = ios_of m in
  fprintf oc "architecture rtl of %s_cc is\n" modname;
  fprintf oc "\n";
  Vhdl.dump_module_intf "component" oc m;
  fprintf oc "\n";
  fprintf oc "  signal start : std_logic;\n";
  fprintf oc "  signal rdy : std_logic;\n";
  List.iter
    (fun (id,ty) -> fprintf oc "  signal %s: %s;\n" id (Vhdl_types.string_of_type ~type_marks:TM_Full ty))
    (inps @ outps);
  fprintf oc "  type write_state_t is (Idle, StartAsserted);\n";
  fprintf oc "  signal write_state: write_state_t;\n";
  fprintf oc "\n";
  fprintf oc "begin\n";
  fprintf oc "\n";
  fprintf oc "  %s_CC : component %s\n" (String.uppercase_ascii modname) modname;
  fprintf oc "		 port map (\n";
  fprintf oc "          start => start,\n";
  fprintf oc "          rdy => rdy,\n";
  List.iter
    (fun (id,_) -> fprintf oc "          %s => %s,\n" id id)
    (inps @ outps);
  fprintf oc "          clk => clock_clk,\n";
  fprintf oc "          rst => reset_reset\n";
  fprintf oc "		);\n";
  fprintf oc "\n";
  fprintf oc "  WRITE: process (clock_clk, reset_reset)\n";
  fprintf oc "  begin\n";
  fprintf oc "    if reset_reset = '1' then\n";
  fprintf oc "      write_state <= Idle;\n";
  fprintf oc "    elsif rising_edge(clock_clk) then \n";
  fprintf oc "      case write_state is\n";
  fprintf oc "        when StartAsserted =>\n";
  fprintf oc "          start <= '0';      \n";
  fprintf oc "          write_state <= Idle;\n";
  fprintf oc "        when Idle =>\n";
  fprintf oc "          if avs_%s_write = '1' then\n" cfg.avs_slave_id;
  fprintf oc "            case avs_%s_address is\n" cfg.avs_slave_id;
  fprintf oc "              when \"%s\" =>  -- writing CSR asserts start  for one clock period\n" (bin_addr 0);
  fprintf oc "                start <= '1';\n";
  fprintf oc "                write_state <= StartAsserted;\n";
  List.iteri
    (fun i (n,ty) ->
      let src = Printf.sprintf "avs_%s_writedata" cfg.avs_slave_id in 
      fprintf oc "              when \"%s\" =>\n" (inp_addr i); 
      fprintf oc "                %s <= %s;\n" n (from_std_logic_vector src ty))
    inps;
  fprintf oc "              when others =>\n";
  fprintf oc "                null; \n";
  fprintf oc "            end case;\n";
  fprintf oc "          end if;\n";
  fprintf oc "      end case;\n";
  fprintf oc "    end if;\n";
  fprintf oc "  end process;\n";
  fprintf oc "\n";
  fprintf oc "  READ: process (clock_clk)\n";
  fprintf oc "  begin\n";
  fprintf oc "    if rising_edge(clock_clk) then \n";
  fprintf oc "      if avs_%s_read = '1' then\n" cfg.avs_slave_id;
  fprintf oc "        case avs_%s_address is\n" cfg.avs_slave_id;
  fprintf oc "          when \"%s\" => avs_%s_readdata <= \"%s\" & rdy; -- when reading CSR, bit 0 is rdy\n"
    (bin_addr 0)
    cfg.avs_slave_id
    (String.make (cfg.avs_data_width-1) '0');
  List.iteri
    (fun i (n,ty) ->
      let src = to_std_logic_vector n ty cfg.avs_data_width in
      fprintf oc "          when \"%s\" => avs_%s_readdata <= %s;\n"
        (inp_addr i)
        cfg.avs_slave_id
        src)
    inps;
  List.iteri
    (fun i (n,ty) ->
      let src = to_std_logic_vector n ty cfg.avs_data_width in
      fprintf oc "          when \"%s\" => avs_%s_readdata <= %s;\n"
        (outp_addr i)
        cfg.avs_slave_id
        src)
    outps;
  fprintf oc "          when others => null; \n";
  fprintf oc "        end case;\n";
  fprintf oc "      end if;\n";
  fprintf oc "    end if;\n";
  fprintf oc "  end process;\n";
  fprintf oc "\n";
  fprintf oc "end architecture;\n"

let dump_cc_wrapper fname source m =
  let oc = open_out fname in
  fprintf oc "-- AVALON MM-slave wrapper around the core %s IP\n" m.Vhdl.v_name; 
  fprintf oc "-- Generated by %s on %s from source file %s\n\n" cfg.compiler (Utils.time_of_day ()) source;
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n";
  fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "\n";
  dump_cc_intf oc m;
  fprintf oc "\n";
  dump_cc_arch oc m;
  printf "Wrote file %s\n" fname;
  close_out oc

(* QSys TCL descriptors *)
(* This code is largely inspired from https://github.com/lsylvestre/macle/blob/main/src/target/gen_hw_tcl.ml *)

let dump_hw_tcl name fname source =
  let name_cc = name ^ "_cc" in
  (* let name_cc_up = String.uppercase_ascii name_cc in *)
  let oc = open_out fname in 
  fprintf oc "# TCL File Generated for Component Editor 15.1\n";
  fprintf oc "# %s \"%s\"\n" name_cc name_cc;
  fprintf oc "# request TCL package from ACDS 15.1\n";
  fprintf oc "#\n";
  fprintf oc "# Generated by %s on %s from source file %s\n\n" cfg.compiler (Utils.time_of_day ()) source;
  fprintf oc "package require -exact qsys 15.1\n";
  fprintf oc "#\n";
  fprintf oc "#module %s\n" name_cc;
  fprintf oc "#\n";
  fprintf oc "set_module_property DESCRIPTION \"\"\n";
  fprintf oc "set_module_property NAME %s\n" name_cc;
  fprintf oc "set_module_property VERSION 1.0\n";
  fprintf oc "set_module_property INTERNAL false\n";
  fprintf oc "set_module_property OPAQUE_ADDRESS_MAP true\n";
  fprintf oc "set_module_property GROUP %s\n" cfg.ip_group;
  fprintf oc "set_module_property AUTHOR \"\"\n";
  fprintf oc "set_module_property DISPLAY_NAME %s\n" name_cc;
  fprintf oc "set_module_property INSTANTIATE_IN_SYSTEM_MODULE true\n";
  fprintf oc "set_module_property EDITABLE true\n";
  fprintf oc "set_module_property REPORT_TO_TALKBACK false\n";
  fprintf oc "set_module_property ALLOW_GREYBOX_GENERATION false\n";
  fprintf oc "set_module_property REPORT_HIERARCHY false\n";
  fprintf oc "#\n";
  fprintf oc "# file sets\n";
  fprintf oc "#\n";
  fprintf oc "add_fileset QUARTUS_SYNTH QUARTUS_SYNTH \"\" \"\"\n";
  fprintf oc "set_fileset_property QUARTUS_SYNTH TOP_LEVEL %s\n" name_cc;
  fprintf oc "set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false\n";
  fprintf oc "set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false\n";
  fprintf oc "add_fileset_file %s.vhd VHDL PATH %s/%s.vhd TOP_LEVEL_FILE\n" name_cc cfg.ip_rel_dir name_cc;
  fprintf oc "add_fileset_file %s.vhd VHDL PATH %s/%s.vhd\n" name cfg.ip_rel_dir name;
  fprintf oc "add_fileset_file %s VHDL PATH %s/%s\n" cfg.base_support_unit cfg.ip_rel_dir cfg.base_support_unit; 
  fprintf oc "add_fileset_file %s VHDL PATH %s/%s\n" cfg.globals_unit cfg.ip_rel_dir cfg.globals_unit; 
  fprintf oc "#\n";
  fprintf oc "# parameters\n";
  fprintf oc "#\n";
  fprintf oc "#\n";
  fprintf oc "# display items\n";
  fprintf oc "#\n";
  fprintf oc "#\n";
  fprintf oc "# connection point s0\n";
  fprintf oc "#\n";
  fprintf oc "add_interface s0 avalon end\n";
  fprintf oc "set_interface_property s0 addressUnits WORDS\n";
  fprintf oc "set_interface_property s0 associatedClock clock\n";
  fprintf oc "set_interface_property s0 associatedReset reset\n";
  fprintf oc "set_interface_property s0 bitsPerSymbol 8\n";
  fprintf oc "set_interface_property s0 burstOnBurstBoundariesOnly false\n";
  fprintf oc "set_interface_property s0 burstcountUnits WORDS\n";
  fprintf oc "set_interface_property s0 explicitAddressSpan 0\n";
  fprintf oc "set_interface_property s0 holdTime 0\n";
  fprintf oc "set_interface_property s0 linewrapBursts false\n";
  fprintf oc "set_interface_property s0 maximumPendingReadTransactions 0\n";
  fprintf oc "set_interface_property s0 maximumPendingWriteTransactions 0\n";
  fprintf oc "set_interface_property s0 readLatency 0\n";
  fprintf oc "set_interface_property s0 readWaitTime 1\n";
  fprintf oc "set_interface_property s0 setupTime 0\n";
  fprintf oc "set_interface_property s0 timingUnits Cycles\n";
  fprintf oc "set_interface_property s0 writeWaitTime 0\n";
  fprintf oc "set_interface_property s0 ENABLED true\n";
  fprintf oc "set_interface_property s0 EXPORT_OF \"\"\n";
  fprintf oc "set_interface_property s0 PORT_NAME_MAP \"\"\n";
  fprintf oc "set_interface_property s0 CMSIS_SVD_VARIABLES \"\"\n";
  fprintf oc "set_interface_property s0 SVD_ADDRESS_GROUP \"\"\n";
  fprintf oc "\n";
  fprintf oc "add_interface_port s0 avs_s0_address address Input %d\n" cfg.avs_addr_width;
  fprintf oc "add_interface_port s0 avs_s0_write write Input 1\n";
  fprintf oc "add_interface_port s0 avs_s0_writedata writedata Input %d\n" cfg.avs_data_width;
  fprintf oc "add_interface_port s0 avs_s0_read read Input 1\n";
  fprintf oc "add_interface_port s0 avs_s0_readdata readdata Output %d\n" cfg.avs_data_width;
  fprintf oc "set_interface_assignment s0 embeddedsw.configuration.isFlash 0\n";
  fprintf oc "set_interface_assignment s0 embeddedsw.configuration.isMemoryDevice 0\n";
  fprintf oc "set_interface_assignment s0 embeddedsw.configuration.isNonVolatileStorage 0\n";
  fprintf oc "set_interface_assignment s0 embeddedsw.configuration.isPrintableDevice 0\n";
  fprintf oc "\n";
  fprintf oc "#\n";
  fprintf oc "# connection point clock\n";
  fprintf oc "#\n";
  fprintf oc "add_interface clock clock end\n";
  fprintf oc "set_interface_property clock clockRate 0\n";
  fprintf oc "set_interface_property clock ENABLED true\n";
  fprintf oc "set_interface_property clock EXPORT_OF \"\"\n";
  fprintf oc "set_interface_property clock PORT_NAME_MAP \"\"\n";
  fprintf oc "set_interface_property clock CMSIS_SVD_VARIABLES \"\"\n";
  fprintf oc "set_interface_property clock SVD_ADDRESS_GROUP \"\"\n";
  fprintf oc "\n";
  fprintf oc "add_interface_port clock clock_clk clk Input 1\n";
  fprintf oc "\n";
  fprintf oc "#\n";
  fprintf oc "# connection point reset\n";
  fprintf oc "#\n";
  fprintf oc "add_interface reset reset end\n";
  fprintf oc "set_interface_property reset associatedClock clock\n";
  fprintf oc "set_interface_property reset synchronousEdges DEASSERT\n";
  fprintf oc "set_interface_property reset ENABLED true\n";
  fprintf oc "set_interface_property reset EXPORT_OF \"\"\n";
  fprintf oc "set_interface_property reset PORT_NAME_MAP \"\"\n";
  fprintf oc "set_interface_property reset CMSIS_SVD_VARIABLES \"\"\n";
  fprintf oc "set_interface_property reset SVD_ADDRESS_GROUP \"\"\n";
  fprintf oc "\n";
  fprintf oc "add_interface_port reset reset_reset reset Input 1\n";
  printf "Wrote file %s\n" fname;
  close_out oc

(* let dump_ext_tcl name fname =
 *   let name_cc = name ^ "_cc" in
 *   (\* let name_cc_up = String.uppercase_ascii name_cc in *\)
 *   let oc = open_out fname in 
 *   fprintf oc "add_instance %s %s 1.0\n" name_cc name_cc;
 *   fprintf oc "add_connection cpu.data_master %s.s0 avalon\n" name_cc;
 *   fprintf oc "set_connection_parameter_value cpu.data_master/%s.s0 arbitrationPriority {1}\n" name_cc;
 *   fprintf oc "set_connection_parameter_value cpu.data_master/%s.s0 baseAddress {0x%08d}\n" name_cc cfg.ext_tcl_idx;
 *   fprintf oc "set_connection_parameter_value cpu.data_master/%s.s0 defaultConnection {0}\n" name_cc;
 *   cfg.ext_tcl_idx <- cfg.ext_tcl_idx + 1000;
 *   fprintf oc "add_connection clk.clk %s.clock clock\n" name_cc;
 *   fprintf oc "add_connection clk.clk_reset %s.reset reset\n" name_cc;
 *   printf "Wrote file %s\n" fname;
 *   close_out oc *)

(* Entry point *)

let write ~dir ~prefix ~src_file f = 
  let m = Vhdl.build_model f in
  let () = Utils.check_dir ~strict:true dir in
  dump_cc_wrapper (full_name (Utils.subdir dir "ip") (prefix ^ "_cc") "vhd") src_file m;
  dump_hw_tcl prefix (full_name (Utils.subdir dir "qsys") (prefix ^ "_cc_hw") "tcl") src_file
  (* dump_ext_tcl prefix (full_name dir (prefix ^ "_cc_ext") "tcl") *)
