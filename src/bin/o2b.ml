open Fcf
open Printf
   
type cfg = {
  avs_slave_id: string;
  avs_addr_width: int;
  }

let cfg = {
  avs_slave_id = "s0";
  avs_addr_width = 4;
  }
    
exception Too_many_ios of int * int

let bin_addr v = Misc.bits_of_int cfg.avs_addr_width v

let ios_of m = 
  let inps = List.filter (fun (n,_) -> n <> "start") m.Vhdl.v_inps in
  let outps = List.filter (fun (n,_) -> n <> "rdy") m.Vhdl.v_outps in
  let inp_offset = 1 in
  let outp_offset = List.length inps + 1 in
  inps,
  outps,
  (fun i -> bin_addr (inp_offset+i)),
  (fun i -> bin_addr (outp_offset+i))
                        

let dump_module_intf oc m = 
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
    
let dump_module_arch oc m = 
  let modname = m.Vhdl.v_name in
  let inps, outps, inp_addr, outp_addr = ios_of m in
  fprintf oc "architecture rtl of %s_cc is\n" modname;
  fprintf oc "\n";
  Vhdl.dump_module_intf "component" oc m;
  fprintf oc "\n";
  fprintf oc "  signal start : std_logic;\n";
  fprintf oc "  signal rdy : std_logic;\n";
  List.iter
    (fun (id,ty) -> fprintf oc "  signal %s: %s;\n" id (Vhdl.string_of_type ~type_marks:TM_Full ty))
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
    (fun i (n,_) -> fprintf oc "              when \"%s\" =>\n" (inp_addr i); 
                    fprintf oc "                %s <= unsigned(avs_%s_writedata);\n" n cfg.avs_slave_id)
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
  fprintf oc "          when \"%s\" => avs_%s_readdata <= \"0000000000000000000000000000000\" & rdy; -- when reading CSR, bit 0 is rdy\n" (bin_addr 0) cfg.avs_slave_id;
  List.iteri
    (fun i (n,_) -> fprintf oc "          when \"%s\" => avs_%s_readdata <= std_logic_vector(%s);\n"
                      (inp_addr i) cfg.avs_slave_id n)
    inps;
  List.iteri
    (fun i (n,_) -> fprintf oc "          when \"%s\" => avs_%s_readdata <= std_logic_vector(%s);\n"
                      (outp_addr i) cfg.avs_slave_id n)
    outps;
  fprintf oc "          when others => null; \n";
  fprintf oc "        end case;\n";
  fprintf oc "      end if;\n";
  fprintf oc "    end if;\n";
  fprintf oc "  end process;\n";
  fprintf oc "\n";
  fprintf oc "end architecture;\n"

let dump_model fname m =
  let oc = open_out fname in
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n";
  fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "\n";
  dump_module_intf oc m;
  fprintf oc "\n";
  dump_module_arch oc m;
  printf "Wrote file %s\n" fname;
  close_out oc

let write ?(dir="") ~prefix f = 
  let m = Vhdl.build_model f in
  (* let () = Misc.check_dir dir in *)
  let p = dir ^ Filename.dir_sep ^ prefix in
  dump_model (p ^ ".vhd") m
