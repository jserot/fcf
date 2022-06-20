type config = {
  version: string;
  family: string;
  device: string;
  output_directory: string;
  }

let cfg = {
  version = "17.1.0";
  family = "MAX 10";
  device = "10M50DAF484C7G";
  output_directory = "output_files";
}

let write_qsf ~dir name models = 
  let open Printf in
  let fname = dir ^ Filename.dir_sep ^ name ^ ".qsf" in
  let oc = open_out fname in
  fprintf oc "set_global_assignment -name FAMILY \"%s\"\n" cfg.family;
  fprintf oc "set_global_assignment -name DEVICE %s\n" cfg.device;
  fprintf oc "set_global_assignment -name TOP_LEVEL_ENTITY %s\n" name;
  fprintf oc "set_global_assignment -name ORIGINAL_QUARTUS_VERSION %s\n" cfg.version;
  fprintf oc "set_global_assignment -name PROJECT_CREATION_TIME_DATE \"%s\"\n" @@ Utils.time_of_day ();
  fprintf oc "set_global_assignment -name PROJECT_OUTPUT_DIRECTORY %s\n" cfg.output_directory;
  fprintf oc "set_global_assignment -name MIN_CORE_JUNCTION_TEMP 0\n";
  fprintf oc "set_global_assignment -name MAX_CORE_JUNCTION_TEMP 85\n";
  fprintf oc "set_global_assignment -name ERROR_CHECK_FREQUENCY_DIVISOR 256\n";
  fprintf oc "set_global_assignment -name PARTITION_NETLIST_TYPE SOURCE -section_id Top\n";
  fprintf oc "set_global_assignment -name PARTITION_FITTER_PRESERVATION_LEVEL PLACEMENT_AND_ROUTING -section_id Top\n";
  fprintf oc "set_global_assignment -name PARTITION_COLOR 16764057 -section_id Top\n";
  fprintf oc "set_instance_assignment -name PARTITION_HIERARCHY root_partition -to | -section_id Top\n";
  fprintf oc "set_global_assignment -name NUM_PARALLEL_PROCESSORS 1\n";
  List.iter 
    (fun m -> fprintf oc "set_global_assignment -name VHDL_FILE %s.vhd\n" m)
    models;
  printf "Wrote file %s\n" fname;
  close_out oc

let write_qpf ~dir name =
  let open Printf in
  let fname = dir ^ Filename.dir_sep ^ name ^ ".qpf" in
  let oc = open_out fname in
  fprintf oc "QUARTUS_VERSION = \"%s\"\n" cfg.version;
  fprintf oc "DATE = \"%s\"\n" @@ Utils.time_of_day ();
  fprintf oc "PROJECT_REVISION = \"%s\"\n" name;
  printf "Wrote file %s\n" fname;
  close_out oc
