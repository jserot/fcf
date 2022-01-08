build:
	quartus_map --read_settings_files=on --write_settings_files=off top -c top
	quartus_fit --read_settings_files=off --write_settings_files=off top -c top
	quartus_asm --read_settings_files=off --write_settings_files=off top -c top
	quartus_sta top -c top

program:
	nios2-configure-sof output_files/top.sof
