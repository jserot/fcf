let time_of_day () =
  let t = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d at %02d:%02d:%02d"
    (t.Unix.tm_year+1900) (t.Unix.tm_mon+1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

exception Nonexistent_dir of string

let check_dir ~strict name = 
  if not (Sys.file_exists name && Sys.is_directory name) then
    begin
      if strict then raise (Nonexistent_dir name)
      else
        begin
          Printf.printf "Creating directory %s\n" name;
          Sys.mkdir name 0o777
        end
    end

let subdir dir d  = dir ^ Filename.dir_sep ^ d
