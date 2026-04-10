(*  Title:      ml/utils/robust_file_io.ML
    Author:     davidebonav
    Project:    TacticProfiler

    Safe file I/O with automatic creation of parent directories.
    Handles both Unix and Windows (via bash emulation) transparently.
*)

signature ROBUST_FILE_IO =
sig
  (* Read the entire content of a file as a string. *)
  val read_file_content : string -> string

  (* Write a string to a file, creating parent directories if needed. *)
  val write_file_content : string -> string -> unit

  (* Append a string to a file; if the file does not exist, create it. *)
  val append_to_file : string -> string -> unit

  (* Write a line (string + newline) to a file, creating parent directories if needed. *)
  val write_line : string -> string -> unit

  (* Append a line (string + newline) to a file; if the file does not exist, create it. *)
  val append_line : string -> string -> unit
end;

structure RobustFileIO : ROBUST_FILE_IO =
struct

  (* Ensure that the directory containing the given path exists.
     Creates all missing parent directories using Isabelle_System.make_directory. *)
  fun ensure_parent_directory_exists file_path =
    let
      val parent_dir = Path.dir file_path
    in
      if not (File.exists parent_dir) then
        Isabelle_System.make_directory parent_dir
      else
        ()
    end

  (* Platform‑aware append operation.
     On Unix systems it uses File.append directly.
     On Windows (detected via ML_System.platform) it falls back to a bash
     'echo -n' command to avoid line ending issues. *)
  fun platform_aware_append file_path file_content =
    let
      val is_windows = String.isSubstring "windows" ML_System.platform
    in
      if is_windows then
        let
          val escaped_path = File.bash_path file_path
          val escaped_content = Bash.string file_content
          val append_cmd = "echo -n " ^ escaped_content ^ " >> " ^ escaped_path
        in
          Isabelle_System.bash append_cmd;
          ()
        end
      else
        File.append file_path file_content
    end

  (* Read the entire content of the file given by the string path. *)
  fun read_file_content file_path_string =
    File.read (Path.explode file_path_string)

  (* Write a string to a file.
     Creates missing parent directories before writing. *)
  fun write_file_content file_path_string file_content =
    let
      val file_path = Path.explode file_path_string
    in
      ensure_parent_directory_exists file_path;
      File.write file_path file_content
    end

  (* Write a line (string terminated by newline) to a file.
     Equivalent to write_file_content with an added newline. *)
  fun write_line file_path_string line_content =
    write_file_content file_path_string (line_content ^ "\n")

  (* Append a string to a file.
     If the file does not exist, it is created (including parent directories). *)
  fun append_to_file file_path_string file_content =
    let
      val file_path = Path.explode file_path_string
    in
      if File.exists file_path then
        platform_aware_append file_path file_content
      else
        write_file_content file_path_string file_content
    end

  (* Append a line (string terminated by newline) to a file.
     If the file does not exist, it is created (including parent directories). *)
  fun append_line file_path_string line_content =
    append_to_file file_path_string (line_content ^ "\n")

end;