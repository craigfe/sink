let ( let+ ) x f = Lwt.map f x
let ( let* ) = Lwt.bind

let exec cmd =
  let pipe_out, pipe_in = Lwt_unix.pipe () in
  let* process_status =
    Lwt_process.exec ~stderr:`Dev_null
      ~stdout:(`FD_move (Lwt_unix.unix_file_descr pipe_in))
      (Lwt_process.shell cmd)
  in
  let+ lines =
    Lwt_io.(of_fd ~mode:input) pipe_out
    |> Lwt_io.read_lines
    |> Lwt_stream.to_list
  in
  match process_status with
  | WEXITED 0 -> Ok lines
  | WEXITED 127 ->
      Error (`Command_not_found (String.split_on ' ' cmd |> List.hd_exn))
  | WEXITED n -> Result.errorf "Command \"%s\" failed with return code %d" cmd n
  | WSIGNALED _ | WSTOPPED _ ->
      Result.errorf "Command \"%s\" was interrupted" cmd
