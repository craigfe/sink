open Sink

[@@@ocamlformat "disable"]

module%t List = struct
  let check m = Alcotest.(check (list int)) m

  let%t empty () = check "empty" List.empty []
end
