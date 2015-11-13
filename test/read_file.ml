let read_channel channel =
  let buffer = Buffer.create 65536 in

  let rec repeat () = Buffer.add_channel buffer channel 1; repeat () in

  try repeat ()
  with End_of_file -> Buffer.contents buffer

let read_file filename =
  let channel = open_in filename in

  try
    let text = read_channel channel in
    close_in_noerr channel;
    text

  with exn ->
    close_in_noerr channel;
    raise exn
