open Core.Std

let tmp_buf = Buffer.create 256000

let request_buffer () =
  Buffer.clear tmp_buf;
  tmp_buf

let shrink_buf () =
    if Buffer.length tmp_buf > 10200300
    then Buffer.reset tmp_buf
    else ()

let read in_channel =
  let len =
    try input_binary_int in_channel with
      End_of_file -> exit 0 (* Port is closing *)
  in
  let buf = request_buffer () in
  Buffer.add_channel buf in_channel len;
  let term = Eterm.of_buffer buf in
  shrink_buf();
  term

let write out_channel term =
  let buf = request_buffer () in
  Eterm.to_buffer buf term;
  output_binary_int out_channel (Buffer.length buf);
  Buffer.output_buffer out_channel buf;
  flush stdout;
  shrink_buf ()

let interact f =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  let rec loop () =
    let term = read stdin in
    let reply = f term in
    write stdout reply;
    loop () in
  try loop () with
  | Eterm.Parse_error (_,_) -> ()
