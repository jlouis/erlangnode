open Core.Std

exception Parse_error of string * int
exception Encode_error of string
                            
type t =
  | ET_Int of Int.t
  | ET_Atom of String.t

let decode rbyte rint _rstr _rbuf =
  match rbyte() with
  (* INTEGERS *)
  | 97 -> ET_Int (rbyte ())
  | 98 -> ET_Int (rint ())
  | n -> raise (Parse_error ("unknown term tag", n))

let encode wbyte wint wstr _wbuf term =
  match term with
  | ET_Int n when n >= 0 && n < 256 ->
     wbyte 97;
     wbyte n
  | ET_Int n ->
     wbyte 98;
     wint n
  | ET_Atom str ->
     match String.length str with
     | len when len < 256 ->
        wbyte 100;
        wbyte 0;
        wbyte len;
        wstr str
     | _ ->
        raise (Encode_error "Atom length exceeds 256 bytes")

(* The buffer code here is used to read and write terms from buffers *)
(* as an intermediary point of the data. It is not perfect, but it is *)
(* better than doing something else. *)
module Buffer = struct               
  let read buf =
    let offset = ref 0 in
    let rbyte () =
      let byte = int_of_char (Buffer.nth buf !offset) in
        incr offset;
        byte in
    let rint () = Int32.(
      let f a e = a + (shift_left (of_int_exn (rbyte ())) e) in
        to_int_exn (
            List.fold_left [24; 16; 8; 0] ~init:zero ~f:f)) in
    let istr len =
      let result = Buffer.sub buf !offset len in
        offset := !offset + len;
        result in
    let ibuf len =
      let result = Buffer.create len in
      let s = istr len in
      Buffer.add_string result s;
      result in
    match rbyte() with
    | 131 -> decode rbyte rint istr ibuf
    | n ->
       raise (Parse_error ("Eterm does not start with 131", n))

  let write buf term =
    let wbyte x = Buffer.add_char buf (char_of_int x) in
    let wint x =
      let x32 = Int32.of_int_exn x in
      List.iter
        ~f:(fun n ->
          wbyte Int32.(to_int_exn (bit_and (shift_right_logical x32 n)
                                          0xFFl)))
        [24;16;8;0] in
    let wstr = Buffer.add_string buf in
    let wbuf = Buffer.add_buffer buf in
      wbyte 131;
      encode wbyte wint wstr wbuf term
end
                  
let of_buffer = Buffer.read
    
let to_buffer = Buffer.write
      
  
                       
                   

                              