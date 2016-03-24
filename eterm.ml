open Core.Std
open Bignum.Std

exception Parse_error of string * int
exception Encode_error of string
exception Invariant
            
let parse_err what code = raise (Parse_error (what, code))

type t = 
  | ET_Int of Int32.t
  | ET_Atom of String.t
  | ET_String of String.t
  | ET_Float of float
  | ET_Binary of Buffer.t
  | ET_BitBinary of Buffer.t * int
  | ET_Pid of pid_ext
  | ET_Bigint of Bigint.t
  | ET_Tuple of t list
  | ET_List of t list
 and
  pid_ext = { node : string;
              id : Int32.t;
              serial : Int32.t;
              creation : int }
(* | ET_Port of string * Int32.t * Int32.t * int *)


let rec read_big rbyte = function
  | 0 -> Bigint.of_int 0
  | n ->
     let x256 = Bigint.of_int 256 in
     let b = rbyte () |> Bigint.of_int in
         let rest = read_big rbyte (n-1) in
         Bigint.( x256 * rest + b)

let rec split_big num =
  let n256 = Bigint.of_int 256 in
  match Bigint.( equal zero num ) with
  | true -> []
  | false ->
     Bigint.(
      let q = num / n256 in
      let r = num % n256 in
      to_int_exn r :: split_big q )
          
  

let rec decode rbyte rint rstr rbuf () =
  let decode_term = decode rbyte rint rstr rbuf in
  let rec list_of = function
    | 0 -> []
    | n ->
      let e = decode_term () in
        e :: list_of (n-1)
  in
  match rbyte() with
  (* INTEGERS *)
  | 97 -> ET_Int (Int32.of_int_exn (rbyte ()))
  | 98 -> ET_Int (rint ())
  (* FLOAT *)
  | 99 ->
     let s' = rstr 31 in
     let zeros = String.index_exn s' (char_of_int 0) in
     let s = String.sub s' ~pos:0 ~len:zeros in
     ET_Float (float_of_string s)
  (* STRINGS / ATOMS *)
  | (100 | 107) as c ->
     let len2 = rbyte () in
     let len1 = rbyte() in
     let len = 256 * len2 + len1 in
     let s = rstr len in
       (match c with
        | 100 -> ET_Atom s
        | 107 -> ET_String s
        | _ -> raise Invariant)
  (* BIGNUMS *)
  | 110 ->
     let n = rbyte () in
     let sign = rbyte () in
     let num = read_big rbyte n in
     ET_Bigint (if sign > 0 then Bigint.neg num else num)
  | 111 ->
     let n = rint () |> Int32.to_int_exn in
     let sign = rbyte () in
     let num = read_big rbyte n in
     ET_Bigint (if sign > 0 then Bigint.neg num else num)
  (* PID *)
  | 103 ->
     let node =
       match decode_term () with
       | ET_Atom atom -> atom
       | _ -> parse_err "Wrong format in Pid" 103 in
     let id = rint () in
     let serial = rint () in
     let creation = rbyte () in
       ET_Pid { node; id; serial; creation }
  (* TUPLE *)
  | 104 ->
     let arity = rbyte() in
       ET_Tuple (list_of arity)
  | 105 ->
     let arity = rint() |> Int32.to_int_exn in
       ET_Tuple (list_of arity)
  (* LIST *)
  | 106 -> ET_List []
  | 108 ->
     let len = rint() |> Int32.to_int_exn in
     let term = ET_List (list_of len) in
     (match rbyte() with
      | 106 -> term
      | code -> parse_err "Improper list" code)
  (* BINARY *)
  | 77 ->
     let len = rint () |> Int32.to_int_exn in
     let bits = rbyte () in
     ET_BitBinary (rbuf len, bits)
  | 109 ->
     let len = rint () |> Int32.to_int_exn in
       ET_Binary (rbuf len)
  | n -> raise (Parse_error ("unknown term tag", n))


let rec encode wbyte wint wstr wbuf term =
  let encode_term = encode wbyte wint wstr wbuf in
  match term with
  | ET_Int n ->
    (match Int32.to_int_exn n with
     | k when k >= 0 && k < 256 -> wbyte 97; wbyte k
     | k -> wbyte 98; wint k)
  | ET_Bigint n ->
     let sign = match Bigint.sign n with Neg -> 1 | _ -> 0 in
     let ds = split_big (Bigint.abs n) in
     (match List.length ds with
      | len when len < 256 ->
         wbyte 110;
         wbyte len;
         wbyte sign;
         List.iter ~f:wbyte ds
      | len ->
         wbyte 111;
         wint len;
         wbyte sign;
         List.iter ~f:wbyte ds)
  | ET_Float f ->
     let s = Printf.sprintf "%.20e" f in
     let pad = String.make (31 - String.length s) (char_of_int 0) in
     wbyte 99;
     wstr s;
     wstr pad
  | ET_Atom str ->
     (match String.length str with
      | len when len < 256 ->
         wbyte 100;
         wbyte 0;
         wbyte len;
         wstr str
      | _ ->
         raise (Encode_error "Atom length exceeds 256 bytes"))
  | ET_String str ->
     (match String.length str with
      | len when len < 65536 ->
          let a,b = len / 256, len mod 256 in
          wbyte 107;
          wbyte a;
          wbyte b;
          wstr str
      | len ->
         wbyte 108;
         wint len;
         (* Output a list of bytes to the Erlang world *)
         String.iter str
            ~f:(fun c -> wbyte 97; wbyte (int_of_char c));
         wbyte 106)
  | ET_Binary bin ->
    wbyte 109;
    wint (Buffer.length bin);
    wbuf bin
  | ET_BitBinary (buf, 0) -> encode_term (ET_Binary buf)
  | ET_BitBinary (buf, bits) ->
    wbyte 77;
    wint (Buffer.length buf);
    wbyte bits;
    wbuf buf
  | ET_Pid {node; id; serial; creation} ->
    wbyte 103;
    encode_term (ET_Atom node);
    wint (Int32.to_int_exn id);
    wint (Int32.to_int_exn serial);
    wbyte creation
  | ET_Tuple l ->
    (match List.length l with
     | len when len < 256 ->
         wbyte 104;
         wbyte len;
         List.iter ~f:encode_term l
     | len ->
         wbyte 105;
         wint len;
         List.iter ~f:encode_term l)
  | ET_List [] -> wbyte 106
  | ET_List lst ->
    wbyte 108;
    wint (List.length lst);
    List.iter ~f:encode_term lst;
    wbyte 106


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
        List.fold_left [24; 16; 8; 0] ~init:zero ~f:f) in
    let rstr len =
      let result = Buffer.sub buf !offset len in
        offset := !offset + len;
        result in
    let rbuf len =
      let result = Buffer.create len in
      let s = rstr len in
      Buffer.add_string result s;
      result in
    match rbyte() with
    | 131 -> decode rbyte rint rstr rbuf ()
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
      
