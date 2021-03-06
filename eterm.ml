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
  | ET_Port of port_ext
  | ET_Bigint of Bigint.t
  | ET_Tuple of t list
  | ET_List of t list
  | ET_Map of (t * t) list
  | ET_Ref of ref_ext
  | ET_NRef of nref_ext
  | ET_Fun of fun_ext
  | ET_NFun of nf_ext
  | ET_Export of exp_ext
 and
  ref_ext = { ref_node : string;
              ref_id : Int32.t;
              ref_creation : int }
 and
  nref_ext = { nref_node : string;
               nref_creation : int;
               nref_ids : Int32.t list }
 and
  pid_ext = { pid_node : string;
              pid_id : Int32.t;
              pid_serial : Int32.t;
              pid_creation : int }
 and
  port_ext = { port_node : string;
               port_id : Int32.t;
               port_creation : int }
 and
  fun_ext = { fn_pid : t;
              fn_module : string;
              fn_index : Int32.t;
              fn_uniq : Int32.t;
              fn_free_vars : t list }
 and
  nf_ext = { nf_arity : int;
             nf_uniq : string;
             nf_index : Int32.t;
             nf_num_free : Int32.t;
             nf_module : string;
             nf_old_index : Int32.t;
             nf_old_uniq : Int32.t;
             nf_rest : string }
 and
  exp_ext = { exp_module : string;
              exp_function : string;
              exp_args : Int32.t }

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
          
  

let rec decode rbyte rint rint64 rstr rbuf () =
  let decode_term = decode rbyte rint rint64 rstr rbuf in
  let decode_atom opcode =
    match decode_term () with
    | ET_Atom a -> a
    | _ -> parse_err "Expected Atom for opcode" opcode in
  let rec list_of f = function
    | 0 -> []
    | n ->
      let e = f () in
        e :: list_of f (n-1) in
  let rec map_of = function
    | 0 -> []
    | n ->
       let k = decode_term () in
       let v = decode_term () in
       (k,v) :: map_of (n-1)
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
  | 70 ->
     let x = rint64 () in
     ET_Float (Int64.float_of_bits x)
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
     let pid_node =
       match decode_term () with
       | ET_Atom atom -> atom
       | _ -> parse_err "Wrong format in Pid" 103 in
     let pid_id = rint () in
     let pid_serial = rint () in
     let pid_creation = rbyte () in
       ET_Pid { pid_node;
                pid_id;
                pid_serial;
                pid_creation }
  (* PORT *)
  | 102 ->
     let port_node = decode_atom 102 in
     let port_id = rint () in
     let port_creation = rbyte () in
       ET_Port { port_node;
                 port_id;
                 port_creation }
  (* REF *)
  | 101 ->
     let ref_node = decode_atom 101 in
     let ref_id = rint () in
     let ref_creation = rbyte () in
       ET_Ref { ref_node;
                ref_id;
                ref_creation }
  (* NREF *)
  | 114 ->
     let len2 = rbyte () in
     let len1 = rbyte () in
     let len = 256 * len2 + len1 in
     let nref_node = decode_atom 114 in
     let nref_creation = rbyte () in
     let nref_ids = list_of rint len in
       ET_NRef { nref_node; nref_creation; nref_ids }
  (* TUPLE *)
  | 104 ->
     let arity = rbyte() in
       ET_Tuple (list_of decode_term arity)
  | 105 ->
     let arity = rint() |> Int32.to_int_exn in
       ET_Tuple (list_of decode_term arity)
  (* LIST *)
  | 106 -> ET_List []
  | 108 ->
     let len = rint() |> Int32.to_int_exn in
     let term = ET_List (list_of decode_term len) in
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
  | 116 ->
     let arity = rint () |> Int32.to_int_exn in
       ET_Map (map_of arity)
  (* FUN *)
  | 117 ->
     let num_free = rint () |> Int32.to_int_exn in
     let fn_pid = decode_term () in
     let m = decode_term () in
     let i = decode_term () in
     let u = decode_term () in
     let fn_free_vars = list_of decode_term num_free in
     (match (m, i, u) with
      | (ET_Atom fn_module, ET_Int fn_index, ET_Int fn_uniq) ->
         ET_Fun { fn_pid;
                  fn_module;
                  fn_index;
                  fn_uniq;
                  fn_free_vars }
      | _ -> parse_err "Parse error in types of Fun" 117)
  (* NEW_FUN *)
  | 112 ->
     let size = rint () |> Int32.to_int_exn in
     let nf_arity = rbyte () in
     let nf_uniq = rstr 16 in
     let nf_index = rint () in
     let nf_num_free = rint () in
     let m = decode_term () in
     let old_index' = decode_term () in
     let old_uniq' = decode_term () in
     let nf_module, nf_old_index, nf_old_uniq =
       match (m, old_index', old_uniq') with
       | (ET_Atom mm, ET_Int oidx, ET_Int ouniq) ->
          (mm, oidx, ouniq)
       | _ -> parse_err "Parse error in new_fun_spec" 112 in
     let byte_count x =
       let x = Int32.to_int_exn x in
       if (x >= 0 && x < 256) then 2 else 5 in
     let size_base = 4 + 1 + 16 + 4 + 4
                     + 3+(String.length nf_module)
                     + byte_count nf_old_index
                     + byte_count nf_old_uniq in
     let rest_len = size - size_base in
     let nf_rest = rstr rest_len in
     ET_NFun {
         nf_arity;
         nf_uniq;
         nf_index;
         nf_num_free;
         nf_module;
         nf_old_index;
         nf_old_uniq;
         nf_rest }
  (* EXPORT *)
  | 113 ->
     let m = decode_term () in
     let f = decode_term () in
     let a = decode_term () in
     (match (m,f,a) with
      | (ET_Atom exp_module,
         ET_Atom exp_function,
         ET_Int exp_args) -> ET_Export { exp_module; exp_function;
                                         exp_args }
      | _ -> parse_err "Export has wrong terms" 113)
  | n -> raise (Parse_error ("unknown term tag", n))


let rec encode wbyte wint wint64 wstr wbuf term =
  let encode_term = encode wbyte wint wint64 wstr wbuf in
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
     let i = Int64.bits_of_float f in
     wbyte 70;
     wint64 i
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
  | ET_Pid { pid_node; pid_id; pid_serial; pid_creation} ->
     wbyte 103;
     encode_term (ET_Atom pid_node);
     wint (Int32.to_int_exn pid_id);
     wint (Int32.to_int_exn pid_serial);
     wbyte pid_creation
  | ET_Ref { ref_node; ref_id; ref_creation } ->
     wbyte 101;
     encode_term (ET_Atom ref_node);
     wint (Int32.to_int_exn ref_id);
     wbyte ref_creation
  | ET_NRef { nref_node; nref_creation; nref_ids } ->
     let id_len = List.length nref_ids in
     let a,b = id_len / 256, id_len mod 256 in
     wbyte 114;
     wbyte a;
     wbyte b;
     encode_term (ET_Atom nref_node);
     wbyte nref_creation;
     List.iter ~f:(fun n -> wint (Int32.to_int_exn n)) nref_ids
  | ET_Export { exp_module; exp_function; exp_args } ->
     wbyte 113;
     encode_term (ET_Atom exp_module);
     encode_term (ET_Atom exp_function);
     encode_term (ET_Int exp_args)
  | ET_Port { port_node; port_id; port_creation } ->
     wbyte 102;
     encode_term (ET_Atom port_node);
     wint (Int32.to_int_exn port_id);
     wbyte port_creation
  | ET_Fun { fn_pid; fn_module; fn_index; fn_uniq; fn_free_vars } ->
     wbyte 117;
     wint (List.length fn_free_vars);
     encode_term fn_pid;
     encode_term (ET_Atom fn_module);
     encode_term (ET_Int fn_index);
     encode_term (ET_Int fn_uniq);
     List.iter ~f:encode_term fn_free_vars
  | ET_NFun { nf_arity;
              nf_uniq;
              nf_index;
              nf_num_free;
              nf_module;
              nf_old_index;
              nf_old_uniq;
              nf_rest } ->
     let byte_count x =
       let x = Int32.to_int_exn x in
       if x >= 0 && x < 256 then 2 else 5 in
     let size = 4 + 1 + 16 + 4 + 4
                + 3+(String.length nf_module)
                + byte_count nf_old_index
                + byte_count nf_old_uniq
                + String.length nf_rest in
     wbyte 112;
     wint size;
     wbyte nf_arity;
     wstr nf_uniq;
     wint (Int32.to_int_exn nf_index);
     wint (Int32.to_int_exn nf_num_free);
     encode_term (ET_Atom nf_module);
     encode_term (ET_Int nf_old_index);
     encode_term (ET_Int nf_old_uniq);
     wstr nf_rest
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
  | ET_Map pairs ->
     wbyte 116;
     wint (List.length pairs);
     List.iter ~f:(fun (k, v) -> encode_term k; encode_term v) pairs


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
    let rint64 () = Int64.(
      let f a e = a + (shift_left (of_int_exn (rbyte ())) e) in
        List.fold_left
          [56; 48; 40; 32; 24; 16; 8; 0]
          ~init:zero
          ~f:f) in
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
    | 131 -> decode rbyte rint rint64 rstr rbuf ()
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
    let wint64 x =
      let n255 = Int64.of_int_exn 255 in
      List.iter
        ~f:(fun n ->
          wbyte Int64.(to_int_exn (bit_and (shift_right_logical x n)
                                           n255)))
        [56; 48; 40; 32; 24; 16; 8; 0] in
    let wstr = Buffer.add_string buf in
    let wbuf = Buffer.add_buffer buf in
      wbyte 131;
      encode wbyte wint wint64 wstr wbuf term
end
                  
let of_buffer = Buffer.read
let to_buffer = Buffer.write
