let set32_ocaml s pos (i:int32) =
  let (>:) = Int32.shift_right_logical in
  let (&:) = Int32.logand in
  let mask = Int32.of_int 0xff in
  let to_char v = Char.chr (Int32.to_int v) in
  let too_far = pos + 4 in
  let rec loop p i =
    if p = too_far
    then ()
    else
      let vp = i &: mask in
      let cp = to_char vp in
      let () = s.[p] <- cp in
      loop (p+1) (i >: 8)
  in
  loop pos i


let get32_ocaml s pos =
  let (<:) = Int32.shift_left in
  let (|:) = Int32.logor in
  let to_i32 c = Int32.of_int (Char.code c) in
  let rec loop acc p =
    if p < pos
    then acc
    else
      let cp = s.[p] in
      let vp = to_i32 cp in
      let acc' = (acc <: 8) |: vp in
      loop acc' (p-1)
  in
  loop 0l (pos + 3)

external get32_prim : string -> int -> int32         = "%caml_string_get32"
external set32_prim : string -> int -> int32 -> unit = "%caml_string_set32"



let verify_set n =
  let s0 = String.create 4 in
  let s1 = String.create 4 in
  let s2 = String.create 4 in
  let limit = (-n) in
  let rec loop i =
    if i < limit then ()
    else
      let i32 = Int32.of_int i in
      let () = set32_ocaml s0 0 i32 in
      let () = set32_prim  s1 0 i32 in
      let () = Zooph.set32 s2 0 i32 in
      assert (s0 = s1);
      assert (s1 = s2);
      loop (i-1)
  in
  loop n

let verify_get n =
  let s0 = String.create 4 in
  let limit = (-n) in
  let check i j =
    if i <> j
    then
      let msg = Printf.sprintf "%li <> %li %S" i j s0 in
      failwith msg
  in
  let rec loop i =
    if i < limit then ()
    else
      let () = if i mod 1000_000 = 0 then Printf.printf "%i\n" i in
      let i32= Int32.of_int i in
      let () = set32_prim  s0 0 i32 in
      let j0 = get32_ocaml s0 0 in
      let j1 = get32_prim  s0 0 in
      let j2 = Zooph.get32 s0 0 in
      check i32 j0;
      check i32 j1;
      check i32 j2;
      loop (i-1)
  in
  loop n

module OCaml = struct
    let get32 = get32_ocaml
    let set32 = set32_ocaml
end
module Prim = struct
    let get32 = get32_prim
    let set32 = set32_prim
end
module Thrift = struct
  let get_byte32 i b = 255 land (Int32.to_int (Int32.shift_right i (8*b)))
  let set32 s pos i =
    let gb = get_byte32 i in
    for i=0 to 3 do
      s.[pos + 3-i] <- char_of_int (gb i)
    done

  let get32 b pos =
    let n = 4 in
    let s = ref 0l in
    let sb = 32 - 8*n in
    for i=pos to pos + 3 do
      s:= Int32.logor !s (Int32.shift_left
                            (Int32.of_int (int_of_char b.[i]))
                            (8*(n-1-i)))
    done;
    Int32.shift_right (Int32.shift_left !s sb) sb

end
(* open OCaml *)
(* open Zooph *)
open Prim
(* open Thrift*)

let benchmark n =
  let t0 = Unix.gettimeofday() in
  let s = String.create 4 in
  let limit = Int32.of_int n in
  let rec loop i32 =
    if i32 = limit
    then ()
    else
      let () = set32 s 0 i32 in
      let j32 = get32 s 0 in
      assert (i32 = j32);
      loop (Int32.succ i32)
  in
  let () = loop 0l in
  let t1 = Unix.gettimeofday () in
  let d = t1 -. t0 in
  let speed = float n /. d in
  let megaspeed = speed /. 1000000.0 in
  Printf.printf "%i took %f => %fe6/s\n" n d megaspeed


let () =
  let n = 1000_000_000 in
  verify_set n;
  verify_get n;
  benchmark n
