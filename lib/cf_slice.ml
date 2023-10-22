(*---------------------------------------------------------------------------*
  Copyright (C) 2017-2019, james woodyatt
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
  OF THE POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open Base
type 'a t = {
    vector: 'a;
    start: int;
    limit: int;
}
[@@deriving sexp]

let m_invalid_arg fn arg =
    let open Printf in
    invalid_arg (sprintf "%s.%s: boundary exceeded by %s" Stdlib.__MODULE__ fn arg)

module Unsafe = struct
    let of_subvector vector start limit = { vector; start; limit }
    let of_vector length vector = of_subvector vector 0 (length vector)

    let invariant length v =
        if v.start < 0 || v.start > v.limit then
            m_invalid_arg "of_vector/of_subvector" "start";
        let length = length v.vector in
        if length < 0 || v.limit > length then
            m_invalid_arg "of_vector/of_subvector" "limit";
        true

    let index arg i v =
        if i < 0 || i > v.limit - v.start then
            m_invalid_arg arg "index";
        true

    let truncate n v = { v with limit = v.start + n }
    let shift n v = { v with start = v.start + n  }
    let split n v = truncate n v, shift n v
end

let of_subvector length start limit v =
    let s = Unsafe.of_subvector v start limit in
    let _ = Unsafe.invariant length s in
    s

let of_vector length v =
    let s = Unsafe.of_vector length v in
    let _ = Unsafe.invariant length s in
    s

let of_array v = Unsafe.of_vector Array.length v
let of_subarray start limit  = of_subvector Array.length  start limit

let of_bytes = Unsafe.of_vector Bytes.length
let of_subbytes = of_subvector Bytes.length

let of_string = Unsafe.of_vector String.length
let of_substring = of_subvector String.length

let to_vector f s = f s.vector ~pos:s.start ~len:(s.limit - s.start)
let to_string s = to_vector String.unsafe_sub s 
let to_array s = to_vector Array.sub  s 

let rec to_seq_aux (get, limit as w) i () =
    if i < limit then Stdlib.Seq.Cons (get i, to_seq_aux w (Int.succ i)) else Stdlib.Seq.Nil

let to_chars s () =
    to_seq_aux (String.unsafe_get s.vector, s.limit) s.start ()

let to_elements s () =
    to_seq_aux (Array.unsafe_get s.vector, s.limit) s.start ()

let length v = v.limit - v.start

let truncate n v =
    let _ = Unsafe.index "truncate" n v in
    Unsafe.truncate n v

let shift n v =
    let _ = Unsafe.index "shift" n v in
    Unsafe.shift n v

let split n v =
    let _ = Unsafe.index "split" n v in
    Unsafe.split n v

(*--- End ---*)

