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

(** Slices of basic vector types, i.e. arrays, byte sequences and string. *)

(** {6 Type} *)

(** The type of a vector slice. *)
open Base

type 'a t = private {
    vector: 'a;     (** The type of the underlying vector. *)
    start: int;     (** Index in [vector] of element at start of slice. *)
    limit: int;     (** Index in [vector] of element after end of slice. *)
}
[@@deriving sexp]


(** {6 Constructors} *)

(** Use [of_vector f v] to make a slice of [v] referencing the entire range of
    [v] starting at index zero and limiting at the index obtained by applying
    [f v]. Raises [Failure] if [f] returns a negative integer.
*)
val of_vector: ('a -> int) -> 'a -> 'a t

(** Use [of_subvector f v start limit] to make a slice that references the part
    of [v] starting at index [start] and limiting at index [limit]. Raises
    [Invalid_argument] if [start > limit] or if either [start] or [limit] are
    not valid indexes in [v], where valid indexes are non-negative integers
    less than the result of [f v]. Raises [Failure] if [f] returns a negative
    integer.
*)
val of_subvector: ('a -> int) -> int -> int ->'a ->  'a t

(** Use [of_array a] to make a slice that references all of [a]. *)
val of_array: 'a array -> 'a array t

(** Use [of_subarray a start limit] to make a slice that references the part of
    [a] that starts at index [start] and is limited at index [limit].
*)
val of_subarray: int -> int -> 'a array -> 'a array t

(** Use [of_bytes b] to make a slice that references all of [b]. *)
val of_bytes: bytes -> bytes t

(** Use [of_subbytes b start limit] to make a slice that references the part of
    [b] that starts at index [start] and is limited at index [limit].
*)
val of_subbytes:int -> int ->  bytes -> bytes t

(** Use [of_string s] to make a slice that references all of [s]. *)
val of_string: string -> string t

(** Use [of_substring s start limit] to make a slice that references the part
    of [s] that starts at index [start] and is limited at index [limit].
*)
val of_substring:int -> int -> string ->  string t

(** {6 Conversions} *)

(** Use [to_vector f s] to copy the values in [s] into a new vector by
    evaluating [f start length] where [start] is the starting index of the
    slice and [length] is the difference between the limit and the start.
*)
val to_vector: ('a -> pos:int -> len:int -> 'a) -> 'a t -> 'a

(** Use [to_string s] to copy the octets in [s] into a new string. *)
val to_string: string t -> string

(** Use [to_array s] to copy the elements in [s] into a new array. *)
val to_array: 'a array t -> 'a array

(** Use [to_chars s] to make a sequence of the characters in [s]. *)
val to_chars: string t -> char Stdlib.Seq.t

(** Use [to_elements s] to make a sequence of the elements in [s]. Elements
    are read from the array as the sequence is evaluated.
*)
val to_elements: 'a array t -> 'a Stdlib.Seq.t

(** {6 Interface} *)

(** Use [length s] to obtain the length of [s] in elements. *)
val length: 'a t -> int

(** Use [truncate n s] to make a new slice that references the first [n]
    elements in [s]. Raises [Invalid_argument] if [n] is not a valid index
    into [s].
*)
val truncate: int -> 'a t -> 'a t

(** Use [shift n s] to make a new slice that references the remaining elements
    in [s] after the first [n]. Raises [Invalid_argument] if [n] is not a valid
    index into [s].
*)
val shift: int -> 'a t -> 'a t

(** Use [split n s] to make two slices that reference the same mutable or
    immutable sequences as [s]. The first slice comprises the first [n]
    elements of [s], and the second slice comprises the remaining elements of
    [s]. Raises [Invalid_argument] if [n] is not a valid index into [s].
*)
val split: int -> 'a t -> 'a t * 'a t


(*--- End ---*)

