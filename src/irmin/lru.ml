(*
   Copyright (c) 2016 David Kaloper Meršinjak
   Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

(* Extracted from https://github.com/pqwy/lru *)

module Make (H : Hashtbl.HashedType) = struct
  module HT = Hashtbl.Make (H)

  module Q = struct
    type 'a node = {
      value : 'a;
      mutable visited : bool;
      mutable next : 'a node option;
      mutable prev : 'a node option;
    }

    type 'a t = {
      mutable head : 'a node option;
      mutable tail : 'a node option;
      mutable hand : 'a node option;
    }


    let detach t n =
      let np = n.prev and nn = n.next in
      (match np with
      | None -> t.tail <- nn
      | Some x ->
          x.next <- nn;
          n.prev <- None);
      match nn with
      | None -> t.head <- np
      | Some x ->
          x.prev <- np;
          n.next <- None

    let append t n =
      let on = Some n in
      match t.head with
      | Some x as l ->
          x.next <- on;
          t.head <- on;
          n.prev <- l
      | None ->
          t.tail <- on;
          t.head <- on

    let rec eject t on =
      match (on, t.tail) with
      | None, None -> None
      | Some n, _ | None, Some n -> (
          match n.visited with
          | true ->
              n.visited <- false;
              eject t n.next
          | false ->
              t.hand <- n.next;
              detach t n;
              Some n.value)

    let eject t = eject t t.hand
    let node x = { value = x; prev = None; next = None; visited = false }
    let create () = { head = None; tail = None; hand = None }

    let iter t f =
      let rec aux f = function
        | Some n ->
            let next = n.next in
            f n.value;
            aux f next
        | _ -> ()
      in
      aux f t.head

    let clear t =
      t.head <- None;
      t.tail <- None;
      t.head <- None
  end

  type key = HT.key

  type 'a t = {
    ht : (key * 'a) Q.node HT.t;
    q : (key * 'a) Q.t;
    mutable cap : int;
    mutable w : int;
    weight : 'a -> int;
  }

  let create ?(weight = function _ -> 1) cap =
    { cap; w = 0; ht = HT.create cap; q = Q.create (); weight }

  let eject t =
    match Q.eject t.q with None -> () | Some (k, v) ->
      t.w <- t.w - t.weight v;
      HT.remove t.ht k

  let remove t k =
    try
      let n = HT.find t.ht k in
      t.w <- t.w - t.weight (snd n.value);
      HT.remove t.ht k;
      Q.detach t.q n
    with Not_found -> ()

  let add t k v =
    if t.cap = 0 then ()
    else (
      remove t k;
      let n = Q.node (k, v) in
      let w = t.weight v in
      if w > t.cap then
        (* if [v] is bigger than the LRU capacity, just skip it *) ()
      else (
        t.w <- t.w + w;
        while t.w > t.cap do
          eject t
        done;
        HT.add t.ht k n;
        Q.append t.q n))

  let find t k =
    let v = HT.find t.ht k in
    v.visited <- true;
    snd v.value
  
  let mem t k =
    try
      let v = HT.find t.ht k in
      v.visited <- true;
      true
    with Not_found -> false

  let iter t f = Q.iter t.q (fun (k, v) -> f k v)

  let clear t =
    t.w <- 0;
    HT.clear t.ht;
    Q.clear t.q
end
