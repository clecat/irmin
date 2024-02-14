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
    mutable cap : cap;
    mutable w : int;
  }

  and cap = Uncapped | Capped of int

  let weight t = t.w

  let create cap =
    let cap, ht_cap =
      if cap < 0 then (Uncapped, 65536) else (Capped cap, cap)
    in
    { cap; w = 0; ht = HT.create ht_cap; q = Q.create () }

  let eject t =
    match Q.eject t.q with None -> () | Some (k, _) -> 
      t.w <- t.w - 1;
      HT.remove t.ht k
  
  let drop t =
    match t.q.head with
    | None -> None
    | Some ({ Q.value = k, v; _ } as n) ->
        t.w <- t.w - 1;
        HT.remove t.ht k;
        Q.detach t.q n;
        (match t.q.hand with
        | None -> ()
        | Some n -> if n.next = None then n.next <- t.q.head);
        Some v

  let add t k v =
    let add t k v =
      let n = Q.node (k, v) in
      t.w <- t.w + 1;
      HT.add t.ht k n;
      Q.append t.q n
    in
    match t.cap with
    | Capped c when c = 0 -> ()
    | Uncapped -> add t k v
    | Capped c ->
        if weight t > c then
          eject t;
        add t k v

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
