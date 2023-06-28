(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
include Dict_intf

module Make (Fm : File_manager.S) = struct
  module Fm = Fm

  type t = {
    cache : (string, int) Hashtbl.t;
    index : (int, string) Hashtbl.t;
    find_info : (int, int) Hashtbl.t;
    index_info : (string, int) Hashtbl.t;
    fm : Fm.t;
    mutable last_refill_offset : int63;
  }

  module File = struct
    let append_exn t = Fm.Dict.append_exn (Fm.dict t.fm)
    let offset t = Fm.Dict.end_poff (Fm.dict t.fm)
    let read_to_string t = Fm.Dict.read_to_string (Fm.dict t.fm)
  end

  type nonrec int32 = int32 [@@deriving irmin ~to_bin_string ~decode_bin]

  let append_string t v =
    let len = Int32.of_int (String.length v) in
    let buf = int32_to_bin_string len ^ v in
    File.append_exn t buf

  (* Refill is only called once for a RW instance *)
  let refill t =
    let open Result_syntax in
    let from = t.last_refill_offset in
    let len = Int63.to_int Int63.Syntax.(File.offset t - from) in
    t.last_refill_offset <- File.offset t;
    let+ raw = File.read_to_string t ~off:from ~len in
    let pos_ref = ref 0 in
    let rec aux n =
      if !pos_ref >= len then ()
      else
        let v = decode_bin_int32 raw pos_ref in
        let len = Int32.to_int v in
        let v = String.sub raw !pos_ref len in
        pos_ref := !pos_ref + len;
        Hashtbl.add t.cache v n;
        Hashtbl.add t.index n v;
        (aux [@tailcall]) (n + 1)
    in
    (aux [@tailcall]) (Hashtbl.length t.cache)

  let index t v =
    [%log.debug "[dict] index %S" v];
    let n = try Hashtbl.find t.index_info v with Not_found -> 0 in
    Hashtbl.replace t.index_info v (n + 1);
    try Some (Hashtbl.find t.cache v)
    with Not_found ->
      let id = Hashtbl.length t.cache in
      append_string t v;
      Hashtbl.add t.cache v id;
      Hashtbl.add t.index id v;
      Some id

  let find t id =
    [%log.debug "[dict] find %d" id];
    let n = try Hashtbl.find t.find_info id with Not_found -> 0 in
    Hashtbl.replace t.find_info id (n + 1);
    let v = try Some (Hashtbl.find t.index id) with Not_found -> None in
    v
  let v fm =
    let open Result_syntax in
    let cache = Hashtbl.create 997 in
    let index = Hashtbl.create 997 in
    let find_info = Hashtbl.create 997 in
    let index_info = Hashtbl.create 997 in
    let last_refill_offset = Int63.zero in
    let t =
      { index; cache; find_info; index_info; fm; last_refill_offset }
    in
    let* () = refill t in
    Fm.register_dict_consumer fm ~after_reload:(fun () -> refill t);
    Ok t

  let close t =
    let find_fd = 
      Unix.out_channel_of_descr @@ Unix.openfile "/tmp/irmin_find_debug_info" Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
    in
    let find_condensed = Hashtbl.create 10 in
    Hashtbl.iter
      (fun _ b ->
        let n = try Hashtbl.find find_condensed b with Not_found -> 0 in
        Hashtbl.replace find_condensed b (n + 1);
      ) t.find_info;
    let fmt = Format.formatter_of_out_channel find_fd in
    let x = Hashtbl.length t.index - Hashtbl.length t.find_info in
    Fmt.pf fmt "%d %d\n" 0 x;
    let l = ref [] in
    Hashtbl.iter
      (fun a b ->
        l := (a, b) :: !l;
      ) find_condensed;
    let l = List.sort (fun (x, _) (y, _) -> Int.compare x y) !l in
    List.iter (fun (a, b) -> Fmt.pf fmt "%d %d\n" a b) l;

    let index_fd = 
      Unix.out_channel_of_descr @@ Unix.openfile "/tmp/irmin_index_debug_info" Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
    in
    let index_condensed = Hashtbl.create 10 in
    Hashtbl.iter
      (fun _ b ->
        let n = try Hashtbl.find index_condensed b with Not_found -> 0 in
        Hashtbl.replace index_condensed b (n + 1);
      ) t.index_info;
    let fmt = Format.formatter_of_out_channel index_fd in
    let x = Hashtbl.length t.index - Hashtbl.length t.index_info in
    Fmt.pf fmt "%d %d\n" 0 x;
    let l = ref [] in
    Hashtbl.iter
      (fun a b ->
        l := (a, b) :: !l;
      ) index_condensed;
    let l = List.sort (fun (x, _) (y, _) -> Int.compare x y) !l in
    List.iter (fun (a, b) -> Fmt.pf fmt "%d %d\n" a b) l;
    ()
end
