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

open Cmdliner
open Lwt.Syntax
open Lwt.Infix
open Import
open Irmin_server

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Cmdliner.Term.(
    const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let with_timer f =
  let t0 = Sys.time () in
  let+ a = f () in
  let t1 = Sys.time () -. t0 in
  (t1, a)

let init ~uri ~branch ~tls (module Client : Irmin_client.S) : client Lwt.t =
  let* x = Client.Repo.v (Irmin_client.config ~tls uri) in
  let+ x =
    match branch with
    | Some b ->
        Client.of_branch x
          (Irmin.Type.of_string Client.Branch.t b |> Result.get_ok)
    | None -> Client.main x
  in
  S ((module Client : Irmin_client.S with type t = Client.t), x)

let run f time iterations =
  let rec eval iterations =
    if iterations = 0 then Lwt.return_unit
    else
      let* () = f () in
      eval (iterations - 1)
  in
  let x =
    if time then (
      let+ n, x = with_timer (fun () -> eval iterations) in
      Logs.app (fun l -> l "Time: %fs" (n /. float_of_int iterations));
      x)
    else f ()
  in
  Lwt_main.run x

let list_server_commands () =
  let module Store = Irmin_mem.KV.Make (Irmin.Contents.String) in
  let module Cmd = Command.Make (Irmin_client_unix.IO) (Conn.Codec.Bin) (Store)
  in
  let str t =
    Fmt.to_to_string Irmin.Type.pp_ty t
    |> String.split_on_char '\n'
    |> String.concat "\n\t\t"
  in
  List.iter
    (fun (name, (module C : Cmd.CMD)) ->
      Printf.printf "%s:\n\tInput: %s\n\tOutput: %s\n" name (str C.req_t)
        (str C.res_t))
    Cmd.commands

let ping client =
  run (fun () ->
      client >>= fun (S ((module Client), client)) ->
      let repo = Client.repo client in
      let+ result = Client.ping repo in
      let () = Error.unwrap "ping" result in
      Logs.app (fun l -> l "OK"))

let find client path =
  run (fun () ->
      client >>= fun (S ((module Client), client)) ->
      let path =
        Irmin.Type.of_string Client.Path.t path |> Error.unwrap "path"
      in
      let* result = Client.find client path in
      match result with
      | Some data ->
          let* () =
            Lwt_io.printl (Irmin.Type.to_string Client.Contents.t data)
          in
          Lwt_io.flush Lwt_io.stdout
      | None ->
          Logs.err (fun l ->
              l "Not found: %a" (Irmin.Type.pp Client.Path.t) path);
          Lwt.return_unit)

let mem client path =
  run (fun () ->
      client >>= fun (S ((module Client), client)) ->
      let path =
        Irmin.Type.of_string Client.Path.t path |> Error.unwrap "path"
      in
      let* result = Client.mem client path in
      Lwt_io.printl (if result then "true" else "false"))

let mem_tree client path =
  run (fun () ->
      client >>= fun (S ((module Client), client)) ->
      let path =
        Irmin.Type.of_string Client.Path.t path |> Error.unwrap "path"
      in
      let* result = Client.mem_tree client path in
      Lwt_io.printl (if result then "true" else "false"))

let set client path author message contents =
  run (fun () ->
      client >>= fun (S ((module Client), client)) ->
      let module Info = Irmin_client_unix.Info (Client.Info) in
      let path =
        Irmin.Type.of_string Client.Path.t path |> Error.unwrap "path"
      in
      let contents =
        Irmin.Type.of_string Client.Contents.t contents
        |> Error.unwrap "contents"
      in
      let info = Info.v ~author "%s" message in
      let+ () = Client.set_exn client path ~info contents in
      Logs.app (fun l -> l "OK"))

let remove client path author message =
  run (fun () ->
      client >>= fun (S ((module Client), client)) ->
      let module Info = Irmin_client_unix.Info (Client.Info) in
      let path =
        Irmin.Type.of_string Client.Path.t path |> Error.unwrap "path"
      in
      let info = Info.v ~author "%s" message in
      let+ () = Client.remove_exn client path ~info in
      Logs.app (fun l -> l "OK"))

let export client filename =
  run (fun () ->
      client >>= fun (S ((module Client), client)) ->
      let* slice = Client.export (Client.repo client) in
      let s = Irmin.Type.(unstage (to_bin_string Client.slice_t) slice) in
      Lwt_io.chars_to_file filename (Lwt_stream.of_string s))

let import client filename =
  run (fun () ->
      client >>= fun (S ((module Client), client)) ->
      let* slice = Lwt_io.chars_of_file filename |> Lwt_stream.to_string in
      let slice =
        Irmin.Type.(unstage (of_bin_string Client.slice_t) slice)
        |> Error.unwrap "slice"
      in
      let+ () = Client.import (Client.repo client) slice in
      Logs.app (fun l -> l "OK"))

let replicate client author message prefix =
  Lwt_main.run
    ( client >>= fun (S ((module Client), client)) ->
      let module Info = Irmin_client_unix.Info (Client.Info) in
      let diff input =
        Irmin.Type.(
          of_json_string
            (list
               (pair Client.Path.t
                  (Irmin.Diff.t (pair Client.Contents.t Client.Metadata.t)))))
          input
        |> Result.get_ok
      in
      let rec loop () =
        let* input = Lwt_io.read_line Lwt_io.stdin in
        let batch : Client.Batch.t =
          List.fold_left
            (fun acc (k, diff) ->
              match diff with
              | `Updated (_, (value, metadata)) | `Added (value, metadata) ->
                  Client.Batch.add_value ~metadata k value acc
              | `Removed _ -> Client.Batch.remove k acc)
            (Client.Batch.v ()) (diff input)
        in
        let info = Info.v ~author "%s" message in
        let prefix =
          match prefix with
          | Some p -> Irmin.Type.of_string Client.Path.t p |> Result.get_ok
          | None -> Client.Path.empty
        in
        let* _ = Client.Batch.apply ~info client ~path:prefix batch in
        loop ()
      in
      loop () )

let watch client =
  Lwt_main.run
    ( client >>= fun (S ((module Client), client)) ->
      let repo = Client.repo client in
      let pp = Irmin.Type.pp (Client.Commit.t repo) in
      let* _w =
        Client.watch client (fun x ->
            match x with
            | `Updated (a, b) ->
                Logs.app (fun l -> l "Updated (%a, %a)" pp a pp b);
                Lwt.return_unit
            | `Added a ->
                Logs.app (fun l -> l "Added %a" pp a);
                Lwt.return_unit
            | `Removed a ->
                Logs.app (fun l -> l "Removed %a" pp a);
                Lwt.return_unit)
      in
      let x, _ = Lwt.wait () in
      x )

let pr_str = Format.pp_print_string

let path index =
  let doc = Arg.info ~docv:"PATH" ~doc:"Path to lookup or modify" [] in
  Arg.(required & pos index (some string) None & doc)

let prefix =
  let doc = Arg.info ~docv:"PATH" ~doc:"Optional prefix" [] in
  Arg.(value & pos 0 (some string) None & doc)

let filename index =
  let doc = Arg.info ~docv:"PATH" ~doc:"Filename" [] in
  Arg.(required & pos index (some string) None & doc)

let author =
  let doc = Arg.info ~docv:"NAME" ~doc:"Commit author name" [ "author" ] in
  Arg.(value & opt string "irmin-client" & doc)

let message =
  let doc = Arg.info ~docv:"MESSAGE" ~doc:"Commit message" [ "message" ] in
  Arg.(value & opt string "" & doc)

let branch =
  let doc = Arg.info ~docv:"BRANCH" ~doc:"Branch name" [ "branch" ] in
  Arg.(value & opt (some string) None & doc)

let value index =
  let doc = Arg.info ~docv:"DATA" ~doc:"Value" [] in
  Arg.(required & pos index (some string) None & doc)

let tls =
  let doc = Arg.info ~doc:"Enable TLS" [ "tls" ] in
  Arg.(value @@ flag doc)

let time =
  let doc = Arg.info ~doc:"Enable timing" [ "time" ] in
  Arg.(value @@ flag doc)

let iterations =
  let doc =
    Arg.info ~doc:"Iterations when timing is enabled" [ "i"; "iterations" ]
  in
  Arg.(value @@ opt int 1 doc)

let freq =
  let doc = Arg.info ~doc:"Update frequency" [ "f"; "freq" ] in
  Arg.(value @@ opt float 5. doc)

let config =
  let create uri (branch : string option) tls (store, hash, contents) codec
      config_path () =
    let codec =
      match codec with
      | `Bin -> (module Conn.Codec.Bin : Conn.Codec.S)
      | `Json -> (module Conn.Codec.Json)
    in
    let (module Codec) = codec in
    let store, config =
      Irmin_cli.Resolver.load_config ?config_path ?store ?hash ?contents ()
    in
    let config = Irmin_server.Cli.Conf.v config uri in
    let (module Store : Irmin.Generic_key.S) =
      Irmin_cli.Resolver.Store.generic_keyed store
    in
    let module Client = Irmin_client_unix.Make_codec (Codec) (Store) in
    let uri = Irmin.Backend.Conf.(get config Irmin_server.Cli.Conf.Key.uri) in
    init ~uri ~branch ~tls (module Client)
  in
  Term.(
    const create
    $ Cli.uri
    $ branch
    $ tls
    $ Irmin_cli.Resolver.Store.term ()
    $ Cli.codec
    $ Cli.config_path
    $ setup_log)

let help =
  let help () =
    Printf.printf "See output of `%s --help` for usage\n" Sys.argv.(0)
  in
  Term.(const help $ const ())

let () =
  Stdlib.exit
  @@ Cmd.eval
  @@ Cmd.group ~default:help (Cmd.info "irmin-client")
       [
         Cmd.v
           (Cmd.info ~doc:"List all commands available on server"
              "list-commands")
           Term.(const list_server_commands $ const ());
         Cmd.v
           (Cmd.info ~doc:"Ping the server" "ping")
           Term.(const ping $ config $ time $ iterations);
         Cmd.v
           (Cmd.info ~doc:"Get the path associated with a value" "get")
           Term.(const find $ config $ path 0 $ time $ iterations);
         Cmd.v
           (Cmd.info ~doc:"Alias for 'get' command" "find")
           Term.(const find $ config $ path 0 $ time $ iterations);
         Cmd.v
           (Cmd.info ~doc:"Set path/value" "set")
           Term.(
             const set
             $ config
             $ path 0
             $ author
             $ message
             $ value 1
             $ time
             $ iterations);
         Cmd.v
           (Cmd.info ~doc:"Remove value associated with the given path" "remove")
           Term.(
             const remove
             $ config
             $ path 0
             $ author
             $ message
             $ time
             $ iterations);
         Cmd.v
           (Cmd.info ~doc:"Import from dump file" "import")
           Term.(const import $ config $ filename 0 $ time $ iterations);
         Cmd.v
           (Cmd.info ~doc:"Export to dump file" "export")
           Term.(const export $ config $ filename 0 $ time $ iterations);
         Cmd.v
           (Cmd.info ~doc:"Check if path is set" "mem")
           Term.(const mem $ config $ path 0 $ time $ iterations);
         Cmd.v
           (Cmd.info ~doc:"Check if path is set to a tree value" "mem_tree")
           Term.(const mem_tree $ config $ path 0 $ time $ iterations);
         Cmd.v
           (Cmd.info ~doc:"Watch for updates" "watch")
           Term.(const watch $ config);
         Cmd.v
           (Cmd.info ~doc:"Replicate changes from irmin CLI" "replicate")
           Term.(const replicate $ config $ author $ message $ prefix);
       ]
