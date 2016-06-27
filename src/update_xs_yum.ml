
(*
 * Copyright (C) 2015 Citrix Systems Inc
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

open Lwt
open Iso
open Astring
open Cohttp
open Cohttp_lwt_unix

module Iso = Isofs.Make(Block)(Io_page)

exception Error
let (>>*=) m f = match m with
  | `Error e -> fail Error
  | `Ok x -> f x

let ok = `Ok ()

let (>>|=) m f = m >>= fun x -> x >>*= f

let mkdir_safe dir perm =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir dir perm)
    (function | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return ()
              | e -> fail e)

(** create a directory, and create parent if doesn't exist *)
let mkdir_p dir perm =
  Printf.printf "mkdir_p %s %d\n" dir perm;
  let rec p_mkdir dir =
    let p_name = Filename.dirname dir in
    (if p_name <> "/" && p_name <> "." 
     then p_mkdir p_name
     else Lwt.return ()) >>= fun () ->
    mkdir_safe dir perm in
 p_mkdir dir >>= fun () -> Lwt.return ok

(* Dump a list of cstructs into a file f *)
let cstrs_to_file cstrs f =
  Lwt_unix.openfile f [Lwt_unix.O_RDWR; Lwt_unix.O_CREAT] 0o644
  >>= fun fd ->
  Lwt_list.iter_s (
    fun cstr -> Lwt_cstruct.write fd cstr
      >>= fun _ -> Lwt.return ())
    cstrs
  >>= fun () ->
  Lwt_unix.close fd

let default_filtermap = function
  | `D name -> Some name
  | `F name -> Some name

(* Sync an iso filesystem to a local filesystem. Optionally maps dir names and filenames *)
let sync ?(filtermap=default_filtermap) iso entries lrelpath irelpath =
  mkdir_p lrelpath 0o755
  >>|= fun () -> 
  let rec inner entries lrelpath irelpath = 
    Lwt_list.fold_left_s
      (fun acc f ->
         match f with
         | (name, Isofs.Directory d) ->
           acc >>*= fun () ->
           (match filtermap (`D name) with
            | Some name' ->
              mkdir_p (Filename.concat lrelpath name') 0o755
              >>|= fun () ->
              inner
                d.Isofs.d_contents
                (Filename.concat lrelpath name')
                (Filename.concat irelpath name)
            | None -> 
              Lwt.return ok)
         | (name, Isofs.File f) ->
           acc >>*= fun () ->
           (match filtermap (`F name) with
            | Some name' ->
              let ipath = Filename.concat irelpath name in
              Iso.KV_RO.size iso ipath
              >>|= fun size ->
              Iso.KV_RO.read iso ipath 0 (Int64.to_int size)
              >>|= fun result ->
              let filename = Filename.concat lrelpath name' in
              Printf.printf "%s\n" filename;
              cstrs_to_file result filename >>= fun () ->
              Lwt.return ok
            | None ->
              Lwt.return ok)
      ) ok entries
  in inner entries lrelpath irelpath

let create_tree root binpkg_iso sources_iso =
  let rpmsdir = Filename.concat root "domain0/RPMS" in
  let srpmsdir = Filename.concat root "domain0/SRPMS" in
  Block.connect binpkg_iso
  >>|= fun b ->
  Iso.connect b
  >>|= fun iso ->
  (* Sync binpkg.iso to RPMS dir locally, skipping the 'repodata'
     directory *)
  sync ~filtermap:(
    function
    | `D "repodata" -> None
    | `D x -> Some x
    | `F x -> Some x)
    iso iso.Iso.entries rpmsdir "/"
  >>|= fun () ->
  Block.connect sources_iso
  >>|= fun b ->
  Iso.connect b
  >>|= fun iso ->
  (* Sync sources.iso to SRPMS dir locally, flattening the dir
     hierarchy and skipping any file that doesn't look like an SRPM *)
  sync ~filtermap:(
    function
    | `D x -> Some "/"
    | `F x ->
      if String.is_suffix ".src.rpm" x
      then Some x
      else None)
    iso iso.Iso.entries srpmsdir "/"
  >>|= fun () ->
  Lwt.return ok

let check th =
  th >>= function
  | Lwt_unix.WEXITED x when x=0 -> Lwt.return ok
  | _ -> Lwt.return (`Error `Bad)

let get uri filename =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let so_far = ref 0 in
  let last_percentage = ref (-1) in
  Client.call `GET (Uri.of_string uri)
  >>= fun (res, body) ->
  let status = Response.status res in
  let res_sexp = Response.sexp_of_t res in
  Printf.printf "Response: %s\n%!" (Sexplib.Sexp.to_string res_sexp);
  let headers = Response.headers res in
  let len =
    match Header.get headers "content-length" with
    | Some len ->
      Printf.printf "Got content-length: %s\n%!" len;
      Some (int_of_string len)
    | None ->
      Printf.printf "No content-length\n%!";
      None
  in
  match Code.is_success (Code.code_of_status status) with
  | false ->
    Printf.fprintf stderr "Got bad return code fetching \"%s\": %s"
      uri (Code.string_of_status status);
    exit 1
  | true ->
    Lwt_io.with_file ~mode:Lwt_io.output filename
      (fun chan -> Lwt_stream.iter_s
          (fun s ->
             so_far := !so_far + (String.length s);
             (match len with
             | Some i ->
               let percent = (100 * !so_far) / i in
               if !last_percentage <> percent then begin
                 if percent mod 10 = 0 then
                   Printf.printf "%d%%%!" percent
                 else
                   Printf.printf ".%!";
                 last_percentage := percent
               end
             | None -> ());
             Lwt_io.write chan s)
          (Cohttp_lwt_body.to_stream body)) >>= fun () ->
    match len with
    | Some i ->
      if !so_far != i
      then begin
        Printf.fprintf stderr "Receieved size <> content-length: (%d <> %d)\n%!" (!so_far) i;
        exit 1
      end;
      Lwt.return ok
    | None ->
      Lwt.return ok
        

let with_downloaded_isos uri_base f =
  let binpkg_uri = Printf.sprintf "%s/binpkg.iso" uri_base in
  let binpkg_fname = Filename.temp_file "binpkg" "iso" in
  let sources_uri = Printf.sprintf "%s/source.iso" uri_base in
  let sources_fname = Filename.temp_file "source" "iso" in

  Printf.printf "Downloading binpkg.iso\n%!";
  get binpkg_uri binpkg_fname
  >>|= fun () ->
  Printf.printf "Downloading source.iso\n%!";
  get sources_uri sources_fname
  >>|= fun () ->
  f (binpkg_fname, sources_fname)
  >>= fun x ->
  Lwt_unix.unlink binpkg_fname
  >>= fun () -> 
  Lwt_unix.unlink sources_fname
  >>= fun () ->
  Lwt.return x
  
let run root branch s3bin =
  with_downloaded_isos branch 
    (fun (binpkg_iso, sources_iso) ->
      Printf.printf "Running createtree\n%!";
      create_tree root binpkg_iso sources_iso
      >>|= fun () ->
      Printf.printf "Running createrepo\n%!";
      check (Lwt_unix.system (Printf.sprintf "createrepo %s/domain0" root))
      >>|= fun _ ->
      Printf.printf "Running s3cmd sync\n%!";
      check (Lwt_unix.system (Printf.sprintf "s3cmd sync --delete-removed %s %s" root s3bin))
      >>|= fun _ -> Lwt.return ok)

let find_latest () =
  Client.get (Uri.of_string "http://downloadns.citrix.com.edgesuite.net/8170/listing.html")
  >>= fun (res,body) ->
  let ok = 
    if not (res |> Response.status |> Code.code_of_status |> Code.is_success)
    then
      (
        Printf.fprintf stderr "Bad response: %s\n" (res |> Response.status |> Code.string_of_status);
        Lwt.return (`Error `Bad_response)
      )
    else Lwt.return (`Ok ())
  in
  ok
  >>|= fun () ->
  (Cohttp_lwt_body.to_string body >>= fun s -> Lwt.return (`Ok s))
  >>|= fun body ->
  (try
    let date =
      Soup.(parse body $$ "p" |> to_list |> List.hd |> texts |> List.hd |> fun s -> String.sub s ~start:0 ~stop:10 |> String.Sub.to_string)
    in
    Lwt.return (`Ok date)
  with e ->
    Printf.fprintf stderr "Failed to parse body: %s\n" body;
    Lwt.return (`Error `Bad_data))
  >>|= fun date ->
  Lwt.return (`Ok date)

let _ =
  let carbon   = "http://coltrane.uk.xensource.com/usr/groups/build/carbon" in
  let uuid     = String.concat ~sep:"-" in
  let (//) x y = x ^"/"^ y in

  Lwt_main.run (
    run (uuid ["449e52a4";"271a";"483a";"baa7";"24bf362866f7"])
      (carbon // "trunk-ring3/xe-phase-3-latest/xe-phase-3")
      "s3://xs-yum-repos/" >>|= fun () ->

    run (uuid ["8efb3ee";"3c7d";"11e6";"b95e";"33de9392ddf2"])
      (carbon // "trunk-pvs-direct/xe-phase-3-latest/xe-phase-3/")
      "s3://xs-yum-repos/" >>|= fun () ->

    find_latest () >>|= fun s ->
    run (uuid ["f51c9e97";"9d3f";"434c";"b6f7";"ec2a7526db92"])
      (Printf.sprintf "http://downloadns.citrix.com.edgesuite.net/8170/%s" s)
      "s3://xs-yum-repos/"
    )

