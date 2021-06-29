open Core

module Util = struct
  let dirname path =
    (* nothing is relative, all things are absolute! *)
    match path |> Filename.is_relative with
    | true -> Filename.concat "/" (Filename.dirname path)
    | false -> path |> Filename.dirname
end

module Item = struct
  type t = {
    parent : Slug.t option;
    slug : Slug.t;
    path : string;
    description : string;
    tags : string list;
  }

  let compare t1 t2 = String.equal t1.path t2.path

  let make ~parent ~slug ~path ~description ~tags =
    { parent; slug; path; description; tags }

  let title item = item.path |> Filename.basename

  let of_json ?(basepath = None) json =
    let slug =
      Ezjsonm.find json [ "slug" ]
      |> Ezjsonm.get_string |> Slug.of_string ~basepath
    in
    let path = Ezjsonm.find json [ "path" ] |> Ezjsonm.get_string in
    let description =
      Ezjsonm.find json [ "description" ] |> Ezjsonm.get_string
    in
    let tags = Ezjsonm.find json [ "tags" ] |> Ezjsonm.get_strings in
    let parent =
      match Ezjsonm.find_opt json [ "parent" ] with
      | Some parent -> (
          match parent with
          | `Null -> None
          | `String name -> Some (name |> Slug.of_string)
          | _ -> failwith "parent should be null or a string")
      | None -> None
    in
    { slug; parent; path; description; tags }

  let to_json item =
    let parent =
      match item.parent with
      | Some parent -> parent |> Slug.shortname |> Ezjsonm.string
      | None -> Ezjsonm.unit ()
    in
    Ezjsonm.dict
      [
        ("parent", parent);
        ("slug", item.slug |> Slug.shortname |> Ezjsonm.string);
        ("path", item.path |> Ezjsonm.string);
        ("description", item.description |> Ezjsonm.string);
        ("tags", item.tags |> Ezjsonm.strings);
      ]
end

type t = { state_dir : string; items : Item.t list }

let make state_dir = { state_dir; items = [] }

let empty = { state_dir = ""; items = [] }

let of_json ?(state_dir = None) json =
  let items =
    Ezjsonm.find json [ "items" ]
    |> Ezjsonm.get_list (fun item -> item |> Item.of_json ~basepath:state_dir)
  in
  let state_dir =
    match state_dir with Some state_dir -> state_dir | None -> "/"
  in
  { state_dir; items }

let to_json manifest =
  let items = Ezjsonm.list Item.to_json manifest.items in
  Ezjsonm.dict [ ("items", items) ]

let of_string ?(state_dir = None) manifest =
  manifest |> Ezjsonm.from_string |> of_json ~state_dir

let to_string manifest = manifest |> to_json |> Ezjsonm.to_string

let lockfile manifest = Filename.concat manifest.state_dir "note.lock"

let mpath manifest = Filename.concat manifest.state_dir "manifest.json"

let lock manifest =
  let lockfile = manifest |> lockfile in
  match lockfile |> Sys.file_exists with
  | `Yes -> failwith "unable to aquire lock"
  | `No | `Unknown -> Out_channel.write_all ~data:"<locked>" lockfile

let unlock manifest =
  let lockfile = manifest |> lockfile in
  match lockfile |> Sys.file_exists with
  | `Yes -> Sys.remove lockfile
  | `No | `Unknown -> ()

let load_or_init state_dir =
  let mpath = Filename.concat state_dir "manifest.json" in
  match Sys.file_exists mpath with
  | `Yes ->
      mpath |> In_channel.read_all |> of_string ~state_dir:(Some state_dir)
  | `No | `Unknown ->
      mpath |> Out_channel.write_all ~data:(to_string empty);
      make state_dir

let save manifest =
  manifest |> lock;
  Out_channel.write_all ~data:(to_string manifest) (manifest |> mpath);
  manifest |> unlock

let find ~path manifest =
  manifest.items |> List.find ~f:(fun item -> Filename.equal item.path path)

(* TODO: no support for recursive operations yet *)
let create ~path ~description ~tags manifest =
  if
    Option.is_some
      (manifest.items
      |> List.find ~f:(fun item -> Filename.equal item.path path))
  then failwith "duplicate entry"
  else
    let parent_dir = path |> Filename.dirname in
    let last_slug =
      match manifest.items |> List.hd with
      | Some item -> Some item.slug
      | None -> None
    in
    let next_slug = Slug.next ~last:last_slug manifest.state_dir in
    match parent_dir with
    | "." | "/" | "" ->
        (* root entry *)
        let item =
          Item.make ~parent:None ~slug:next_slug ~path ~description ~tags
        in
        { manifest with items = item :: manifest.items }
    | parent_dir -> (
        let parent = manifest |> find ~path:parent_dir in
        match parent with
        | Some parent ->
            let parent_slug = parent.slug in
            let item =
              Item.make ~parent:(Some parent_slug) ~slug:next_slug ~path
                ~description ~tags
            in
            { manifest with items = item :: manifest.items }
        | None -> failwith "no parent")

let list ~path manifest =
  manifest.items
  |> List.filter ~f:(fun item ->
         String.equal (item.path |> Filename.dirname) path)

let remove ~path manifest =
  match manifest |> list ~path |> List.length with
  | 0 ->
      let items =
        manifest.items
        |> List.filter ~f:(fun item -> not (Filename.equal item.path path))
      in
      { manifest with items }
  | _ -> failwith "will not delete recursively"

let update ~path ~description ~tags manifest =
  let result =
    manifest.items
    |> List.findi ~f:(fun _ item -> Filename.equal item.path path)
  in
  match result with
  | Some (other, _) ->
      let items =
        manifest.items
        |> List.foldi ~init:[] ~f:(fun index accm item ->
               if Int.equal index other then
                 let item = { item with description; tags } in
                 item :: accm
               else item :: accm)
      in
      { manifest with items }
  | None -> failwith "not found"

let move ~source ~dest manifest =
  let item = manifest |> find ~path:source in
  let others = manifest |> list ~path:source in
  match others |> List.length with
  | 0 -> (
      match item with
      | Some item ->
          let description, tags = (item.description, item.tags) in
          let manifest = manifest |> remove ~path:source in
          manifest |> create ~path:dest ~description ~tags
      | None -> failwith "not found")
  | _ -> failwith "cannot update recursively"
