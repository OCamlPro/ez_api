
(*
       (List.map (fun section ->
            EzAPI.section_name section,
            EzAPI.md_of_services ~section ?base_url
                                 (Api_info.files @ Service_doc.doc)
          ) (Service.sections
             @
               (match find_url_arg "doc" with
                | None -> []
                | Some _ -> Service.other_sections)))
     in
 *)

let json sections =

  let tags =
    List.map (fun s ->
        let name = EzAPI.section_name s in
        `O [
           "name", `String name;
         ]
      ) sections
  in
  let paths, definitions =
    EzAPI.paths_of_sections sections
  in
  `O [
     "swagger", `String "2.0";
     "info", `O [
         "description", `String "TzScan API, beta until stable";
         "version", `String "1.0";
         "title", `String "TzScan API";
         "termsOfService", `String "http://www.ocamlpro.com/legal/";
         "contact", `O [
                       "email", `String "contact@tzscan.io"
                     ];
         "license", `O [
                       "name", `String "Apache 2.0";
                       "url", `String
                               "http://www.apache.org/licenses/LICENSE-2.0.html"
                     ];
       ];
     "host", `String "api.tzscan.io";
     "basePath", `String "";
     "schemes", `A [ `String "http" ];
     "tags", `A tags;
     "paths", `O paths;
     "definitions", `O definitions;
   ]

let to_string sections = Ezjsonm.to_string (json sections)

let handler sections (_req : EzAPI.request) () =
    Lwt.fail (EzAPIServer.EzRawReturn
                (to_string sections))
