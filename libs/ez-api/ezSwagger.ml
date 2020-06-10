
let json
    ?(descr="API")
    ?(version="1.0")
    ?(title="Tzscan API")
    ?(terms="http://www.ocamlpro.com")
    ?(contact="contact@tzscan.io")
    ?(license=("AGPL 3.0", "https://www.gnu.org/licenses/agpl-3.0.en.html"))
    ?(servers=["https://api2.tzscan.io", "Tzscan API server"])
    ?docs
    sections =

  let servers = List.map (fun (url, descr_server) ->
      `O [ "url", `String url; "description", `String descr_server ] ) servers in
  let tags =
    List.map (fun s ->
        let name = EzAPI.section_name s in
        `O [
          "name", `String name;
        ]
      ) sections
  in
  let paths, definitions =
    EzAPI.paths_of_sections ?docs sections
  in
  `O [
    "openapi", `String "3.0.2";
    "info", `O [
      "description", `String descr;
      "version", `String version;
      "title", `String title;
      "termsOfService", `String terms;
      "contact", `O [
        "email", `String contact
      ];
      "license", `O [
        "name", `String (fst license);
        "url", `String (snd license)
      ];
    ];
    "servers", `A servers;
    "tags", `A tags;
    "paths", `O paths;
    "components", `O definitions;
  ]

let to_string
    ?descr ?version ?title ?terms ?contact ?license ?servers ?docs sections =
  Ezjsonm.to_string @@
  json ?descr ?version ?title ?terms ?contact ?license ?servers ?docs sections

let handler sections (_req : EzAPI.request) () =
    Lwt.fail (EzAPIServerUtils.EzRawReturn
                (to_string sections))
