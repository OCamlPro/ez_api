(lang dune 2.8)
(generate_opam_files true)
(name ez_api)
(source (github ocamlpro/ez_api))
(license LGPL-2.1-only)
(authors
 "Cagdas Bozman <cagdas.bozman@ocamlpro.com>"
 "Michael Laporte <michael.laporte@ocamlpro.com>"
 "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>"
 "Maxime Levillain <maxime.levillain@ocamlpro.com>"
 "Alain Mebsout <alain.mebsout@ocamlpro.com>")
(maintainers "Fabrice Le Fessant <fabrice.le_fessant@ocamlpro.com>")

(package
 (name ez_api)
 (synopsis "Easy API library and tools")
 (documentation "https://ocamlpro.github.io/ez_api/ez_api/index.html")
 (depends
  (ocaml (>= 4.08))
  (json-data-encoding (>= 1.0.1))
  (ezjsonm (>= 1.1.0))
  uuidm)
 (depopts
  lwt
  geoip
  js_of_ocaml-lwt
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  cohttp-lwt-jsoo
  httpaf-lwt-unix
  curl
  curl_lwt
  ezjs_fetch
  ppx_deriving_encoding
  digestif
  yaml
  httpun
  httpun-lwt-unix
  httpun-ws
  httpun-ws-lwt-unix
  tls-lwt
  lwt_ssl
  eio
  httpun-eio
  httpun-ws-eio
  cohttp-eio
  js_of_ocaml-eio
  tls-eio
  eio-ssl
  websocket
 )
 (conflicts
  (js_of_ocaml-lwt (< 6.0.0))
  (cohttp-lwt-unix (< 5.0.0))
  (cohttp-lwt-jsoo (< 5.0.0))
  (calendar (< 2.03.0))
  (httpaf-lwt-unix (< 0.6.0))
  (ezjs_fetch (< 0.2))
  (digestif (< 1.0.0))
  (ppx_deriving_encoding (< 0.4.0))
))
