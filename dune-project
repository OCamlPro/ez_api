(lang dune 2.8)
(name ez_api)

(source (github ocamlpro/ez_api))
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
 (documentation "https://ocpmax.github.io/ez_api/ez_api/index.html")
 (depends
  (ocaml (>= 4.08))
  (json-data-encoding (>= 0.7.0))
  (lwt (>= 4.0.0))
  (ezjsonm (>= 1.0.0))
  uuidm)
 (depopts
  geoip
  js_of_ocaml-lwt
  (calendar (>= 2.03))
  cohttp-lwt-unix
  (cohttp-lwt-jsoo (>= 4.0.0))
  lwt_log
  tls
  (httpaf-lwt-unix (>= 0.6.0))
  (ocurl (>= 0.8.0))
  (ezjs_fetch (>= 0.2))
  websocket-lwt-unix
  websocket-httpaf-lwt
  ppxlib
  ppx_deriving_encoding
  (digestif (>= 1.0.0)))
 (conflicts
  (cohttp-lwt-jsoo (< 4.0.0))
  (calendar (< 2.03.0))
  (httpaf-lwt-unix (< 0.6.0))
  (ezjs_fetch (< 0.2))
  (ocurl (< 0.8.0))
  (digestif (< 1.0.0))
))
