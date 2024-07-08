## [2.1.0](https://github.com/ocamlpro/ez_api/compare/v2.0.0...2.1.0) (2024-07-08)

* revamp registration of services (register/hide)
* fixes:
  * allow different methods with same path
  * 404 instead of 405 if the path has no services
* improvements:
  * extension syntax instead of attributes for ppx
  * allow multipart in curl common
  * use http code defined in error cases in the server
* remove deprecated curl_multi

## [2.0.0](https://github.com/ocamlpro/ez_api/compare/1.2.0...v2.0.0) (2023-02-06)

* fixes:
  * capital method names
  * curl callback
* improvements:
  * export session services
  * cookie support

## [1.2.0](https://github.com/ocamlpro/ez_api/compare/1.1.0...1.2.0) (2022-06-27)

* release dependencies constraints
* better result errors

## [1.1.0](https://github.com/ocamlpro/ez_api/compare/v1.0.0...1.1.0) (2022-04-11)

* allow collision of static and dynamic path in server
* update js_of_ocaml, cohttp, ppx_deriving_encoding deps

## [1.0.0](https://github.com/ocamlpro/ez_api/compare/0.1.0...v1.0.0) (2021-12-01)

* better optional dependencies
* various fixes

## 0.1.0 (2021-31-03)

First release
