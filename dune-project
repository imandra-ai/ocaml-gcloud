(lang dune 3.8)
(using melange 0.1)

(name gcloud)
(source (github imandra-ai/ocaml-gcloud))
(authors "Matt Bray <matt@imandra.ai>" "Dave Aitken <dave@imandra.ai>")
(maintainers "Imandra <tech@imandra.ai>")
(generate_opam_files true)

(package
 (name gcloud)
 (synopsis "Bindings to Google Cloud Platform APIs")
 (description "Runtime-agnostic core: types, and service bindings as functors over an async runtime and an HTTP client. Pair with a backend such as gcloud-lwt.")
 (depends
   base64
   cohttp
   containers
   dune
   ezgzip
   logs
   ppx_deriving
   ppx_deriving_yojson
   ptime
   uri
   yojson))

(package
 (name gcloud-lwt)
 (synopsis "Lwt + cohttp backend for ocaml-gcloud")
 (description "Credential discovery, HTTP client and Lwt instantiations of every gcloud service module.")
 (depends
  (alcotest :with-test)
  (alcotest-lwt :with-test)
   gcloud
   cohttp
   cohttp-lwt
   cohttp-lwt-unix
   containers
   cstruct
   dune
   jose
   logs
   lwt
  (ppx_here :with-test)
   ptime
  (tls :with-test)
   uri
  (x509 (and (>= 0.12.0) (< 1.0))) ;; Required for the ~sloppy:true arg becoming the default
   yojson))

(package
 (name gcloud-direct)
 (synopsis "Direct-style (OCaml 5 effects / thread pool) backend for ocaml-gcloud")
 (description "ezcurl HTTP client, credential discovery and direct-style instantiations of every gcloud service module, for moonpool, picos and plain threads. The optional gcloud-direct.picos sub-library adds a fiber-suspending sleep.")
 (depends
   gcloud
   cohttp
   containers
   cstruct
   dune
   ezcurl
   jose
   logs
   ocurl
   uri
  (x509 (and (>= 0.12.0) (< 1.0)))
   yojson)
 (depopts
   picos_std))

(package
 (name gcloud-melange)
 (synopsis "Bindings to Google Cloud Platform APIs")
 (description "")
 (depends
   melange))

(package
 (name gcloud-cli)
 (synopsis "CLI for accessing Google Cloud Platform APIs")
 (description "For when the official gcloud distribution is too heavyweight.")
 (depends
   gcloud-lwt
   (cohttp (< 6))
   cmdliner
   logs
   tls-lwt))
