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
 (description "")
 (depends
  (alcotest :with-test)
  (alcotest-lwt :with-test)
   base64
   cohttp
   cohttp-lwt-unix
   containers
   cstruct
   dune
   ezgzip
   jose
   logs
   lwt
   ppx_deriving_yojson
  (ppx_here :with-test)
  (tls :with-test)
  (x509 ( >= 0.12.0)) ;; Required for the ~sloppy:true arg becoming the default
   yojson))

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
   gcloud
   (cohttp (< 6))
   cmdliner
   logs
   tls-lwt))
