(lang dune 2.1)

(generate_opam_files true)

(authors "Rong Yang" "Meihan Lin")

(maintainers "test@test.com" )

(package
 (name rmovie) 
 (synopsis "An OCaml library for Movie Recommendation")
 (description "This is a movie recommendation Application with 3 recommender mode")
 (depends
  (core
   (>= 0.15.0)) 
  (ounit2
   (<= 2.2.6)) 
  (core_unix
   (>= 0.15.0))
  (csv
   (>= 2.4)) 
  (yojson
   (>= 2.0.2)) 
  (owl
   (>= 1.0.2-1)) 
  (sklearn 
   (>= sk0.23-0.3.1))
  (bisect_ppx
   (>= 2.8.1))
  (inquire
   (>= 0.3.1))
  (ppx_deriving
   (>= 5.2.1))
  ))
