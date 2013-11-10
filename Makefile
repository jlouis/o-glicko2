RESULT = glicko2
SOURCES = \
  glicko2.mli glicko2.ml \
  bench.ml

PACKS = core core_bench
THREADS = yes
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)