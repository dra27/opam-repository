wget https://raw.githubusercontent.com/dra27/ocaml-ci-scripts/test-system-switches/.travis-ocaml.sh

export OPAM_INIT=false
bash -ex .travis-ocaml.sh
