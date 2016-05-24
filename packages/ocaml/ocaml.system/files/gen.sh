#!/bin/bash -ue

case "$(uname -s)" in
  CYGWIN*)
    WINDOWS=true
    SEP=\\
    EXE=.exe
    ;;
  *)
    WINDOWS=false
    SEP=/
    EXE=
    ;;
esac

if ! OCAMLC_SHELL=$(which ocamlc$EXE); then
    echo "No OCaml compiler was found on the system" >&2
    exit 2
fi

LIBDIR=$("$OCAMLC_SHELL" -where | tr -d '\r')
if $WINDOWS ; then
  LIBDIR_SHELL=$(cygpath "$LIBDIR")
  OCAMLC=$(cygpath -w "$OCAMLC_SHELL")
else
  LIBDIR_SHELL=$LIBDIR
  OCAMLC=$OCAMLC_SHELL
fi
STUBLIBS=$(cat $LIBDIR_SHELL/ld.conf | tr -d '\r' | tr '\n' ':')

echo "Using ocaml compiler found at $OCAMLC with base lib at $LIBDIR"

bool() {
    if "$@"; then echo "true"; else echo "false"; fi
}

cat >ocaml.config <<EOF
opam-version: "1.3.0~dev4"
file-depends: ["$(echo "$OCAMLC"| sed -e "s/\\\\/\\\\\\\\/g")" "$(md5sum "$OCAMLC_SHELL" | cut -d' ' -f1)"]
variables {
    ocaml-version: "$("$OCAMLC_SHELL" -version | tr -d '\r')"
    compiler: "system"
    preinstalled: true
    ocaml-native: $(bool [ -x "$(dirname "$OCAMLC_SHELL")"/ocamlopt ])
    ocaml-native-tools: $(bool [ -x "$(dirname "$OCAMLC_SHELL")"/ocamlc.opt$EXE ])
    ocaml-native-dynlink: $(bool [ -e "$LIBDIR_SHELL"/dynlink.cmxa ])
    ocaml-stubsdir: "$(echo "$STUBLIBS"| sed -e "s/\\\\/\\\\\\\\/g")"
}
EOF
