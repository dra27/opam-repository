#!/bin/bash -ue

# Ensure the environment isn't accidentally polluted with pre-set values from msvs-tools
MSVS_NAME=
MSVS_PATH=
MSVS_INC=
MSVS_LIB=

# Variables passed by OPAM:
#   $1 = switch-cc
#   $2 = switch-libc
#   $3 = switch-arch
#   $4 = Version number of the msvs-tools tarball

cc=$1
libc=$2
arch=$3

if [ "$libc" = "msvc" ] ; then
  if [ "$cc" = "cl" ] ; then
    if [ "$arch" = "x86_64" ] ; then
      MSVS_ARCH=x64
    else
      MSVS_ARCH=x86
    fi

    tar -xzf msvs-tools-$4.tar.gz
    # Determine if the Microsoft C Compiler is available
    eval $(msvs-tools-$4/msvs-detect --arch=$MSVS_ARCH --with-mt --with-assembler)

    if [ "$MSVS_NAME" = "" ] ; then
      echo No Microsoft C compiler could be found>&2
      exit 2
    else
      MSVS_PATH=$(cygpath -pw "${MSVS_PATH%:}")
    fi
  fi
fi

case $cc-$libc-$arch in
  cc-msvc-i686)
    conf=mingw
    o=o
    a=a
    so=dll
    exe=.exe
    ;;
  cc-msvc-x86_64)
    conf=mingw64
    o=o
    a=a
    so=dll
    exe=.exe
    ;;
  cl-msvc-i686)
    conf=msvc
    o=obj
    a=lib
    so=dll
    exe=.exe
    ;;
  cl-msvc-x86_64)
    conf=msvc64
    o=obj
    a=lib
    so=dll
    exe=.exe
    ;;
  cl-libc-*)
    echo "Microsoft compilers cannot target libc!" >&2
    exit 2
    ;;
  *)
    conf=other
    o=o
    a=a
    so=so
    exe=
    ;;
esac

cat >compiler.config <<EOF
opam-version: "2.0"
variables {
    ocaml-win-conf: "$conf"
    o: "$o"
    a: "$a"
    so: "$so"
    exe: "$exe"
    msvs-name: "$MSVS_NAME"
    msvs-path: "$(echo $MSVS_PATH| sed -e "s/\\\\/\\\\\\\\/g")"
    msvs-inc: "$(echo $MSVS_INC| sed -e "s/\\\\/\\\\\\\\/g")"
    msvs-lib: "$(echo $MSVS_LIB| sed -e "s/\\\\/\\\\\\\\/g")"
}
EOF
