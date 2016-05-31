#!/bin/bash -ue

# Ensure the environment isn't accidentally polluted with pre-set values from msvs-tools
MSVS_NAME=
MSVS_PATH=
MSVS_INC=
MSVS_LIB=

# Variables passed by OPAM:
#   $1 = ocaml:target-version (may be empty)
#   $2 = os
#   $3 = arch (only x86_64 and i686 are relevant)
#   $4 = switch-cc
#   $5 = switch-libc
#   $6 = switch-arch
#   $7 = Version number of the msvs-tools tarball

if [ "$1" = "system" ] ; then
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

  if ! OCAMLC=$(which ocamlc$EXE); then
      echo "No OCaml compiler was found on the system" >&2
      exit 2
  fi

  libc=msvc
  SYSTEM=$("$OCAMLC" -config | tr -d '\r' | grep "^system: " | sed -e "s/system: //")
  case $SYSTEM in
    mingw*)
      conf=$SYSTEM
      ;;
    win32*)
      conf=msvc
      ;;
    win64*)
      conf=msvc64
      ;;
    *)
    conf=other
      libc=libc
    ;;
  esac
  o=$("$OCAMLC" -config | tr -d '\r' | grep "^ext_obj: " | sed -e "s/ext_obj: .//")
  a=$("$OCAMLC" -config | tr -d '\r' | grep "^ext_lib: " | sed -e "s/ext_lib: .//")
  so=$("$OCAMLC" -config | tr -d '\r' | grep "^ext_dll: " | sed -e "s/ext_dll: .//")

  cc=$("$OCAMLC" -config | tr -d '\r' | grep "^ccomp_type: " | sed -e "s/ccomp_type: //" -e "s/msvc/cl/")

  case "$("$OCAMLC" -config | tr -d '\r' | grep "^architecture: " | sed -e "s/architecture: //")" in
    i386)
      arch=i686
      ;;
    amd64)
      arch=x86_64
      ;;
    *)
      arch=other
      ;;
  esac
else
  # switch-cc, switch-libc and switch-arch are "default" if not specified on the OPAM command line

  # Default switch-libc to msvc (i.e. mingw or msvc ports, depending on switch-cc) on Windows
  # Note that os != "win32" for Cygwin OPAM
  if [ "$5" = "default" ] ; then
    if [ "$2" = "win32" ] ; then
      libc=msvc
    else
      libc=libc
    fi
  else
    libc=$5
  fi

  # Default switch-arch to arch (passed in $3)
  if [ "$6" = "default" ] ; then
    arch=$3
  else
    arch=$6
  fi

  # Probe the system to determine the default for switch-cc on Windows
  # Microsoft C compiler is "preferred" (because it's optional)
  if [ "$libc" = "msvc" ] ; then
    if [ "$4" != "cc" ] ; then
      if [ "$arch" = "x86_64" ] ; then
        MSVS_ARCH=x64
      else
        MSVS_ARCH=x86
      fi

      tar -xzf msvs-tools-$7.tar.gz
      # Determine if the Microsoft C Compiler is available
      eval $(msvs-tools-$7/msvs-detect --arch=$MSVS_ARCH --with-mt --with-assembler)
      # When running on 64-bit Windows, allow falling back to a 32-bit compiler (as long as --arch wasn't specified)
      if [ "$MSVS_NAME" = "" -a "$3" = "x86_64" -a "$6" = "default" ] ; then
        eval $(msvs-tools-$7/msvs-detect --arch=x86 --with-mt --with-assembler)
        if [ "$MSVS_NAME" != "" ] ; then
          arch=i686
        fi
      fi

      if [ "$MSVS_NAME" = "" ] ; then
        if [ "$4" = "default" ] ; then
          cc=cc
        else
          echo No Microsoft C compiler could be found>&2
          exit 2
        fi
      else
        MSVS_PATH=$(cygpath -pw "${MSVS_PATH%:}")
        cc=cl
      fi
    else
      cc=cc
    fi
  else
    # Always using cc-like compiler for non-Microsoft C Runtime
    cc=cc
  fi

  case $cc-$libc-$arch in
    cc-msvc-i686)
      conf=mingw
      o=o
      a=a
      so=dll
      ;;
    cc-msvc-x86_64)
      conf=mingw64
      o=o
      a=a
      so=dll
      ;;
    cl-msvc-i686)
      conf=msvc
      o=obj
      a=lib
      so=dll
      ;;
    cl-msvc-x86_64)
      conf=msvc64
      o=obj
      a=lib
      so=dll
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
      ;;
  esac
fi

cat >compiler.config <<EOF
opam-version: "2.0"
variables {
    ocaml-win-conf: "$conf"
    o: "$o"
    a: "$a"
    so: "$so"
    cc: "$cc"
    libc: "$libc"
    target-arch: "$arch"
    msvs-name: "$MSVS_NAME"
    msvs-path: "$(echo $MSVS_PATH| sed -e "s/\\\\/\\\\\\\\/g")"
    msvs-inc: "$(echo $MSVS_INC| sed -e "s/\\\\/\\\\\\\\/g")"
    msvs-lib: "$(echo $MSVS_LIB| sed -e "s/\\\\/\\\\\\\\/g")"
}
EOF
