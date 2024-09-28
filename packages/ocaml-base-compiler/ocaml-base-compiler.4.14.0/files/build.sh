#!/bin/sh

if [ -z "$2" ] ; then
  # Installation
  if [ -e 'Makefile.config' ] ; then
    make install
  else
    # Cloning
    SOURCE="$(cat clone-from)"
    cp "$SOURCE/share/ocaml/config.cache" .
    mkdir -p "$1/man/man1"
    cp "$SOURCE/man/man1/"ocaml* "$1/man/man1/"
    mkdir -p "$1/bin"
    cp "$SOURCE/bin/"ocaml* "$1/bin/"
    rm -rf "$1/lib/ocaml"
    cp -a "$SOURCE/lib/ocaml" "$1/lib/"
  fi
else
  # Building

  # $1 = prefix; $2 = os; $3 = jobs; $4 = runtimeID (for now)

  find $OPAMROOT -maxdepth 1 -mindepth 1 -type d | while IFS= read -r dir ; do
    if [ -e "$dir/lib/ocaml/Makefile.config" ] ; then
      if grep -Fq "BYTECODE_RUNTIME_ID=$4" "$dir/lib/ocaml/Makefile.config" ; then
        echo "$dir" > clone-from
      fi
    fi
  done

  if [ -e 'clone-from' ] ; then
    exit 0
  fi

  tar --strip-components=1 -xf ocaml.tar.gz

  if [ "$2" = "openbsd" ] || [ "$2" = "macos" ] ; then
    # Does this work - configure should pick it up?
    export CC=cc
    export ASPP='cc -c'
  fi

  if [ "$2" = "win32" ] ; then
    rm -rf flexdll
    tar -xzf flexdll.tar.gz
    mv flexdll-0.41 flexdll
    ./configure --prefix "$1" --host=x86_64-pc-windows --enable-relative --enable-runtime-search=always -C
    # Just a warning on 4.13
    make "-j$3" flexdll
  else
    ./configure --prefix "$1" --enable-relative --enable-runtime-search=always -C
  fi
  make "-j$3"
fi
