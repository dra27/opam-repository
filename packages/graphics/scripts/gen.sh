#!/usr/bin/env bash

if [ ! -d ../ocaml-base-compiler ] ; then
  echo "Script should be run from within the package directory">&2
  echo "(can't find OCaml package in ../ocaml-base-compiler)">&2
  exit 1
fi

if ! command -v opam-ed &>/dev/null ; then
  echo "opam-ed is required for this script">&2
  exit 1
fi

for i in ../ocaml-base-compiler/* ; do
  if [ -d "$i" ] ; then
    RAW_VERSION=${i#../ocaml-base-compiler/ocaml-base-compiler.}
    VERSION=${RAW_VERSION//./}
    VERSION=${VERSION//+/}
    if [ "$VERSION" = "307" ] ; then
      VERSION=3070
    fi
  fi
  if [[ $VERSION -lt 3100 ]] ; then
    PATCH=to-3.09.3
  elif [[ $VERSION -eq 3100 ]] ; then
    PATCH=3.10.0
  elif [[ $VERSION -lt 3110 ]] ; then
    PATCH=to-3.10.2
  elif [[ $VERSION -lt 4000 ]] ; then
    PATCH=to-3.12.1
  else
    PATCH=
  fi
  PACKAGE=graphics.$RAW_VERSION
  mkdir -p $PACKAGE/files
  sed -e "s/VERSION/$RAW_VERSION/" scripts/META > $PACKAGE/files/META
  cp -f scripts/install.sh $PACKAGE/files/install.sh
  cp -f scripts/opam $PACKAGE/opam
  cat >> $PACKAGE/opam <<EOF
extra-files: [
  ["META" "md5=$(md5sum $PACKAGE/files/META | cut -d ' ' -f 1)"]
  ["install.sh" "md5=$(md5sum $PACKAGE/files/install.sh | cut -d ' ' -f 1)"]]
EOF
  EXTRAS=$(opam-ed -f $i/opam 'field-list' | fgrep extra-source.src | wc -l)
  if [[ $EXTRAS -gt 1 ]] ; then
    # Apparent limitation in opam-ed
    echo "Warning: $i/opam contains more than one extra-source; only the first will have been copied"
  fi
  PATCHES=$(opam-ed -f $i/opam 'get patches' 2>/dev/null)
  opam-ed -f $PACKAGE/opam -i "add url.src $(opam-ed -f $i/opam 'get url.src')" \
                              "add url.checksum $(opam-ed -f $i/opam 'get url.checksum')" \
                              "append depends \"ocaml\" {= \"$RAW_VERSION\"}"
  if [[ -n $PATCHES ]] ; then
    opam-ed -f $PACKAGE/opam -i "add patches $(opam-ed -f $i/opam 'get patches')"
  fi
  if [[ $EXTRAS -gt 0 ]] ; then
    opam-ed -f $PACKAGE/opam -i "add extra-source.src $(opam-ed -f $i/opam 'get extra-source.src')" \
                                "add extra-source.checksum $(opam-ed -f $i/opam 'get extra-source.checksum')"
    sed -i -e "s/extra-source {/$(fgrep extra-source $i/opam | head -n 1)/" $PACKAGE/opam
  fi
  if [[ -n $PATCH ]] ; then
    PATCH=PR5477-$PATCH.patch
    URL=https://raw.githubusercontent.com/metastack/ocaml-legacy/master/$PATCH
    if [[ ! -e scripts/$PATCH ]] ; then
      CHECKSUM=$(curl -Ls $URL | md5sum | cut -d ' ' -f 1 > scripts/$PATCH)
    else
      CHECKSUM=$(cat scripts/$PATCH)
    fi
    if [[ $EXTRAS -gt 0 ]] ; then
      cat >> $PACKAGE/opam <<EOF
extra-source "PR5477.patch" {
  src:
    "$URL"
  checksum: "md5=$CHECKSUM"
}
EOF
      opam-ed -f $PACKAGE/opam -i "append patches \"PR5477.patch\""
    else
      opam-ed -f $PACKAGE/opam -i "add extra-source.src \"$URL\"" \
                                  "add extra-source.checksum \"md5=$CHECKSUM\"" \
                                  "append patches \"PR5477.patch\""
      sed -i -e 's/extra-source {/extra-source "PR5477.patch" {/' $PACKAGE/opam
    fi
  fi
done
