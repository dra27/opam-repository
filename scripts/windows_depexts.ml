(* @dra27, in mild anger, 20-Apr-2026. Nemo nunc te poteste servare *)

let repo = OpamFilename.Dir.of_string "/home/dra/opam-repository-4"
let all_packages = OpamRepository.packages repo

(* XXX Doesn't this rescan relative to the above?? *)
let iter_packages ?(quiet=false) f =
  let packages = OpamRepository.packages_with_prefixes repo in
  let changed_pkgs = ref 0 in
  let changed_files = ref 0 in
  (* packages *)
  OpamPackage.Map.iter (fun package prefix ->
      if not quiet then
        OpamConsole.msg "Processing package %s... "
          (OpamPackage.to_string package);
      let opam_file = OpamRepositoryPath.opam repo prefix package in
      let opam = OpamFile.OPAM.read opam_file in
      let opam2 =
        f package ~prefix ~opam
      in
      let changed = ref false in
      let upd () = changed := true; incr changed_files in
      if not (OpamFile.OPAM.equal opam opam2) then
        (upd (); OpamFile.OPAM.write_with_preserved_format opam_file opam2);
      if !changed then
        (incr changed_pkgs;
         if not quiet then begin
           OpamConsole.carriage_delete ();
           OpamConsole.msg "Updated %s\n" (OpamPackage.to_string package)
         end)
      else if not quiet then
        OpamConsole.carriage_delete ();
    ) packages;
  if not quiet then
    OpamConsole.msg "Done. Updated %d files in %d packages.\n"
      !changed_files !changed_pkgs

type distribution = MSYS2 | Cygwin
type system = X86_64 | I686
module SystemSet = Set.Make(struct
  type t = system
  let compare = Stdlib.compare
end)

type mechanism =
| No_test
| Pkgconf of string
| Version of string

type depext_info = (string * mechanism) option

type depext = {
  name: string;
  project: string;
  authors: string list;
  license: string list;
  homepage: string;
  systems: [ `X86_64 of msys2:depext_info * cygwin:depext_info
           | `I686 of msys2:depext_info * cygwin:depext_info ] list;
}

let base_version = OpamPackage.Version.of_string "1"

let parse_filtered_formula (s : string) : OpamTypes.filtered_formula =
  let v = OpamParser.FullPos.value_from_string s "<string>" in
  OpamPp.parse
    (OpamFormat.V.package_formula `Conj OpamFormat.V.(filtered_constraints ext_version))
    ~pos:OpamTypesBase.pos_null v

let print_filtered_formula (ff : OpamTypes.filtered_formula) : string =
  let v =
    OpamPp.print
      (OpamFormat.V.package_formula `Conj
         OpamFormat.V.(filtered_constraints ext_version))
      ff
  in
  OpamPrinter.FullPos.value v

let canonicalise depends =
  parse_filtered_formula (print_filtered_formula depends)

let depexts =
  let rec systems ?(template = `Default) ?pkgconf depext =
    let pkgconf = Option.value ~default:depext pkgconf in
    match template with
    | `Default -> systems ~template:(`Cygwin depext) ~pkgconf depext
    | `Cygwin cygwin_depext -> [
        `X86_64 (~msys2:(Some (depext, Pkgconf pkgconf)), ~cygwin:(Some (cygwin_depext, Pkgconf pkgconf)));
        `I686 (~msys2:(Some (depext, Pkgconf pkgconf)), ~cygwin:(Some (cygwin_depext, Pkgconf pkgconf)))]
    | `MSYS2_only -> [
        `X86_64 (~msys2:(Some (depext, Pkgconf pkgconf)), ~cygwin:None);
        `I686 (~msys2:(Some (depext, Pkgconf pkgconf)), ~cygwin:None)]
  in
  (* XXX Missing mechanism variants:
     - gcc, g++, pkgconf use direct binary invocation (e.g. "x86_64-w64-mingw32-gcc" "--version"),
       not pkgconf. They need a new mechanism variant, e.g. Binary of string.
     - ucrt-gcc is similar but deferred for now. *)

  (* XXX license field is string but some packages have multiple licenses
     (cairo: LGPL-2.1-only + MPL-1.1, liblz4: GPL-2.0-only + BSD-2-Clause).
     Consider changing to string list. *)

  let depexts = [
    {name     = "allegro5";
     project  = "allegro5";
     authors  = ["The Allegro authors"];
     license  = ["Giftware"];
     homepage = "https://liballeg.org";
     systems  = systems ~template:`MSYS2_only ~pkgconf:"allegro-5" "allegro"};
    {name     = "ao";
     project  = "libao";
     authors  = ["libao dev team"];
     license  = ["GPL-2.0-only"];
     homepage = "https://www.xiph.org/ao/";
     systems  = systems ~pkgconf:"ao" "libao"};
    (* XXX bzip2: Cygwin package has no bzip2.pc file.
       Existing opam uses "true" {os = "win32" & os-distribution = "cygwin"} as the build
       command on Cygwin, with a comment:
         https://cygwin.com/cgi-bin2/package-cat.cgi?file=noarch%2Fmingw64-x86_64-bzip2%2Fmingw64-x86_64-bzip2-1.0.6-4
         no bzip2.pc file in cygwin package
       Also: existing opam is missing conf-mingw-w64-gcc-x86_64 dep. *)
    {name     = "bzip2";
     project  = "bzip2";
     authors  = ["Julian Seward"];
     license  = ["bzip2-1.0.6"];
     homepage = "https://sourceware.org/bzip2/";
     systems  = [
       `X86_64 (~msys2:(Some ("bzip2", Pkgconf "bzip2")), ~cygwin:(Some ("bzip2", No_test)));
       `I686 (~msys2:(Some ("bzip2", Pkgconf "bzip2")), ~cygwin:(Some ("bzip2", No_test)));
     ]};
    {name     = "cairo";
     project  = "cairo";
     authors  = ["Keith Packard"; "Carl Worth"; "Behdad Esfahbod"];
     license  = ["LGPL-2.1-only"; "MPL-1.1"];
     homepage = "http://cairographics.org/";
     systems  = systems "cairo"};
    {name     = "curl";
     project  = "libcurl";
     authors  = ["Daniel Stenberg"];
     license  = ["MIT"];
     homepage = "http://curl.haxx.se/";
     systems  = systems ~pkgconf:"libcurl" "curl"};
    (* XXX freeglut: Cygwin package is missing a pkg-config .pc file.
       Existing opam only probes on MSYS2:
         ["pkg-config" "--personality=x86_64-w64-mingw32" "freeglut"] {os = "win32" & os-distribution = "msys2"}
       Also uses "pkg-config" instead of "pkgconf". *)
    {name     = "freeglut";
     project  = "FreeGLUT";
     authors  = ["Pawel W. Olszta"; "Andreas Umbach"; "Steve Baker"; "John F. Fay"; "John Tsiombikas"; "Diederick C. Niehorster"];
     license  = ["X11"];
     homepage = "https://freeglut.sourceforge.net/";
     systems  = [
       `X86_64 (~msys2:(Some ("freeglut", Pkgconf "freeglut")), ~cygwin:(Some ("freeglut", No_test)));
       `I686 (~msys2:(Some ("freeglut", Pkgconf "freeglut")), ~cygwin:(Some ("freeglut", No_test)));
     ]};
    {name     = "freetype";
     project  = "freetype";
     authors  = ["Christophe Troestler <Christophe.Troestler@umons.ac.be>"]; (* FIXME These are opam pkg authors, not freetype authors *)
     license  = ["GPL-1.0-or-later"];
     homepage = "http://www.freetype.org";
     systems  = systems ~template:(`Cygwin "freetype2") ~pkgconf:"freetype2" "freetype"};
    {name     = "glade";
     project  = "glade";
     authors  = ["The Glade Authors"];
     license  = ["LGPL-2.1-or-later"];
     homepage = "https://glade.gnome.org/";
     systems  = [
       `X86_64 (~msys2:(Some ("libglade", Pkgconf "libglade-2.0")), ~cygwin:(Some ("libglade2.0", Pkgconf "libglade-2.0")));
       `I686 (~msys2:None, ~cygwin:(Some ("libglade2.0", Pkgconf "libglade-2.0")));
     ]};
    (* XXX gmp: existing opam has --personality only for Cygwin:
         build: ["pkgconf" "--personality=x86_64-w64-mingw32" {os-distribution = "cygwin"} "gmp"]
       The current model always adds --personality for Cygwin. On MSYS2, pkgconf
       is run without --personality, which differs from the standard pattern. *)
    {name     = "gmp";
     project  = "libgmp";
     authors  = ["Torbjörn Granlund et al"];
     license  = ["GPL-1.0-or-later"];
     homepage = "http://gmplib.org/";
     systems  = systems "gmp"};
    {name     = "g++";
     project  = "g++";
     authors  = ["David Allsopp"]; (* FIXME Not last time I checked... *)
     license  = ["GPL-3.0-or-later"];
     homepage = "https://www.mingw-w64.org";
     systems  = [
       `X86_64 (~msys2:(Some ("", Version "x86_64-w64-mingw32-g++")), ~cygwin:(Some ("gcc-g++", Version "x86_64-w64-mingw32-g++")));
       `I686 (~msys2:(Some ("", Version "i686-w64-mingw32-g++")), ~cygwin:(Some ("gcc-g++", Version "i686-w64-mingw32-g++")));
     ]};
    {name     = "gcc";
     project  = "GCC";
     authors  = ["David Allsopp"]; (* FIXME Not last time I checked... *)
     license  = ["GPL-3.0-or-later"];
     homepage = "https://www.mingw-w64.org";
     systems  = [
       `X86_64 (~msys2:(Some ("gcc", Version "x86_64-w64-mingw32-gcc")), ~cygwin:(Some ("gcc-core", Version "x86_64-w64-mingw32-gcc")));
       `I686 (~msys2:(Some ("gcc", Version "i686-w64-mingw32-gcc")), ~cygwin:(Some ("gcc-core", Version "i686-w64-mingw32-gcc")))
     ]};
    (* XXX gnomecanvas: existing opam has two separate build commands:
         ["pkgconf" "--personality=x86_64-w64-mingw32" "libgnomecanvas-2.0"] {os = "win32" & os-distribution = "cygwin"}
         ["pkg-config" "libgnomecanvas-2.0"] {os = "win32" & os-distribution = "msys2"}
       Uses pkgconf with --personality on Cygwin but plain pkg-config on MSYS2.
       Current model can't express per-distribution build commands. *)
    {name     = "gnomecanvas";
     project  = "gnomecanvas";
     authors  = ["The GNOME Project"];
     license  = ["LGPL-2.1-or-later"];
     homepage = "https://developer.gnome.org/libgnomecanvas/2.30/";
     systems  = [
       `X86_64 (~msys2:(Some ("libgnomecanvas", Pkgconf "libgnomecanvas-2.0")), ~cygwin:(Some ("libgnomecanvas2", Pkgconf "libgnomecanvas-2.0")));
       `I686 (~msys2:None, ~cygwin:(Some ("libgnomecanvas2", Pkgconf "libgnomecanvas-2.0")));
     ]};
    {name     = "gnutls";
     project  = "GnuTLS";
     authors  = ["Nikos Mavrogiannopoulos"; "Simon Josefsson"];
     license  = ["LGPL-2.1-or-later"];
     homepage = "https://www.gnutls.org";
     systems  = systems "gnutls"};
    {name     = "gtk2";
     project  = "gtk2";
     authors  = ["The GNOME Project"];
     license  = ["LGPL-2.1-or-later"];
     homepage = "https://gtk.org/";
     systems  = [
       `X86_64 (~msys2:(Some ("gtk2", Pkgconf "gtk+-2.0")), ~cygwin:(Some ("gtk2.0", Pkgconf "gtk+-2.0")));
       `I686 (~msys2:None, ~cygwin:(Some ("gtk2.0", Pkgconf "gtk+-2.0")));
     ]};
    {name     = "gtk3";
     project  = "GTK+ 3";
     authors  = ["The GTK Toolkit"];
     license  = ["LGPL-2.1-or-later"];
     homepage = "https://www.gtk.org/";
     systems  = systems ~pkgconf:"gtk+-3.0" "gtk3"};
    {name     = "gtksourceview3";
     project  = "gtksourceview3";
     authors  = ["The gtksourceview programmers"];
     license  = ["LGPL-2.1-or-later"];
     homepage = "https://projects.gnome.org/gtksourceview/";
     systems  = [
       `X86_64 (~msys2:(Some ("gtksourceview3", Pkgconf "gtksourceview-3.0")), ~cygwin:(Some ("gtksourceview3.0", Pkgconf "gtksourceview-3.0")));
       `I686 (~msys2:None, ~cygwin:(Some ("gtksourceview3.0", Pkgconf "gtksourceview-3.0")));
     ]};
    {name     = "libevent";
     project  = "libevent";
     authors  = ["Libevent dev team"];
     license  = ["BSD-3-clause"];
     homepage = "https://libevent.org";
     systems  = [
       `X86_64 (~msys2:(Some ("libevent", Pkgconf "libevent")), ~cygwin:(Some ("libevent", Pkgconf "libevent")));
       `I686 (~msys2:None, ~cygwin:(Some ("libevent", Pkgconf "libevent")));
     ]};
    {name     = "libffi";
     project  = "libffi";
     authors  = ["Anthony Green"];
     license  = ["MIT"];
     homepage = "https://sourceware.org/libffi";
     systems  = systems "libffi"};
    {name     = "liblz4";
     project  = "liblz4";
     authors  = ["Yann Collet"];
     license  = ["GPL-2.0-only"; "BSD-2-Clause"];
     homepage = "http://lz4.org";
     systems  = systems ~pkgconf:"liblz4" "lz4"};
    (* XXX mbedtls: x86_64 only (no i686 variant).
       No Cygwin support: available: os = "win32" & os-distribution != "cygwin"
       Cygwin depext is commented out in the existing opam file.
       The current model doesn't express available constraints beyond os = "win32". *)
    {name     = "mbedtls";
     project  = "libmbedtls";
     authors  = ["Mbedtls contributors"];
     license  = ["Apache-2.0"];
     homepage = "https://www.trustedfirmware.org/projects/mbed-tls/";
     systems  = [
       `X86_64 (~msys2:(Some ("mbedtls", Pkgconf "mbedtls")), ~cygwin:None);
     ]};
    (* XXX ncurses: probes different pkgconf module names per distribution:
         "ncurses" on Cygwin, "ncursesw" on MSYS2.
       Current model only supports a single Pkgconf module name. *)
    {name     = "ncurses";
     project  = "ncurses";
     authors  = ["GNU Project"];
     license  = ["MIT"];
     homepage = "https://www.gnu.org/software/ncurses/";
     systems  = [
       `X86_64 (~msys2:(Some ("ncurses", Pkgconf "ncursesw")), ~cygwin:(Some ("ncurses", Pkgconf "ncurses")));
       `I686 (~msys2:(Some ("ncurses", Pkgconf "ncursesw")), ~cygwin:(Some ("ncurses", Pkgconf "ncurses")));
     ]};
    (* XXX nettle: existing opam file has wrong homepage (gnutls.org) and
       wrong authors (GnuTLS authors). These are copied from the existing file as-is. *)
    {name     = "nettle";
     project  = "nettle";
     authors  = ["Nikos Mavrogiannopoulos"; "Simon Josefsson"]; (* FIXME These are GnuTLS authors, not Nettle *)
     license  = ["LGPL-2.1-or-later"];
     homepage = "https://www.gnutls.org"; (* FIXME Should be https://www.lysator.liu.se/~nisse/nettle/ *)
     systems  = systems "nettle"};
    {name     = "openssl";
     project  = "libssl";
     authors  = ["The OpenSSL Project"];
     license  = ["Apache-1.0"];
     homepage = "https://www.openssl.org";
     systems  = systems "openssl"};
    {name     = "pcre";
     project  = "libpcre";
     authors  = ["Philip Hazel"; "Zoltan Herczeg"];
     license  = ["BSD-3-Clause"];
     homepage = "https://www.pcre.org/";
     systems  = [
       `X86_64 (~msys2:(Some ("pcre", Pkgconf "libpcre")), ~cygwin:(Some ("pcre", Pkgconf "libpcre")));
       `I686 (~msys2:None, ~cygwin:(Some ("pcre", Pkgconf "libpcre")));
     ]};
    {name     = "pcre2";
     project  = "libpcre2";
     authors  = ["Philip Hazel"; "Zoltan Herczeg"];
     license  = ["BSD-3-Clause"];
     homepage = "https://www.pcre.org/";
     systems  = systems ~pkgconf:"libpcre2-8" "pcre2" (*[
       `X86_64 (~msys2:(Some ("pcre2", Pkgconf "libpcre2-8")), ~cygwin:(Some ("pcre2", Pkgconf "libpcre2-8")));
       `I686 (~msys2:(Some ("pcre2", Pkgconf "libpcre2-8")), ~cygwin:(Some ("pcre2", Pkgconf "libpcre2-8")));
     ]*)};
    {name     = "pkgconf";
     project  = "pkgconf";
     authors  = ["Ariadne Conill et al"];
     license  = ["ISC"];
     homepage = "http://pkgconf.org";
     systems  = [
       (* XXX Likewise, this would be better to install on Cygwin and run anyway? *)
       `X86_64 (~msys2:(Some ("pkgconf", Version "x86_64-w64-mingw32-pkgconf")), ~cygwin:None);
       `I686 (~msys2:(Some ("pkgconf", Version "i686-w64-mingw32-pkgconf")), ~cygwin:None);
     ]};
    {name     = "postgresql";
     project  = "postgresql";
     authors  = ["Markus Mottl"]; (* FIXME This is the opam package author *)
     license  = ["blessing"];
     homepage = "http://www.postgresql.org";
     systems  = [
       `X86_64 (~msys2:(Some ("postgresql", Pkgconf "libpq")), ~cygwin:(Some ("postgresql", Pkgconf "libpq")));
       `I686 (~msys2:None, ~cygwin:(Some ("postgresql", Pkgconf "libpq")));
     ]};
    {name     = "sdl2"; (* FIXME SDL2 *)
     project  = "sdl2"; (* FIXME SDL2 *)
     authors  = ["Sam Lantinga"];
     license  = ["Zlib"];
     homepage = "http://libsdl.org/"; (* FIXME https://libsdl.org/ *)
     systems  = systems ~pkgconf:"sdl2" "SDL2"};
    {name     = "sdl2-image";
     project  = "sdl2-image";
     authors  = ["Sam Lantinga"];
     license  = ["Zlib"];
     homepage = "http://www.libsdl.org/projects/SDL_image/";
     systems  = [
       `X86_64 (~msys2:(Some ("SDL2_image", Pkgconf "SDL2_image")), ~cygwin:(Some ("SDL2_image", Pkgconf "SDL2_image")));
       `I686 (~msys2:None, ~cygwin:(Some ("SDL2_image", Pkgconf "SDL2_image")));
     ]};
    {name     = "sdl2-mixer";
     project  = "sdl2-mixer";
     authors  = ["Sam Lantinga"];
     license  = ["Zlib"];
     homepage = "http://www.libsdl.org/projects/SDL_mixer/";
     systems  = systems "SDL2_mixer"};
    {name     = "sdl2-net";
     project  = "sdl2-net";
     authors  = ["Sam Lantinga"];
     license  = ["Zlib"];
     homepage = "http://www.libsdl.org/projects/SDL_net/";
     systems  = systems "SDL2_net"};
    {name     = "sdl2-ttf";
     project  = "sdl2-ttf";
     authors  = ["Sam Lantinga"];
     license  = ["Zlib"];
     homepage = "http://www.libsdl.org/projects/SDL_ttf/";
     systems  = systems "SDL2_ttf"};
    {name     = "sqlite3";
     project  = "sqlite3";
     authors  = ["D. Richard Hipp"; "Dan Kennedy"; "Joe Mistachkin"];
     license  = ["blessing"];
     homepage = "http://www.sqlite3.org"; (* FIXME https://sqlite.org/ *)
     systems  = systems "sqlite3"};
    {name     = "zlib";
     project  = "zlib";
     authors  = ["Jean-loup Gailly"; "Mark Adler"];
     license  = ["zlib"];
     homepage = "http://www.zlib.net/"; (* FIXME https://www.zlib.net/ *)
     systems  = systems "zlib"};
    {name     = "zstd";
     project  = "libzstd";
     authors  = ["Facebook"];
     license  = ["BSD-3-Clause"];
     homepage = "http://zstd.net"; (* FIXME https://facebook.github.io/zstd/ *)
     systems  = systems ~pkgconf:"libzstd" "zstd"};
  ] in
  let process (names, packages) {name; project; authors; license; homepage; systems} =
    if OpamStd.String.Set.mem name names then
      assert false (* XXX Error handling! *)
    else
      let names = OpamStd.String.Set.add name names in
      let _, packages = List.fold_left (fun (systems, packages) system ->
        let synopsis, arch, system, (~msys2, ~cygwin) =
          match system with
          | `X86_64 environments ->
              "x86_64 mingw-w64 (64-bit x86_64)", "x86_64", X86_64, environments
          | `I686 environments ->
              "i686 mingw-w64 (32-bit x86)", "i686", I686, environments
        in
        if SystemSet.mem system systems then
          assert false (* XXX A bit of error handling here... *)
        else
          let systems = SystemSet.add system systems in
          let module OPAM = OpamFile.OPAM in
          let package = "conf-mingw-w64-" ^ name ^ "-" ^ arch in
          let build = OpamTypes.FIdent ([], OpamVariable.of_string "build", None) in
          let os = OpamTypes.FIdent ([], OpamVariable.of_string "os", None) in
          let os_distribution = OpamTypes.FIdent ([], OpamVariable.of_string "os-distribution", None) in
          let os_win32 = OpamTypes.FOp (os, `Eq, FString "win32") in
          let dist_msys2 = OpamTypes.FOp (os_distribution, `Eq, FString "msys2") in
          let dist_cygwin = OpamTypes.FOp (os_distribution, `Eq, FString "cygwin") in
          let build_os_msys2 = OpamTypes.(FAnd (build, FAnd (os_win32, dist_msys2))) in
          let os_msys2 = OpamTypes.FAnd (os_win32, dist_msys2) in
          let os_cygwin = OpamTypes.FAnd (os_win32, dist_cygwin) in
          let depexts =
            match msys2 with
            | None
            | Some ("", _) -> []
            | Some (package, _) ->
                [OpamSysPkg.(Set.singleton (of_string ("mingw-w64-" ^ arch ^ "-" ^ package))), os_msys2]
          in
          let depexts =
            match cygwin with
            | None
            | Some ("", _) -> depexts
            | Some (package, _) ->
                (OpamSysPkg.(Set.singleton (of_string ("mingw64-" ^ arch ^ "-" ^ package))), os_cygwin) :: depexts
          in
          let available =
            if msys2 = None then
              os_cygwin
            else if cygwin = None then
              os_msys2
            else
              os_win32
          in
          let build, depends =
            let arg ?filter s = OpamTypes.CString s, filter in
            let pkg name filter : OpamTypes.filtered_formula = OpamFormula.Atom (OpamPackage.Name.of_string name, OpamTypes.Atom (OpamTypes.Filter filter)) in
            let depends =
              let root = "conf-mingw-w64-gcc-" ^ arch in
              if root <> package && name <> "pkgconf" then (* XXX If another special case comes up, add an attribute *)
                [pkg root build]
              else
                []
            in
            let depends =
              match msys2, cygwin with
              | Some (_, Pkgconf _), _
              | _, Some (_, Pkgconf _) ->
                (pkg "conf-pkg-config" build) :: depends
              | _ ->
                  depends
            in
            let depends : OpamTypes.filtered_formula list =
              if msys2 = None then
                depends
              else
                (pkg "msys2" build_os_msys2) :: (pkg (if system = X86_64 then "msys2-mingw64" else "msys2-mingw32") os_msys2) :: depends
            in
            let build =
              let f = function
              | Some (_, No_test)
              | None -> None
              | Some (_, probe) -> Some probe
              in
              match f msys2, f cygwin with
              | Some (Pkgconf msys2), Some (Pkgconf cygwin) ->
                  let command =
                    if msys2 = cygwin then
                      [arg msys2]
                    else
                      [arg ~filter:dist_cygwin cygwin; arg ~filter:dist_msys2 msys2]
                  in
                  let personality =
                    match system with
                    | X86_64 -> "--personality=x86_64-w64-mingw32"
                    | I686 -> "--personality=i686-w64-mingw32"
                  in
                    [(arg "pkgconf" :: arg ~filter:dist_cygwin personality :: command), None]
              | Some (Pkgconf name), None ->
                  [[arg "pkgconf"; arg name], (Option.map (Fun.const dist_msys2) cygwin)]
              | None, Some (Pkgconf name) ->
                  let personality =
                    match system with
                    | X86_64 -> "--personality=x86_64-w64-mingw32"
                    | I686 -> "--personality=i686-w64-mingw32"
                  in
                    [[arg "pkgconf"; arg ~filter:dist_cygwin personality; arg name], None]
              | Some (Version msys2), Some (Version cygwin) ->
                  if msys2 = cygwin then
                    [[arg msys2; arg "--version"], None]
                  else
                    [[arg ~filter:dist_cygwin cygwin; arg ~filter:dist_msys2 msys2; arg "--version"], None]
              | Some (Version cmd), None
              | None, Some (Version cmd) ->
                  (* FIXME This should have a filter if the other environment _is_ installed but doesn't support the probe
                           (although that's weird - maybe it should more be an assertion the the other environment is being supported at all) *)
                  [[arg cmd; arg "--version"], None]
              | None, None ->
                  []
              | _, _ ->
                  assert false
            in
            build, OpamFormula.ands depends
          in
          let opam =
            OPAM.empty
            |> OPAM.with_name (OpamPackage.Name.of_string package)
            |> OPAM.with_version base_version
            |> OPAM.with_synopsis ("Virtual package for " ^ project ^ " on " ^ synopsis)
            |> OPAM.with_descr_body ("Ensures the " ^ arch ^ " version of " ^ project ^ " for the mingw-w64 project is available")
            |> OPAM.with_maintainer ["David Allsopp <dra27@dra27.uk>"] (* FIXME Update! *)
            |> OPAM.with_author authors
            |> OPAM.with_license license
            |> OPAM.with_homepage [homepage]
            |> OPAM.with_bug_reports ["https://github.com/ocaml/opam-repository/issues"]
            |> OPAM.with_flags [OpamTypes.Pkgflag_Conf]
            |> OPAM.with_available available
            |> OPAM.with_build build
            |> OPAM.with_depends (canonicalise depends)
            |> OPAM.with_depexts depexts
          in
          let nv = OpamPackage.(create (Name.of_string package) base_version) in
          if not (OpamPackage.Set.mem nv all_packages) then
            Printf.eprintf "WARNING! %s not found\n" package; (* XXX Better handling *)
          systems, OpamStd.String.Map.add package opam packages) (SystemSet.empty, packages) systems
      in
      names, packages
  in
  snd (List.fold_left process (OpamStd.String.Set.empty, OpamStd.String.Map.empty) depexts)

(* XXX Ultimately do this by iterating over the list of depexts *)
let process package ~prefix:_ ~opam =
  let name = OpamPackage.name_to_string package in
  if String.starts_with ~prefix:"conf-mingw-w64-" name then
    if OpamPackage.version_to_string package <> "1" then
      assert false (* XXX Report error! *)
    else
      match OpamStd.String.Map.find name depexts with
      | opam -> opam
      | exception Not_found -> Printf.printf "UNKNOWN %s\n%!" name; opam
  else
    opam

let _ =
  Printf.printf "Found %d packages\n" (OpamPackage.Set.cardinal all_packages);
  iter_packages process
