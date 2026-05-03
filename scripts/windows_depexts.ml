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

type systems = [ `X86_64 of msys2:depext_info * cygwin:depext_info
               | `I686 of msys2:depext_info * cygwin:depext_info ] list

type member =
  | Define of {
      name: string;
      project: string;
      authors: string list;
      license: string list;
      homepage: string;
      systems: systems;
    }
  | Ref of string

type conf_def = {
  name: string;
  project: string;
  authors: string list;
  license: string list;
  homepage: string;
  members: (project:string -> authors:string list -> license:string list -> homepage:string -> member) list;
}

let base_version = OpamPackage.Version.of_string "1"

(* XXX BEGIN Claude *)
(* Compare two opam files and return a list of field names that differ. *)
let diff_opam (a : OpamFile.OPAM.t) (b : OpamFile.OPAM.t) : string list =
  let check name get eq acc =
    if eq (get a) (get b) then acc else name :: acc
  in
  let ( = ) = Stdlib.( = ) in
  let eq_poly x y = x = y in
  List.rev (
    []
    |> check "opam-version"   OpamFile.OPAM.opam_version   eq_poly
    |> check "name"           OpamFile.OPAM.name_opt       eq_poly
    |> check "version"        OpamFile.OPAM.version_opt    eq_poly
    |> check "synopsis"       OpamFile.OPAM.synopsis       eq_poly
    |> check "description"    OpamFile.OPAM.descr_body     eq_poly
    |> check "maintainer"     OpamFile.OPAM.maintainer     eq_poly
    |> check "authors"        OpamFile.OPAM.author         eq_poly
    |> check "license"        OpamFile.OPAM.license        eq_poly
    |> check "tags"           OpamFile.OPAM.tags           eq_poly
    |> check "homepage"       OpamFile.OPAM.homepage       eq_poly
    |> check "doc"            OpamFile.OPAM.doc            eq_poly
    |> check "bug-reports"    OpamFile.OPAM.bug_reports    eq_poly
    |> check "dev-repo"       OpamFile.OPAM.dev_repo       eq_poly
    |> check "depends"        OpamFile.OPAM.depends        eq_poly
    |> check "depopts"        OpamFile.OPAM.depopts        eq_poly
    |> check "conflicts"      OpamFile.OPAM.conflicts      eq_poly
    |> check "conflict-class" OpamFile.OPAM.conflict_class eq_poly
    |> check "available"      OpamFile.OPAM.available      eq_poly
    |> check "flags"          OpamFile.OPAM.flags          eq_poly
    |> check "build"          OpamFile.OPAM.build          eq_poly
    |> check "install"        OpamFile.OPAM.install        eq_poly
    |> check "remove"         OpamFile.OPAM.remove         eq_poly
    |> check "run-test"       OpamFile.OPAM.deprecated_build_test eq_poly
    |> check "build-doc"      OpamFile.OPAM.deprecated_build_doc  eq_poly
    |> check "substs"         OpamFile.OPAM.substs         eq_poly
    |> check "patches"        OpamFile.OPAM.patches        eq_poly
    |> check "build-env"      OpamFile.OPAM.build_env      eq_poly
    |> check "features"       OpamFile.OPAM.features       eq_poly
    |> check "messages"       OpamFile.OPAM.messages       eq_poly
    |> check "post-messages"  OpamFile.OPAM.post_messages  eq_poly
    |> check "depexts"        OpamFile.OPAM.depexts        eq_poly
    |> check "libraries"      OpamFile.OPAM.libraries      eq_poly
    |> check "syntax"         OpamFile.OPAM.syntax         eq_poly
    |> check "pin-depends"    OpamFile.OPAM.pin_depends    eq_poly
    |> check "extra-sources"  OpamFile.OPAM.extra_sources  eq_poly
    |> check "extra-files"    OpamFile.OPAM.extra_files    eq_poly
    |> check "url"            OpamFile.OPAM.url            eq_poly
    |> check "descr"          OpamFile.OPAM.descr          eq_poly
  )

(* Pretty print *)
let print_diff a b =
  match diff_opam a b with
  | [] -> print_endline "opam files are equivalent on all compared fields"
  | fields ->
      print_endline "Differing fields:";
      List.iter (fun f -> Printf.printf "  - %s\n" f) fields

let string_of_relop = function
  | `Eq  -> "="
  | `Neq -> "!="
  | `Geq -> ">="
  | `Gt  -> ">"
  | `Leq -> "<="
  | `Lt  -> "<"

let rec dump_filter = function
  | OpamTypes.FBool b          -> Printf.sprintf "%b" b
  | OpamTypes.FString s        -> Printf.sprintf "%S" s
  | OpamTypes.FIdent (_, v, _) -> OpamVariable.to_string v
  | OpamTypes.FOp (a, op, b)   -> Printf.sprintf "(%s %s %s)"
                                    (dump_filter a) (string_of_relop op) (dump_filter b)
  | OpamTypes.FAnd (a, b)      -> Printf.sprintf "(%s & %s)" (dump_filter a) (dump_filter b)
  | OpamTypes.FOr  (a, b)      -> Printf.sprintf "(%s | %s)" (dump_filter a) (dump_filter b)
  | OpamTypes.FNot f           -> Printf.sprintf "!%s" (dump_filter f)
  | OpamTypes.FDefined f       -> Printf.sprintf "?%s" (dump_filter f)
  | OpamTypes.FUndef f         -> Printf.sprintf "undef(%s)" (dump_filter f)

let dump_condition_atom = function
  | OpamTypes.Constraint (op, f) ->
      Printf.sprintf "%s %s" (string_of_relop op) (dump_filter f)
  | OpamTypes.Filter f ->
      dump_filter f

let rec dump_formula dump_atom = function
  | OpamFormula.Empty      -> "Empty"
  | OpamFormula.Atom a     -> dump_atom a
  | OpamFormula.Block f    -> Printf.sprintf "Block(%s)" (dump_formula dump_atom f)
  | OpamFormula.And (a, b) -> Printf.sprintf "And(%s, %s)"
                                (dump_formula dump_atom a) (dump_formula dump_atom b)
  | OpamFormula.Or  (a, b) -> Printf.sprintf "Or(%s, %s)"
                                (dump_formula dump_atom a) (dump_formula dump_atom b)

let dump_filtered_formula (ff : OpamTypes.filtered_formula) =
  dump_formula (fun (n, c) ->
    Printf.sprintf "%s{%s}"
      (OpamPackage.Name.to_string n)
      (dump_formula dump_condition_atom c))
    ff
(* XXX END Claude *)

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

let compare_filter_or_constraint l r =
  match l, r with
  | OpamTypes.Filter _, OpamTypes.Constraint _ -> -1
  | OpamTypes.Constraint _, OpamTypes.Filter _ -> 1
  | OpamTypes.Filter l, OpamTypes.Filter r ->
      Stdlib.compare l r
  | OpamTypes.Constraint l, OpamTypes.Constraint r ->
      Pair.compare OpamFormula.compare_relop Stdlib.compare l r

let compare_filtered_formula =
  OpamFormula.compare_formula
    (Pair.compare OpamPackage.Name.compare
                  (OpamFormula.compare_formula compare_filter_or_constraint))

let rec normalise_filtered ff =
  let rec collect_and = function
    | OpamFormula.And (a, b) -> collect_and a @ collect_and b
    | OpamFormula.Block f    -> collect_and f
    | f                      -> [f]
  in
  let rec collect_or = function
    | OpamFormula.Or (a, b)  -> collect_or a @ collect_or b
    | OpamFormula.Block f    -> collect_or f
    | f                      -> [f]
  in
  match ff with
  | OpamFormula.Empty   -> OpamFormula.Empty
  | OpamFormula.Atom _ as a -> a
  | OpamFormula.Block f -> normalise_filtered f
  | OpamFormula.And _   ->
      collect_and ff
      |> List.map normalise_filtered
      |> List.sort compare_filtered_formula
      |> OpamFormula.ands
  | OpamFormula.Or _    ->
      collect_or ff
      |> List.map normalise_filtered
      |> List.sort compare_filtered_formula
      |> OpamFormula.ors

let formulae_equivalent a b =
  compare_filtered_formula (normalise_filtered a) (normalise_filtered b) = 0

let pkg name filter : OpamTypes.filtered_formula =
  OpamTypes.Atom (OpamPackage.Name.of_string name, OpamTypes.Atom (OpamTypes.Filter filter))

let filter_holds_on_windows filter =
  let env os_distribution v =
    match OpamVariable.(to_string (Full.variable v)) with
    | "os" -> Some (OpamTypes.S "win32")
    | "os-distribution" -> Some (OpamTypes.S os_distribution)
    | _ -> None
  in
  OpamFilter.eval_to_bool ~default:true (env "msys2") filter
  || OpamFilter.eval_to_bool ~default:true (env "cygwin") filter

let depexts, confs, define_index =
  let define ?project ?authors ?license ?homepage
             ?template ?pkgconf ?depext
             name
             ~project:conf_project ~authors:conf_authors ~license:conf_license ~homepage:conf_homepage =
    let depext = Option.value depext ~default:name in
    let pkgconf = Option.value ~default:depext pkgconf in
    let template = Option.value ~default:(`Cygwin depext) template in
    let systems =
      match template with
      | `Cygwin cygwin_depext -> [
          `X86_64 (~msys2:(Some (depext, Pkgconf pkgconf)),
                   ~cygwin:(Some (cygwin_depext, Pkgconf pkgconf)));
          `I686 (~msys2:(Some (depext, Pkgconf pkgconf)),
                 ~cygwin:(Some (cygwin_depext, Pkgconf pkgconf)))]
      | `MSYS2_only -> [
          `X86_64 (~msys2:(Some (depext, Pkgconf pkgconf)), ~cygwin:None);
          `I686 (~msys2:(Some (depext, Pkgconf pkgconf)), ~cygwin:None)]
      | `Manual systems -> systems
    in
    Define {name;
            project = Option.value project ~default:conf_project;
            authors = Option.value authors ~default:conf_authors;
            license = Option.value license ~default:conf_license;
            homepage = Option.value homepage ~default:conf_homepage;
            systems}
  in
  let conf_defs = [
    {name      = "allegro5";
     project   = "allegro5";
     authors   = ["The Allegro authors"];
     license   = ["Giftware"];
     homepage  = "https://liballeg.org";
     members   = [define ~template:`MSYS2_only ~pkgconf:"allegro-5" ~depext:"allegro" "allegro5"]};
    {name      = "ao";
     project   = "libao";
     authors   = ["libao dev team"];
     license   = ["GPL-2.0-only"];
     homepage  = "https://www.xiph.org/ao/";
     members   = [define ~pkgconf:"ao" ~depext:"libao" "ao"]};
    {name      = "bzip2";
     project   = "bzip2";
     authors   = ["Julian Seward"];
     license   = ["bzip2-1.0.6"];
     homepage  = "https://sourceware.org/bzip2/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("bzip2", Pkgconf "bzip2")), ~cygwin:(Some ("bzip2", No_test)));
       `I686 (~msys2:(Some ("bzip2", Pkgconf "bzip2")), ~cygwin:(Some ("bzip2", No_test)));
     ]) "bzip2"]};
    {name      = "cairo";
     project   = "cairo";
     authors   = ["Keith Packard"; "Carl Worth"; "Behdad Esfahbod"];
     license   = ["LGPL-2.1-only"; "MPL-1.1"];
     homepage  = "http://cairographics.org/";
     members   = [define "cairo"]};
    {name      = "libcurl";
     project   = "libcurl";
     authors   = ["Daniel Stenberg"];
     license   = ["curl"];
     homepage  = "http://curl.haxx.se/";
     members   = [define ~pkgconf:"libcurl" "curl"]};
    {name      = "freeglut";
     project   = "FreeGLUT";
     authors   = ["Pawel W. Olszta"; "Andreas Umbach"; "Steve Baker"; "John F. Fay"; "John Tsiombikas"; "Diederick C. Niehorster"];
     license   = ["X11"];
     homepage  = "https://freeglut.sourceforge.net/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("freeglut", Pkgconf "freeglut")), ~cygwin:(Some ("freeglut", No_test)));
       `I686 (~msys2:(Some ("freeglut", Pkgconf "freeglut")), ~cygwin:(Some ("freeglut", No_test)));
     ]) "freeglut"]};
    {name      = "freetype";
     project   = "freetype";
     authors   = ["Christophe Troestler <Christophe.Troestler@umons.ac.be>"]; (* FIXME These are opam pkg authors, not freetype authors *)
     license   = ["GPL-1.0-or-later"];
     homepage  = "http://www.freetype.org";
     members   = [define ~template:(`Cygwin "freetype2") ~pkgconf:"freetype2" "freetype"]};
    {name      = "glade";
     project   = "glade";
     authors   = ["The Glade Authors"];
     license   = ["LGPL-2.1-or-later"];
     homepage  = "https://glade.gnome.org/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("libglade", Pkgconf "libglade-2.0")), ~cygwin:(Some ("libglade2.0", Pkgconf "libglade-2.0")));
       `I686 (~msys2:None, ~cygwin:(Some ("libglade2.0", Pkgconf "libglade-2.0")));
     ]) "glade"]};
    {name      = "gmp";
     project   = "libgmp";
     authors   = ["Torbjörn Granlund et al"];
     license   = ["GPL-1.0-or-later"];
     homepage  = "http://gmplib.org/";
     members   = [define "gmp"]};
    {name      = "g++"; (* XXX conf-c++ as well *)
     project   = "g++";
     authors   = ["The GCC Developers"];
     license   = ["GPL-3.0-or-later"];
     homepage  = "https://gcc.gnu.org/";
     members   = [define ~homepage:"https://www.mingw-w64.org"
                         ~template:(`Manual [
       `X86_64 (~msys2:(Some ("", Version "x86_64-w64-mingw32-g++")), ~cygwin:(Some ("gcc-g++", Version "x86_64-w64-mingw32-g++")));
       `I686 (~msys2:(Some ("", Version "i686-w64-mingw32-g++")), ~cygwin:(Some ("gcc-g++", Version "i686-w64-mingw32-g++")));
     ]) "g++"]};
    {name      = "gcc";
     project   = "GCC";
     authors   = ["The GCC Developers"];
     license   = ["GPL-3.0-or-later"];
     homepage  = "https://gcc.gnu.org/";
     members   = [define ~homepage:"https://www.mingw-w64.org"
                         ~template:(`Manual [
       `X86_64 (~msys2:(Some ("gcc", Version "x86_64-w64-mingw32-gcc")), ~cygwin:(Some ("gcc-core", Version "x86_64-w64-mingw32-gcc")));
       `I686 (~msys2:(Some ("gcc", Version "i686-w64-mingw32-gcc")), ~cygwin:(Some ("gcc-core", Version "i686-w64-mingw32-gcc")));
     ]) "gcc"]};
    {name      = "gnomecanvas";
     project   = "gnomecanvas";
     authors   = ["The GNOME Project"];
     license   = ["LGPL-2.1-or-later"];
     homepage  = "https://developer.gnome.org/libgnomecanvas/2.30/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("libgnomecanvas", Pkgconf "libgnomecanvas-2.0")), ~cygwin:(Some ("libgnomecanvas2", Pkgconf "libgnomecanvas-2.0")));
       `I686 (~msys2:None, ~cygwin:(Some ("libgnomecanvas2", Pkgconf "libgnomecanvas-2.0")));
     ]) "gnomecanvas"]};
    (* XXX It's possible I advised mte to do it this way, but why isn't this does a second atom on the gnutls package? *)
    (* nettle's authors and homepage match the conf-package's GnuTLS values
       in the existing data (FIXME: nettle's authors should be Niels Möller
       and homepage should be lysator.liu.se/~nisse/nettle/). Only project
       differs. *)
    {name      = "gnutls";
     project   = "GnuTLS";
     authors   = ["Nikos Mavrogiannopoulos"; "Simon Josefsson"];
     license   = ["LGPL-2.1-or-later"];
     homepage  = "https://www.gnutls.org";
     members   = [
       define ~project:"nettle" "nettle";
       define "gnutls";
     ]};
    {name      = "gtk2";
     project   = "gtk2";
     authors   = ["The GNOME Project"];
     license   = ["LGPL-2.1-or-later"];
     homepage  = "https://gtk.org/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("gtk2", Pkgconf "gtk+-2.0")), ~cygwin:(Some ("gtk2.0", Pkgconf "gtk+-2.0")));
       `I686 (~msys2:None, ~cygwin:(Some ("gtk2.0", Pkgconf "gtk+-2.0")));
     ]) "gtk2"]};
    {name      = "gtk3";
     project   = "GTK+ 3";
     authors   = ["The GTK Toolkit"];
     license   = ["LGPL-2.1-or-later"];
     homepage  = "https://www.gtk.org/";
     members   = [define ~pkgconf:"gtk+-3.0" "gtk3"]};
    {name      = "gtksourceview3";
     project   = "gtksourceview3";
     authors   = ["The gtksourceview programmers"];
     license   = ["LGPL-2.1-or-later"];
     homepage  = "https://projects.gnome.org/gtksourceview/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("gtksourceview3", Pkgconf "gtksourceview-3.0")), ~cygwin:(Some ("gtksourceview3.0", Pkgconf "gtksourceview-3.0")));
       `I686 (~msys2:None, ~cygwin:(Some ("gtksourceview3.0", Pkgconf "gtksourceview-3.0")));
     ]) "gtksourceview3"]};
    {name      = "libevent";
     project   = "libevent";
     authors   = ["Libevent dev team"];
     license   = ["BSD-3-clause"];
     homepage  = "https://libevent.org";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("libevent", Pkgconf "libevent")), ~cygwin:(Some ("libevent", Pkgconf "libevent")));
       `I686 (~msys2:None, ~cygwin:(Some ("libevent", Pkgconf "libevent")));
     ]) "libevent"]};
    {name      = "libffi";
     project   = "libffi";
     authors   = ["Anthony Green"];
     license   = ["MIT"];
     homepage  = "https://sourceware.org/libffi";
     members   = [define "libffi"]};
    {name      = "liblz4";
     project   = "liblz4";
     authors   = ["Yann Collet"];
     license   = ["GPL-2.0-only"; "BSD-2-Clause"];
     homepage  = "http://lz4.org";
     members   = [define ~pkgconf:"liblz4" ~depext:"lz4" "liblz4"]};
    {name      = "mbedtls";
     project   = "libmbedtls";
     authors   = ["The Mbed TLS Contributors"];
     license   = ["Apache-2.0 OR GPL-2.0-or-later"];
     homepage  = "https://www.trustedfirmware.org/projects/mbed-tls/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("mbedtls", Pkgconf "mbedtls")), ~cygwin:None);
     ]) "mbedtls"]};
    {name      = "ncurses";
     project   = "ncurses";
     authors   = ["GNU Project"];
     license   = ["MIT"];
     homepage  = "https://www.gnu.org/software/ncurses/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("ncurses", Pkgconf "ncursesw")), ~cygwin:(Some ("ncurses", Pkgconf "ncurses")));
       `I686 (~msys2:(Some ("ncurses", Pkgconf "ncursesw")), ~cygwin:(Some ("ncurses", Pkgconf "ncurses")));
     ]) "ncurses"]};
    {name      = "libssl";
     project   = "libssl";
     authors   = ["The OpenSSL Project"];
     license   = ["Apache-1.0"];
     homepage  = "https://www.openssl.org";
     members   = [define "openssl"]};
    {name      = "libpcre";
     project   = "libpcre";
     authors   = ["Philip Hazel"; "Zoltan Herczeg"];
     license   = ["BSD-3-Clause"];
     homepage  = "https://www.pcre.org/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("pcre", Pkgconf "libpcre")), ~cygwin:(Some ("pcre", Pkgconf "libpcre")));
       `I686 (~msys2:None, ~cygwin:(Some ("pcre", Pkgconf "libpcre")));
     ]) "pcre"]};
    {name      = "libpcre2-8";
     project   = "libpcre2";
     authors   = ["Philip Hazel"; "Zoltan Herczeg"];
     license   = ["BSD-3-Clause"];
     homepage  = "https://www.pcre.org/";
     members   = [define ~pkgconf:"libpcre2-8" "pcre2"]};
    {name      = "pkg-config";
     project   = "pkg-config";
     authors   = ["James Henstridge"; "Tim Janik"; "Havoc Pennington"; "Scott Remnant"];
     license   = ["GPL-2.0-or-later"];
     homepage  = "http://www.freedesktop.org/wiki/Software/pkg-config/"; (* XXX https:// *)
     members   = [define
                    ~project:"pkgconf"
                    ~authors:["Ariadne Conill et al"]
                    ~license:["ISC"]
                    ~homepage:"http://pkgconf.org"
                    ~template:(`Manual [
       (* XXX Likewise, this would be better to install on Cygwin and run anyway? *)
       `X86_64 (~msys2:(Some ("pkgconf", Version "x86_64-w64-mingw32-pkgconf")), ~cygwin:None);
       `I686 (~msys2:(Some ("pkgconf", Version "i686-w64-mingw32-pkgconf")), ~cygwin:None);
     ]) "pkgconf"]};
    {name      = "postgresql";
     project   = "postgresql";
     authors   = ["PostgreSQL Global Development Group"];
     license   = ["PostgreSQL"];
     homepage  = "https://www.postgresql.org";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("postgresql", Pkgconf "libpq")), ~cygwin:(Some ("postgresql", Pkgconf "libpq")));
       `I686 (~msys2:None, ~cygwin:(Some ("postgresql", Pkgconf "libpq")));
     ]) "postgresql"]};
    {name      = "sdl2"; (* FIXME SDL2 *)
     project   = "sdl2"; (* FIXME SDL2 *)
     authors   = ["Sam Lantinga"];
     license   = ["Zlib"];
     homepage  = "http://libsdl.org/"; (* FIXME https://libsdl.org/ *)
     members   = [define ~pkgconf:"sdl2" ~depext:"SDL2" "sdl2"]};
    {name      = "sdl2-image";
     project   = "sdl2-image";
     authors   = ["Sam Lantinga"];
     license   = ["Zlib"];
     homepage  = "http://www.libsdl.org/projects/SDL_image/";
     members   = [define ~template:(`Manual [
       `X86_64 (~msys2:(Some ("SDL2_image", Pkgconf "SDL2_image")), ~cygwin:(Some ("SDL2_image", Pkgconf "SDL2_image")));
       `I686 (~msys2:None, ~cygwin:(Some ("SDL2_image", Pkgconf "SDL2_image")));
     ]) "sdl2-image"]};
    {name      = "sdl2-mixer";
     project   = "sdl2-mixer";
     authors   = ["Sam Lantinga"];
     license   = ["Zlib"];
     homepage  = "http://www.libsdl.org/projects/SDL_mixer/";
     members   = [define ~depext:"SDL2_mixer" "sdl2-mixer"]};
    {name      = "sdl2-net";
     project   = "sdl2-net";
     authors   = ["Sam Lantinga"];
     license   = ["Zlib"];
     homepage  = "http://www.libsdl.org/projects/SDL_net/";
     members   = [define ~depext:"SDL2_net" "sdl2-net"]};
    {name      = "sdl2-ttf";
     project   = "sdl2-ttf";
     authors   = ["Sam Lantinga"];
     license   = ["Zlib"];
     homepage  = "http://www.libsdl.org/projects/SDL_ttf/";
     members   = [define ~depext:"SDL2_ttf" "sdl2-ttf"]};
    {name      = "sqlite3";
     project   = "sqlite3";
     authors   = ["D. Richard Hipp"; "Dan Kennedy"; "Joe Mistachkin"];
     license   = ["blessing"];
     homepage  = "http://www.sqlite3.org"; (* FIXME https://sqlite.org/ *)
     members   = [define "sqlite3"]};
    {name      = "zlib";
     project   = "zlib";
     authors   = ["Jean-loup Gailly"; "Mark Adler"];
     license   = ["zlib"];
     homepage  = "http://www.zlib.net/"; (* FIXME https://www.zlib.net/ *)
     members   = [define "zlib"]};
    {name      = "zstd";
     project   = "libzstd";
     authors   = ["Facebook"];
     license   = ["BSD-3-Clause"];
     homepage  = "http://zstd.net"; (* FIXME https://facebook.github.io/zstd/ *)
     members   = [define ~pkgconf:"libzstd" "zstd"]};
  ] in
  let process (names, packages, confs, define_index) ({name; project; authors; license; homepage; members} as conf_def) =
    let members = List.map (fun f -> f ~project ~authors ~license ~homepage) members in
    let names =
      List.fold_left (fun names m ->
        match m with
        | Ref _ -> names
        | Define {name; _} ->
            if OpamStd.String.Set.mem name names then
              assert false (* XXX Error handling! *)
            else
              OpamStd.String.Set.add name names) names members
    in
    let packages, define_index =
      List.fold_left (fun (packages, define_index) m ->
        match m with
        | Ref _ -> packages, define_index
        | Define {name; project; authors; license; homepage; systems} as define ->
            let define_index =
              OpamStd.String.Map.add name define define_index
            in
            let _, packages = List.fold_left (fun (seen_systems, packages) system ->
              let synopsis, arch, system, (~msys2, ~cygwin) =
                match system with
                | `X86_64 environments ->
                    "x86_64 mingw-w64 (64-bit x86_64)", "x86_64", X86_64, environments
                | `I686 environments ->
                    "i686 mingw-w64 (32-bit x86)", "i686", I686, environments
              in
              if SystemSet.mem system seen_systems then
                assert false (* XXX A bit of error handling here... *)
              else
                let seen_systems = SystemSet.add system seen_systems in
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
                        ((pkg "conf-pkg-config" build) :: depends)
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
                seen_systems, OpamStd.String.Map.add package opam packages) (SystemSet.empty, packages) systems
            in
            packages, define_index) (packages, define_index) members
    in
    let confs =
      let conf = "conf-" ^ name in
      if not (OpamPackage.Set.exists (fun nv -> OpamPackage.name_to_string nv = conf) all_packages) then
        Printf.eprintf "WARNING! %s not found\n" conf; (* XXX Better handling *)
      OpamStd.String.Map.add conf (conf_def, members) confs
    in
    names, packages, confs, define_index
  in
  let (_, depexts, confs, define_index) =
    List.fold_left process
      (OpamStd.String.Set.empty,
       OpamStd.String.Map.empty,
       OpamStd.String.Map.empty,
       OpamStd.String.Map.empty)
      conf_defs
  in
  depexts, confs, define_index

(* XXX Ultimately do this by iterating over the list of depexts *)
let process package ~prefix:_ ~opam =
  let name = OpamPackage.name_to_string package in
  if String.starts_with ~prefix:"conf-mingw-w64-" name then
    if OpamPackage.version_to_string package <> "1" then
      assert false (* XXX Report error! *)
    else
      match OpamStd.String.Map.find name depexts with
      | opam' ->
(*
          Printf.printf "** opam:\n%s\n** opam':\n%s\n" (print_filtered_formula (OpamFile.OPAM.depends opam)) (print_filtered_formula (OpamFile.OPAM.depends opam'));
          List.iter print_endline (diff_opam opam opam');
          Printf.printf "** opam:\n%s\n** opam':\n%s\n" (dump_filtered_formula (OpamFile.OPAM.depends opam)) (dump_filtered_formula (OpamFile.OPAM.depends opam'));
*)
          opam' (* XXX Technically speaking, something here is different, because the file is writing *)
      | exception Not_found -> Printf.printf "UNKNOWN %s\n%!" name; opam
  else if String.starts_with ~prefix:"conf-" name then
    match OpamStd.String.Map.find name confs with
    | ({name; authors; license; homepage; _}, members) ->
        let module OPAM = OpamFile.OPAM in
        (* Resolve members: Define stays as-is, Ref looks up in define_index. *)
        let defines =
          List.map (function
            | Define _ as d -> d
            | Ref ref_name ->
                match OpamStd.String.Map.find ref_name define_index with
                | Define _ as d -> d
                | Ref _ -> assert false (* index never holds Refs *)
                | exception Not_found ->
                    failwith (Printf.sprintf
                      "%s: Ref %S not found in define_index" name ref_name)
          ) members
        in
        let define_systems = function
          | Define {systems; _} -> systems
          | Ref _ -> assert false
        in
        let define_name = function
          | Define {name; _} -> name
          | Ref _ -> assert false
        in
        (* The head member drives per-arch host filters and the iteration
           order over architectures; every other member in the group must
           agree on which architectures are present and which of MSYS2 /
           Cygwin each supports for that to be sound. *)
        let () =
          let shape define =
            List.sort Stdlib.compare (
              List.map (function
                | `X86_64 (~msys2, ~cygwin) ->
                    `X86_64 (Option.is_some msys2, Option.is_some cygwin)
                | `I686 (~msys2, ~cygwin) ->
                    `I686 (Option.is_some msys2, Option.is_some cygwin)
              ) (define_systems define))
          in
          let head_shape = shape (List.hd defines) in
          List.iter (fun define ->
            if shape define <> head_shape then
              failwith (Printf.sprintf
                "%s: members %S and %S disagree on architectures or MSYS2/Cygwin support"
                name (define_name (List.hd defines)) (define_name define))
          ) defines
        in
        (* XXX Very duppy - also wondering if it would be better just to write these as code fragments
               and parse them?? *)
        let build = OpamTypes.FIdent ([], OpamVariable.of_string "build", None) in
        let os = OpamTypes.FIdent ([], OpamVariable.of_string "os", None) in
        let os_distribution = OpamTypes.FIdent ([], OpamVariable.of_string "os-distribution", None) in
        let os_win32 = OpamTypes.FOp (os, `Eq, FString "win32") in
        let dist_msys2 = OpamTypes.FOp (os_distribution, `Eq, FString "msys2") in
        let dist_cygwin = OpamTypes.FOp (os_distribution, `Eq, FString "cygwin") in
        let depends =
          if filter_holds_on_windows (OPAM.available opam) then
            let make_filter data =
              (* XXX Much clearer to use Filter.of_string ... *)
              let filter =
                match data with
                | (~msys2:(Some _), ~cygwin:(Some _)) ->
                    OpamTypes.FOr (dist_cygwin, dist_msys2)
                | (~msys2:(Some _), ~cygwin:None) ->
                    dist_msys2
                | (~msys2:None, ~cygwin:(Some _)) ->
                    dist_cygwin
                | (~msys2:None, ~cygwin:None) ->
                    assert false
              in
              OpamTypes.FAnd(os_win32, filter)
            in
            let convert arch pkg_arch data =
              let host_filter = make_filter data in
              let find_for_arch define =
                List.find_map (fun s ->
                  match arch, s with
                  | "x86_64", `X86_64 d -> Some d
                  | "x86_32", `I686 d -> Some d
                  | _ -> None
                ) (define_systems define)
              in
              let define_atoms =
                List.filter_map (fun define ->
                  match find_for_arch define with
                  | None -> None
                  | Some d ->
                      Some (pkg ("conf-mingw-w64-" ^ define_name define ^ "-" ^ pkg_arch) (make_filter d))
                ) defines
              in
              OpamFormula.ands (pkg ("host-arch-" ^ arch) host_filter :: define_atoms)
            in
            let f = function
            | `X86_64 data ->
                convert "x86_64" "x86_64" data
            | `I686 data ->
                convert "x86_32" "i686" data
            in
            let g l r =
              match l, r with
              | `I686 _, `X86_64 _ -> -1
              | `X86_64 _, `I686 _ -> 1
              | `X86_64 l, `X86_64 r
              | `I686 l, `I686 r -> Stdlib.compare l r
            in
            let regenerated =
              let depends =
                let depends =
                  List.map f (List.sort g (define_systems (List.hd defines)))
                in
                (* FIXME function is definitely crap, but it hints so's the representation! *)
                if List.exists (fun define ->
                     List.exists (function
                       | `I686 (~msys2:(Some (_, Pkgconf _)), ~cygwin:_)
                       | `I686 (~msys2:_, ~cygwin:(Some (_, Pkgconf _)))
                       | `X86_64 (~msys2:(Some (_, Pkgconf _)), ~cygwin:_)
                       | `X86_64 (~msys2:_, ~cygwin:(Some (_, Pkgconf _))) -> true
                       | _ -> false) (define_systems define)) defines then
                  [(pkg "conf-pkg-config" build); OpamFormula.Block (OpamFormula.ors depends)]
                else
                  [OpamFormula.ors (List.map (fun f -> OpamFormula.Block f) depends)]
              in
              canonicalise (OpamFormula.ands depends)
            in
            let original = OPAM.depends opam in
            if regenerated = original then
              regenerated
            else if formulae_equivalent regenerated original then begin
              Printf.eprintf
                "%s: existing depends is semantically equivalent; keeping original\n%!"
                name;
              original
            end else
              regenerated
          else
            let () = Printf.printf "NOT-WINDOWS: %s\n%!" name in
            OPAM.depends opam
        in
        opam
(* XXX Analysis needed
        |> OPAM.with_synopsis ("Virtual package for " ^ project)
*)
        (* TODO with_descr_body *)
        (* TODO with_maintainer <- only doable when the packages are totally covered by the metadata *)
        |> OPAM.with_author authors
        |> OPAM.with_license license
        |> OPAM.with_homepage [homepage]
        |> OPAM.with_bug_reports ["https://github.com/ocaml/opam-repository/issues"]
        (* XXX with_available ? *)
        (* TODO with_build *)
        |> OPAM.with_depends depends
        (* XXX with_depexts ? *)
    | exception Not_found ->
        opam
  else
    opam

let _ =
  Printf.printf "Found %d packages\n" (OpamPackage.Set.cardinal all_packages);
  iter_packages process
