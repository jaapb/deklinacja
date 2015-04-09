let cases = [ `Nom; `Gen; `Dat; `Acc; `Ins; `Loc; `Voc ];;
let nr_cases = List.length cases;;

let case_name_en c =
	match c with
	| `Nom -> Glib.Utf8.lowercase "nominative"
	| `Gen -> Glib.Utf8.lowercase "genitive"
	| `Dat -> Glib.Utf8.lowercase "dative"
	| `Acc -> Glib.Utf8.lowercase "accusative"
	| `Ins -> Glib.Utf8.lowercase "instrumental"
	| `Loc -> Glib.Utf8.lowercase "locative"
	| `Voc -> Glib.Utf8.lowercase "vocative"
	;;

let case_name_pl c = 
	match c with
	| `Nom -> Glib.Utf8.lowercase "mianownik"
	| `Gen -> Glib.Utf8.lowercase "dopełniacz"
	| `Dat -> Glib.Utf8.lowercase "celownik"
	| `Acc -> Glib.Utf8.lowercase "biernik"
	| `Ins -> Glib.Utf8.lowercase "narzędnik"
	| `Loc -> Glib.Utf8.lowercase "miejscownik"
	| `Voc -> Glib.Utf8.lowercase "wołacz"
	;;

let sg_abbr = Glib.Utf8.lowercase "lp";;
let pl_abbr = Glib.Utf8.lowercase "lm";;
