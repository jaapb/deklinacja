let cases = [ `Nom; `Gen; `Dat; `Acc; `Ins; `Loc; `Voc ]

let case_name c = 
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
