open Polish

let read_wiktionary nb word =
let url = Printf.sprintf "http://pl.wiktionary.org/wiki/%s?action=raw" word in
begin
	List.iter (fun (c, n, w) ->
		List.iteri (fun i case ->
			if Glib.Utf8.collate (case_name	case) c = 0 then
			begin
				if Glib.Utf8.collate sg_abbr n = 0 then
					nb#set_singular_entry i w
				else
					nb#set_plural_entry i w
			end
		) cases
	) (Wiktionary.get_tree url)
end;;

let save_contents nb () =
begin
	let singulars = List.mapi (fun i case ->
		(case, nb#get_singular_entry i)	
	) cases
	and plurals = List.mapi (fun i case ->
		(case, nb#get_plural_entry i)
	) cases in
	try	
		let nom_sg = List.assoc `Nom singulars in
		match Database.find_word nom_sg with
		| [] -> Database.add_word nom_sg singulars plurals
		| h::t -> Database.update_word h singulars plurals
	with Not_found -> ()
end;;

let do_exercises nr =
begin
end;;

