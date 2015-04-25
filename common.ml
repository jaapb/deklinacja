open Polish

let read_wiktionary nb word =
let url = Printf.sprintf "http://pl.wiktionary.org/wiki/%s?action=raw" word in
begin
	List.iter (fun (c, n, w) ->
		List.iteri (fun i case ->
			if Glib.Utf8.collate (case_name_pl case) c = 0 then
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
let result = ref [] in
begin
	for i = 1 to nr
	do
		let case = Random.int nr_cases in
		let number = if Random.bool () then "S" else "P" in
		let (id, word) = Database.random_word () in
		let question = Printf.sprintf "What is the %s %s of %s?"
			(case_name_en (List.nth cases case)) number word in
		let answer = Database.find_answer (Int32.of_int case) number id in
		let d = GWindow.message_dialog ~modal:true ~message_type:`QUESTION
			~buttons:GWindow.Buttons.ok ~title:"Question" ~message:question () in
		d#set_default_response `OK;
		let ew = GEdit.entry ~packing:d#vbox#add () in
		ew#set_activates_default true;
		match d#run () with
		| `DELETE_EVENT ->
			begin
				result := (word, case, number, answer, "")::!result;
				d#destroy ()
			end
		| `OK -> 
			begin
				result := (word, case, number, answer, ew#text)::!result;
				d#destroy ()	
			end;
	done;
	List.rev !result
end;;
