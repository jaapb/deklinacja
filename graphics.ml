open Polish

class noun_box ?width ?height ?packing ?show array =
	
	let table = GPack.table ~rows:9 ~columns:3 ~col_spacings:2 ?width ?height
		?packing ?show () in	
	let sg_entries = Array.init (List.length cases) (fun c -> GEdit.entry ()) in  
	let pl_entries = Array.init (List.length cases) (fun c -> GEdit.entry ()) in  

	object (self)
		inherit GObj.widget table#as_widget

	method init =
	let read_wikt () =
	begin
		let d = GWindow.message_dialog ~modal:true ~message_type:`QUESTION
			~buttons:GWindow.Buttons.ok_cancel ~title:"Select word" 
			~message:"What word would you like to look up?" () in
		let ew = GEdit.entry ~packing:d#vbox#add () in
		match d#run () with
		| `CANCEL | `DELETE_EVENT -> d#destroy ()
		| `OK -> let url = Printf.sprintf "http://pl.wiktionary.org/wiki/%s?action=raw" ew#text in
			begin
				List.iter (fun (c, n, w) ->
					List.iteri (fun i case ->
						if Glib.Utf8.collate (case_name	case) c = 0 then
						begin
							if Glib.Utf8.collate sg_abbr n = 0 then
								sg_entries.(i)#set_text w
							else
								pl_entries.(i)#set_text w	
						end
					) cases
        ) (Wiktionary.get_tree url);
				d#destroy ()
			end
	end in
	let save_contents () =
	begin
		let singulars = List.mapi (fun i case ->
			(case, sg_entries.(i)#text)	
		) cases
		and plurals = List.mapi (fun i case ->
			(case, pl_entries.(i)#text)
		) cases in
		let nom_sg = try	
			List.assoc `Nom singulars 
		with Not_found -> "hyuk" in
		match Database.find_word nom_sg with
		| [] -> Database.add_word nom_sg singulars plurals
		| h::t -> Database.update_word h singulars plurals
	end in
	let do_clear () =
	begin
		for i = 0 to 6 do
			sg_entries.(i)#set_text "";
			pl_entries.(i)#set_text ""
		done
	end in
	begin
		for i = 0 to 6 do
			table#attach ~left:1 ~top:(i+1) sg_entries.(i)#coerce;
			table#attach ~left:2 ~top:(i+1) pl_entries.(i)#coerce
		done;
		let sg_lbl = GMisc.label ~packing:(table#attach ~left:1 ~top:0) () in
			sg_lbl#set_label "SINGULAR";
		let pl_lbl = GMisc.label ~packing:(table#attach ~left:2 ~top:0) () in  
			pl_lbl#set_label "PLURAL";
		let nom_lbl = GMisc.label ~packing:(table#attach ~left:0 ~top:1) () in
			nom_lbl#set_label "Nominative";
		let gen_lbl = GMisc.label ~packing:(table#attach ~left:0 ~top:2) () in
			gen_lbl#set_label "Genitive";
		let dat_lbl = GMisc.label ~packing:(table#attach ~left:0 ~top:3) () in
			dat_lbl#set_label "Dative";
		let acc_lbl = GMisc.label ~packing:(table#attach ~left:0 ~top:4) () in
			acc_lbl#set_label "Accusative";
		let ins_lbl = GMisc.label ~packing:(table#attach ~left:0 ~top:5) () in
			ins_lbl#set_label "Instrumental";
		let loc_lbl = GMisc.label ~packing:(table#attach ~left:0 ~top:6) () in
			loc_lbl#set_label "Locative";
		let voc_lbl = GMisc.label ~packing:(table#attach ~left:0 ~top:7) () in
			voc_lbl#set_label "Vocative";
		let btn1 = GButton.button ~label:"Read Wiktionary..."
			~packing:(table#attach ~left:0 ~top:8) () in
			btn1#connect#clicked ~callback:read_wikt;
		let btn2 = GButton.button ~label:"Save"
			~packing:(table#attach ~left:1 ~top:8) () in
			btn2#connect#clicked ~callback:save_contents;
		let btn3 = GButton.button ~label:"Clear"
			~packing:(table#attach ~left:2 ~top:8) () in
			btn3#connect#clicked ~callback:do_clear
	end
end;;

let create_menu mb =
begin
	let file_menu = GMenu.menu () in
	let item = GMenu.menu_item ~label:"Quit" ~packing:file_menu#append () in
		item#connect#activate ~callback:GMain.Main.quit;
	let file_item = GMenu.menu_item ~label:"File" () in
		file_item#set_submenu file_menu;
		mb#append file_item
end;;

let graphics_main () =
begin
	GtkMain.Main.init ();
	let window = GWindow.window ~title:"Deklinacja" () in
  	window#connect#destroy ~callback:GMain.Main.quit;
	let vbox = GPack.vbox ~packing:window#add () in
	let mb = GMenu.menu_bar ~packing:vbox#add () in
		create_menu mb;
	let nb = new noun_box ~packing:vbox#add () in
		nb#init;
		window#show ();
		GMain.Main.main ()
end;;
