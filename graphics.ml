open Polish

class noun_box ?width ?height ?packing ?show array =
	
	let table = GPack.table ~rows:9 ~columns:3 ~col_spacings:2 ?width ?height
		?packing ?show () in	
	let nom_sg = GEdit.entry ~packing:(table#attach ~left:1 ~top:1) () in
	let gen_sg = GEdit.entry ~packing:(table#attach ~left:1 ~top:2) () in
	let dat_sg = GEdit.entry ~packing:(table#attach ~left:1 ~top:3) () in
	let acc_sg = GEdit.entry ~packing:(table#attach ~left:1 ~top:4) () in
	let ins_sg = GEdit.entry ~packing:(table#attach ~left:1 ~top:5) () in
	let loc_sg = GEdit.entry ~packing:(table#attach ~left:1 ~top:6) () in
	let voc_sg = GEdit.entry ~packing:(table#attach ~left:1 ~top:7) () in
	let nom_pl = GEdit.entry ~packing:(table#attach ~left:2 ~top:1) () in
	let gen_pl = GEdit.entry ~packing:(table#attach ~left:2 ~top:2) () in
	let dat_pl = GEdit.entry ~packing:(table#attach ~left:2 ~top:3) () in
	let acc_pl = GEdit.entry ~packing:(table#attach ~left:2 ~top:4) () in
	let ins_pl = GEdit.entry ~packing:(table#attach ~left:2 ~top:5) () in
	let loc_pl = GEdit.entry ~packing:(table#attach ~left:2 ~top:6) () in
	let voc_pl = GEdit.entry ~packing:(table#attach ~left:2 ~top:7) () in

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
					Printf.eprintf "Read (%s %s): %s\n%!" c n w;
					if Glib.Utf8.collate nom_name c = 0 then
					begin
						if Glib.Utf8.collate sg_abbr n = 0 then
							nom_sg#set_text w
						else
							nom_pl#set_text w	
					end
					else if Glib.Utf8.collate gen_name c = 0 then
					begin
						if Glib.Utf8.collate sg_abbr n = 0 then
							gen_sg#set_text w
						else
							gen_pl#set_text w	
					end
					else if Glib.Utf8.collate dat_name c = 0 then
					begin
						if Glib.Utf8.collate sg_abbr n = 0 then
							dat_sg#set_text w
						else
							dat_pl#set_text w	
					end
					else if Glib.Utf8.collate acc_name c = 0 then
					begin
						if Glib.Utf8.collate sg_abbr n = 0 then
							acc_sg#set_text w
						else
							acc_pl#set_text w	
					end
					else if Glib.Utf8.collate ins_name c = 0 then
					begin
						if Glib.Utf8.collate sg_abbr n = 0 then
							ins_sg#set_text w
						else
							ins_pl#set_text w	
					end
					else if Glib.Utf8.collate loc_name c = 0 then
					begin
						if Glib.Utf8.collate sg_abbr n = 0 then
							loc_sg#set_text w
						else
							loc_pl#set_text w	
					end
					else if Glib.Utf8.collate voc_name c = 0 then
					begin
						if Glib.Utf8.collate sg_abbr n = 0 then
							voc_sg#set_text w
						else
							voc_pl#set_text w	
					end
					else
						Printf.eprintf "Er... %s %s\n%!" c n
        ) (Wiktionary.get_tree url);
				d#destroy ()
			end
	end in
	begin
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
		let btn = GButton.button ~label:"Read Wiktionary..."
			~packing:(table#attach ~left:0 ~right:3 ~top:8) () in
			btn#connect#clicked ~callback:read_wikt
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
