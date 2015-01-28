class noun_box ?width ?height ?packing ?show array =
	
	let table = GPack.table ~rows:8 ~columns:3 ~col_spacings:2 ?width ?height
		?packing ?show () in	

	object (self)
		inherit GObj.widget table#as_widget

	method init =
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
			()
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
	let window = GWindow.window () in
  	window#connect#destroy ~callback:GMain.Main.quit;
	let vbox = GPack.vbox ~packing:window#add () in
	let mb = GMenu.menu_bar ~packing:vbox#add () in
		create_menu mb;
	let nb = new noun_box ~packing:vbox#add () in
		nb#init;
		window#show ();
		GMain.Main.main ()
end;;
