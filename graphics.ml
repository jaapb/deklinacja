class noun_box ?width ?height ?packing ?show array =
	
	let vbox = GPack.vbox ?width ?height ?packing ?show () in	

	object (self)
		inherit GObj.widget vbox#as_widget

	method init =
	begin
		let decl = GPack.vbox ~packing:vbox#add () in
		let singular = GPack.vbox ~packing:decl#add () in
		let sg_lbl = GMisc.label ~packing:singular#add () in
			sg_lbl#set_label "SINGULAR";
		let plural = GPack.vbox ~packing:decl#add () in
		let nomsg = GMisc.label ~packing:singular#add () in
			nomsg#set_label "Nominative"		
	end
end;;

let graphics_main () =
	let delete_event ev =
		false
	and destroy () =
		GMain.Main.quit () in
begin
	GtkMain.Main.init ();
	let window = GWindow.window () in
	window#event#connect#delete ~callback:delete_event;
  window#connect#destroy ~callback:destroy;
	let nb = new noun_box ~packing:window#add () in
		nb#init;
		window#show ();
		GMain.Main.main ()
end;;
