let graphics_main () =
begin
	let window = GWindow.window () in
	let button = GButton.button ~label:"Button" ~packing:window#add () in
	window#show ();
	GMain.Main.main ()
end;;
