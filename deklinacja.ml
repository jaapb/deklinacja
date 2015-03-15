open Graphics

let _ =
begin
	Database.open_database ();
	graphics_main ()	
end;;
