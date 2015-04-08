open Graphics

let _ =
begin
	Random.self_init ();
	Database.open_database ();
	graphics_main ()	
end;;
