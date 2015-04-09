let dbh = ref None

let open_database () =
begin
	dbh := Some (PGOCaml.connect ~host:!Config.db_host ~user:!Config.db_user
		~password:!Config.db_password ~database:!Config.db_name ())
end;;

let close_database () =
begin
	match !dbh with
	| None -> ()
	| Some h -> PGOCaml.close h
end;;

let create_tables () =
begin
	match !dbh with
	| None -> raise (Failure "database not opened")
	| Some h ->
			PGSQL(h) "CREATE TABLE words \
				(id SERIAL PRIMARY KEY, \
				word TEXT NOT NULL)";
			PGSQL(h) "CREATE TABLE declensions \
				(word_id INTEGER REFERENCES words(id) NOT NULL, \
				case_nr INTEGER NOT NULL, \
				number CHAR(1) NOT NULL, \
				word TEXT NOT NULL)"
end;;

let find_word w =
begin
	match !dbh with
	| None -> raise (Failure "database not opened")
	| Some h -> List.map Int32.to_int
			(PGSQL(h) "SELECT id FROM words WHERE word = $w")
end;;

let add_word word sgl pll =
begin
	match !dbh with
	| None -> raise (Failure "database not opened")
	| Some h ->
		let ids = PGSQL(h) "INSERT INTO words (id, word) VALUES (DEFAULT, $word) \
			RETURNING id" in
		begin
			match ids with
			| [] -> raise (Failure "oopsie, empty list.")
			| [id] -> List.iteri (fun i (c, w) ->
					let c = Int32.of_int i in
					PGSQL(h) "INSERT INTO declensions (word_id, case_nr, number, word) \
						VALUES ($id, $c, 'S', $w)"
				) sgl;
				List.iteri (fun i (c, w) -> 
					let c = Int32.of_int i in
					PGSQL(h) "INSERT INTO declensions (word_id, case_nr, number, word) \
						VALUES ($id, $c, 'P', $w)"
				) pll
			| _ -> raise (Failure "oopsier, too many elements in list.")
		end
end;;

let update_word i sgl pll =
begin
	let id = Int32.of_int i in
	match !dbh with 
	| None -> raise (Failure "database not opened")
	| Some h ->
		begin
			List.iteri (fun i (c, w) ->
				let c = Int32.of_int i in
				PGSQL(h) "UPDATE declensions SET word = $w \
					WHERE word_id = $id AND case_nr = $c AND number = 'S'"
			) sgl;
			List.iteri (fun i (c, w) ->
				let c = Int32.of_int i in
				PGSQL(h) "UPDATE declensions SET word = $w \
					WHERE word_id = $id AND case_nr = $c AND number = 'P'"
			) pll
		end
end;;

let random_word () =
begin
	match !dbh with
	| None -> raise (Failure "database not opened")
	| Some h ->
		begin
			List.hd (PGSQL(h) "SELECT id, word FROM words \
				OFFSET floor(random() * (SELECT COUNT(*) FROM words)) LIMIT 1")
		end
end;;

let find_answer case_nr number i =
begin
	match !dbh with
	| None -> raise (Failure "database not opened")
	| Some h ->
		begin
			List.hd (PGSQL(h) "SELECT word FROM declensions \
				WHERE case_nr = $case_nr AND number = $number AND word_id = $i")
		end
end;;
