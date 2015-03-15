let open_database () =
begin
	PGOCaml.connect ~host:!Config.db_host ~user:!Config.db_user
		~database:!Config.db_name ()
end;;

let close_database dbh =
begin
	PGOCaml.close dbh
end;;

let create_tables dbh =
begin
	PGSQL(dbh) "CREATE TABLE words \
		(id SERIAL PRIMARY KEY, \
		word TEXT NOT NULL)";
	PGSQL(dbh) "CREATE TABLE declensions \
		(word_id INTEGER REFERENCES words(id) NOT NULL, \
		\"case\" VARCHAR(32) NOT NULL, \
		word TEXT NOT NULL)"
end;;

let save_word sgl pll =
begin
end;;
