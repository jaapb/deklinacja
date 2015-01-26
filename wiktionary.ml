open Http_client
open Nethtml

let rec parse_found ch s n old_res m_res =
begin
	if n >= String.length s then
		old_res	
	else
	let c = ch#input_char () in
		if c = s.[n] then
			parse_found ch s (n+1) old_res (Printf.sprintf "%s%c" m_res c)
		else
			parse_until_aux ch s (Printf.sprintf "%s%s%c" old_res m_res c)
end
and parse_until_aux ch s res =
begin
	try
		let c = ch#input_char () in
		if c = s.[0] then
			parse_found ch s 1 res (String.make 1 c) 
		else
			parse_until_aux ch s (Printf.sprintf "%s%c" res c)
	with End_of_file -> res
end

let rec parse_template ch = 
begin
	let t = parse_until_aux ch "}}" "" in
	try
		let i = String.index t '{' in
		if t.[i + 1] == '{' then
	 		Printf.sprintf "%s}}%s" t (parse_template ch)
		else
			t
	with 
		Not_found -> t
end;;
	
let get_tree url =
begin
	let pipeline = new pipeline in
	let get_call = new get url in
	pipeline#add_with_callback get_call
	(fun m ->
		let ch = m#response_body#open_value_rd () in
		ignore (parse_until_aux ch "{{odmiana-rzeczownik-polski" "");
		let block = parse_template ch in
		let lines = Str.split (Str.regexp_string "\n") block in
		List.iter (fun l ->
			Scanf.sscanf l "|%s %s = %s" (fun case number word ->
				Printf.printf "Case: %s, number: %s -> %s\n" case number word
			)
		) lines
	);
	pipeline#run ();
end;;
