open Polish

class noun_box ?width ?height ?packing ?show () =
	
	let table = GPack.table ~rows:9 ~columns:3 ~col_spacings:2 ?width ?height
		?packing ?show () in	
	let sg_entries = Array.init (nr_cases) (fun c -> GEdit.entry ()) in  
	let pl_entries = Array.init (nr_cases) (fun c -> GEdit.entry ()) in  

	object (self)
		inherit GObj.widget table#as_widget

	method set_singular_entry i w =
	begin
		sg_entries.(i)#set_text w
	end

	method set_plural_entry i w =
	begin
		pl_entries.(i)#set_text w
	end

	method get_singular_entry i =
	begin
		sg_entries.(i)#text
	end

	method get_plural_entry i =
	begin
		pl_entries.(i)#text
	end

	method init =
	let read_wikt () =
	begin
		let d = GWindow.message_dialog ~modal:true ~message_type:`QUESTION
			~buttons:GWindow.Buttons.ok_cancel ~title:"Select word" 
			~message:"What word would you like to look up?" () in
		let ew = GEdit.entry ~packing:d#vbox#add () in
		match d#run () with
		| `CANCEL | `DELETE_EVENT -> d#destroy ()
		| `OK -> let word = ew#text in
			begin
				d#destroy ();
				Common.read_wiktionary self word
			end
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
			btn2#connect#clicked ~callback:(Common.save_contents self);
		let btn3 = GButton.button ~label:"Clear"
			~packing:(table#attach ~left:2 ~top:8) () in
			btn3#connect#clicked ~callback:do_clear
	end
end;;

class result_box ?width ?height ?packing ?show () =

	let table = GPack.table ~columns:5 ~col_spacings:2 ?width ?height
		?packing ?show () in	

	object (self)
		inherit GObj.widget table#as_widget

	val mutable results = ([]: (string*int*string*string*string) list)

	method set_results l =
	begin
		results <- l;
		table#set_rows (List.length l + 1);
		let w_lbl = GMisc.label ~packing:(table#attach ~left:0 ~top:0) () in
			w_lbl#set_label "Word";
		let c_lbl = GMisc.label ~packing:(table#attach ~left:1 ~top:0) () in
			c_lbl#set_label "Case";
		let n_lbl = GMisc.label ~packing:(table#attach ~left:2 ~top:0) () in
			n_lbl#set_label "Number";
		let a_lbl = GMisc.label ~packing:(table#attach ~left:3 ~top:0) () in
			a_lbl#set_label "Your answer";
		let ca_lbl = GMisc.label ~packing:(table#attach ~left:4 ~top:0) () in
			ca_lbl#set_label "Correct answer";
		List.iteri (fun i (w, c, n, ca, a) ->
			let w_lbl = GMisc.label ~packing:(table#attach ~left:0 ~top:(i+1)) () in
				w_lbl#set_label w;
			let c_lbl = GMisc.label ~packing:(table#attach ~left:1 ~top:(i+1)) () in
				c_lbl#set_label (Polish.case_name_en (List.nth cases c));
			let n_lbl = GMisc.label ~packing:(table#attach ~left:2 ~top:(i+1)) () in
				n_lbl#set_label n;
			let a_ev = GBin.event_box ~packing:(table#attach ~left:3 ~top:(i+1)) () in
			let a_lbl = GMisc.label ~packing:a_ev#add () in
				a_lbl#set_label a;
			let ca_lbl = GMisc.label ~packing:(table#attach ~left:4 ~top:(i+1)) () in
				ca_lbl#set_label ca;
			if Glib.Utf8.collate ca a <> 0 then
			begin
				let style = a_ev#misc#style#copy in
				a_ev#misc#set_style style;
				a_ev#misc#style#set_bg [`NORMAL, `NAME "red"; `ACTIVE, `NAME "red";
					`PRELIGHT, `NAME "red"; `INSENSITIVE, `NAME "red";
					`SELECTED, `NAME "red"]
			end
		) results
	end

	method init res = 
	begin
		self#set_results res
	end

end;;

let do_exercise () =
begin
	let d = GWindow.message_dialog ~modal:true ~message_type:`QUESTION
		~buttons:GWindow.Buttons.ok_cancel ~title:"Question" 
		~message:"How many questions would you like?" () in
	let ew = GEdit.entry ~packing:d#vbox#add () in
	match d#run () with
	| `CANCEL | `DELETE_EVENT -> d#destroy ()
	| `OK -> let nr_ex = int_of_string ew#text in
		begin
			d#destroy ();
			let r = Common.do_exercises nr_ex in
			let window = GWindow.window ~title:"Deklinacja: results" () in
			let res = new result_box ~packing:window#add () in
				res#init r;
				window#show ()
		end
end;;

let do_add_words () =
begin
	let window = GWindow.window ~title:"Deklinacja: add words" () in
	let nb = new noun_box ~packing:window#add () in
		nb#init;
		window#show ()
end;;

let create_menu mb =
begin
	let file_menu = GMenu.menu () in
	let item = GMenu.menu_item ~label:"Exercise" ~packing:file_menu#append () in
		item#connect#activate ~callback:do_exercise;
	let item = GMenu.menu_item ~label:"Add words..."
		~packing:file_menu#append () in
		item#connect#activate ~callback:do_add_words;
	ignore (GMenu.separator_item ~packing:file_menu#append ());
	let item = GMenu.menu_item ~label:"Quit" ~packing:file_menu#append () in
		item#connect#activate ~callback:(fun () ->
			Database.close_database (); GMain.Main.quit ());
	let file_item = GMenu.menu_item ~label:"File" () in
		file_item#set_submenu file_menu;
		mb#append file_item
end;;

let graphics_main () =
begin
	GtkMain.Main.init ();
	let window = GWindow.window ~title:"Deklinacja" () in
  	window#connect#destroy ~callback:(fun () ->
			Database.close_database (); GMain.Main.quit ());
	let vbox = GPack.vbox ~packing:window#add () in
	let mb = GMenu.menu_bar ~packing:vbox#add () in
		create_menu mb;
		window#show ();
		GMain.Main.main ()
end;;
