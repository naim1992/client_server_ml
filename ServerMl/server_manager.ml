open Server;;
open Connexion_manager;;

let users = ref [];;
let mutex = Mutex.create ();;
let cond = Condition.create ();;
let matrice = Array.make_matrix 4 4 "";;
let num_tour = ref 1;;
let mutex = Mutex.create ();;


	


  (*transformer la matrice en  string pour l'envoyer*)
let array_to_string tab = 
  let tab_string = ref "" in 
   for i = 0 to ((Array.length tab) -1)  do
    for j = 0 to ((Array.length tab.(i)) -1)  do
    tab_string := !tab_string ^ tab.(i).(j);
    done
   done;
   !tab_string;;


(*lire le dictionnaire dans une liste*)
let read_file filename = 
    let lines = ref [""] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done;
      !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines;;



		
      
(*lancer les des *)
let des () = 
  let list_des = read_file "des.dat" and index = ref 1
	and tab_string = ref "" in
  for i = 0 to (Array.length matrice) - 1 do
    for j = 0 to (Array.length matrice.(i)) - 1 do
        let rand = Random.int 6 and
            case = List.nth list_des !index 
            in 
              matrice.(i).(j) <- (String.make 1 case.[rand]);
              index := !index + 1   
      done
  done;;

(* pour faire le bilan *)
let bilan clients = 
		
		
		ignore (
			List.map (fun x -> 
				 let mots = ref "" and scores = ref ((string_of_int !num_tour) ^ "*") in
					List.map (fun y -> mots := !mots ^ y.user ^ ":" ^  !(y.motsproposes) ^ ";";
					                  scores := !scores ^ y.user ^  "*" ^ (string_of_int !(y.score) ^ "*")) clients;
						
					let message = "BILANMOTS/" ^ !mots ^ "/" ^ !scores ^ "/\n" in							
						output_string x.outchan message;
            flush x.outchan					
				) clients
				)
				
				

		


class tour (usrs : Connexion_manager.infos list ref) nb_tour= 
	object(self)
	initializer 
		self#setTirage ()

	
	(* method debut_tour = *) 
	
	val mutable tirage = matrice
	val mutable words_found = [""]

	method getClients = usrs
	
	method getDictionnaire = read_file "dictionnaire.dat"
  (*tirage : matrice 4*4 *)
  method getTirage = matrice;
	
		
		
	method setTirage () = 
												des ();
												tirage <- matrice;
												
												
	
	method fin_tour () =  Thread.create (fun x -> self#expiration self#getClients)() 
	method getNumTour () = !num_tour
	
	(* experation de tour *)
	method expiration clients = 
		Thread.delay 30.0;
		print_endline ("fin de temps repartie pour le tour " ^ string_of_int !num_tour);
		let message =  "RFIN/\n" in
          ignore (List.map (
                      fun x -> 
                                output_string x.outchan message;
                								flush x.outchan;
                    ) !clients);
										bilan !clients;
										num_tour := !num_tour + 1; 
										ignore (Thread.create (fun x -> self#start_tour !num_tour)());
		Thread.exit ()	
	
	method start_tour num = 
		if !num_tour < nb_tour then 
			begin
				print_endline ("debut de tour " ^ string_of_int num);
				
				
				if (!num_tour > 1) then
					begin
				self#setTirage ();
				let message = "TOUR/" ^ (array_to_string self#getTirage) ^ "/\n" in
								ignore (List.map (
						                      fun x -> 
						                                output_string x.outchan message;
						                								flush x.outchan;
						                    ) !users)
					end;
																
				ignore(self#fin_tour ())
			end
	
	method getWords = words_found
	
	method add_word word = 
		Mutex.lock mutex;
		words_found <- words_found@[word];
		Mutex.unlock mutex	

end;;

class server_maj port n =
   object(self)
   inherit server port n
		
					
	 val mutable tour_actuel = new tour users n
	 
   method treat s sa =
	 ignore( (new connexion_maj s sa true tour_actuel )#start())
   end;;