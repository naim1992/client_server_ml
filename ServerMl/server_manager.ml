open Server;;
open Connexion_manager;;

let users = ref [];;
let mutex = Mutex.create ();;
let cond = Condition.create ();;
let matrice = Array.make_matrix 4 4 "";;
let num_tour = ref 1;;


  (*transformer la matrice en  string pour l'envoyer*)
let array_to_string tab = 
  let tab_string = ref "" in 
   for i = 0 to ((Array.length tab) -1)  do
    for j = 0 to ((Array.length tab.(i)) -1)  do
    tab_string := !tab_string ^ tab.(i).(j);
    done
   done;
   tab_string := !tab_string ^ "\n";
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
				
				
let expiration clients = 
	  Thread.delay 30.0;
		print_endline "fin de temps repartie";
		let message =  "RFIN/\n" in
          ignore (List.map (
                      fun x -> 
                                output_string x.outchan message;
                								flush x.outchan;
                    ) !clients);
										Mutex.lock mutex;
										Condition.signal cond;
										Mutex.unlock mutex;
										bilan !clients;
		Thread.exit ();;	
		


class tour (usrs : Connexion_manager.infos list ref) = 
	object(self)
	initializer
		des ();
		ignore (self#fin_tour ())
	
	(* method debut_tour = *) 
	
	val mutable tirage = matrice

	method getClients = usrs
	
	method getDictionnaire = read_file "dictionnaire.dat"
  (*tirage : matrice 4*4 *)
  method getTirage = matrice
	method setTirage () = des (); tirage <- matrice
	
	method fin_tour () =  Thread.create (fun x -> expiration self#getClients)() 
	
	
		

end;;

class server_maj port n =
   object(self)
   inherit server port n
		
	val mutable tour_actuel = new tour users
	 
	method start_tour num = 
		while !num_tour <> num do
			Mutex.lock mutex;
			Condition.wait cond mutex;
			Mutex.unlock mutex
		done;
		
				Mutex.lock mutex;
				tour_actuel#fin_tour ();
				tour_actuel#setTirage ();
				let message = "TOUR/" ^ (array_to_string tour_actuel#getTirage) in
								ignore (List.map (
						                      fun x -> 
						                                output_string x.outchan message;
						                								flush x.outchan;
						                    ) !users);
				
				num_tour := !num_tour + 1;
				Condition.signal cond;
				Mutex.unlock mutex
											
			
		
		 
   method treat s sa =
	 for i = 1 to 5 do	
   		ignore (Thread.create (fun x -> self#start_tour i)())
		done;
		
	 ignore( (new connexion_maj s sa true tour_actuel )#start())
	
   end;;