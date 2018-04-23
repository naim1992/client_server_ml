open Server;;
open Connexion_manager;;

let users = ref [];;

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
  let matrice = Array.make_matrix 4 4 "" and
  list_des = read_file "des.dat" and index = ref 1 in
  for i = 0 to (Array.length matrice) - 1 do
    for j = 0 to (Array.length matrice.(i)) - 1 do
        let rand = Random.int 6 and
            case = List.nth list_des !index 
            in 
              matrice.(i).(j) <- (String.make 1 case.[rand]);
              index := !index + 1   
      done
  done;
  matrice;;

let expiration clients = Thread.delay 120.0;
		print_endline "fin de temps repartie";
		let message =  "RFIN/\n" in
          List.map (
                      fun x -> 
                                output_string x.outchan message;
                								flush x.outchan;
                    ) !clients;
										print_endline "nouveau tour commence";
		Thread.exit ();;	
		


class tour (usrs : Connexion_manager.infos list ref) = 
	object(self)
	initializer
		ignore (self#fin_tour ())
	
	val clients = usrs
	(* method debut_tour = *) 
	
	method dictionnaire = read_file "dictionnaire.dat"
  (*tirage : matrice 4*4 *)
  method tirage = des ()
	
	method fin_tour () =  Thread.create (fun x -> expiration clients)() 
		
		
		
			

end;;

class server_maj port n =
   object(self)
   inherit server port n
		
	 val tr = new tour users
	 
	 
	 
   method treat s sa =
   ignore( (new connexion_maj s sa true users tr#tirage tr#dictionnaire )#start());
   end;;