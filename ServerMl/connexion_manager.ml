open Connexion;;

type infos = {user : string; socket : Unix.file_descr; motsproposes: string ref;  score : int ref; outchan : out_channel };;

exception Fin ;;


(* verifier la trajectoire *)
let verif_traj traj = 
  
	let verifier = ref true and i = ref 0 	
	in
  if (String.length traj) mod 2 = 0 then
  begin
    while (!i+2) < (String.length traj) && !verifier = true do
        let sub1 = (String.sub traj !i 2) and 
            sub2 = (String.sub traj (!i+2) 2) in
            let index1 = int_of_char sub1.[1] and
            index2 = int_of_char sub2.[1] in
            i := !i + 2; 
            if index2 > (index1 + 1) || index1 > (index2 + 1) then
            begin
              verifier := false
            end;
						
            match sub1.[0] with
            'A' -> if sub2.[0] <> 'B' && sub2.[0] <> 'A' then
                verifier := false
            | 'B' -> if  sub2.[0] <> 'B' && sub2.[0] <> 'A' && sub2.[0] <> 'C' then
                verifier := false
            | 'C' -> if sub2.[0] <> 'C' && sub2.[0] <> 'B' &&  sub2.[0] <> 'D' then							
                verifier := false
            | 'D' -> if sub2.[0] <> 'D' && sub2.[0] <> 'C' then
                verifier := false
            | _-> verifier := false
    done 
  end
  else
  begin
    verifier := false
  end;
  !verifier;;

(*trouvé une lettre selon une case dans une matrice *)
let word_in_case case tab = 
  let lettre =  match case.[0] with
    'A' -> tab.(0).((int_of_string (String.make 1 case.[1])) - 1)
    | 'B' -> tab.(1).((int_of_string (String.make 1 case.[1])) - 1)
    | 'C' -> tab.(2).((int_of_string (String.make 1 case.[1])) - 1)
    | 'D' -> tab.(3).((int_of_string (String.make 1 case.[1])) - 1)
		| _ -> ""
    in
		
  lettre;;


(* pour trouver un mot selon sa trajctoire *)
let find traj tab = 
  let i = ref 0 and mot = ref "" in
  if (String.length traj) mod 2 = 0 then  
  begin
     while (!i+2) <= String.length traj do
        mot := !mot ^ (word_in_case (String.sub traj !i 2) tab);
        i := !i + 2
      done
  end;
    !mot

  (*recuperer la les scores*)
let scores list = 
  let elements  = ref "" in
     ignore (List.map (fun x -> elements := !elements ^ ";" ^ (string_of_int !(x.score))) list);
    !elements ;; 


  (*transformer la matrice en  string pour l'envoyer*)
let array_to_string tab = 
  let tab_string = ref "" in 
   for i = 0 to ((Array.length tab) -1)  do
    for j = 0 to ((Array.length tab.(i)) -1)  do
    tab_string := !tab_string ^ tab.(i).(j);
    done
   done;
   !tab_string;;

class connexion_maj sd sa b tour =
object(self)
inherit connexion sd sa b tour
    
    val score = ref 0
    val inchan = Unix.in_channel_of_descr sd
    val out_chan = Unix.out_channel_of_descr sd
		val clients = tour#getClients
		val tirage = tour#getTirage
		val dictionnaire = tour#getDictionnaire

    method deconnect user = 
          clients := List.filter (fun x -> x.socket <> s_descr) !clients;
          let message =  "DECONNEXION/" ^ user ^ "\n" in
          ignore(List.map (
                      fun x -> 
                                output_string x.outchan message;
                								flush x.outchan;
                    ) !clients);
          Unix.close s_descr


    method already_connected client = 
          List.exists (fun x -> String.equal x.user client) !clients

(* Signalement de la connexion de ’user’ aux autres clients. *)
method signal_connexion client = 
  let others = List.filter (fun x -> x.socket <> s_descr) !clients in
  let message = "CONNECTE/" ^ client ^ "\n"   in
  ignore(List.map (
              fun x ->  
								output_string x.outchan message;
                flush x.outchan;          
              ) others)
  

(*nouvelle connexion d'un client *)
    method connect client = 
        if (self#already_connected client) then
            begin
                let message = "**** " ^ client ^ " deja connecté " ^ "\n" in
                output_string out_chan message;
                flush out_chan;
                Unix.close s_descr  
            end
          else
            begin
              (* pour la gestion de temps*)
                (* Thread.create gestion_temps clients; *)
                clients := !clients@[{user = client; socket = s_descr; motsproposes = ref ""; score = ref 0; outchan = out_chan}];
                print_endline ("new Connexion from : " ^ client);
								let scores = ref ((string_of_int (tour#getNumTour ())) ^ "*") in
					   
								List.map (fun y -> 
					                  scores := !scores ^ y.user ^  "*" ^ (string_of_int !(y.score) ^ "*")) !clients;
						
                let message = "BIENVENU/" ^ (array_to_string tour#getTirage) ^ "/" ^ !scores ^ "/\n" in
                        output_string out_chan message;
                        flush out_chan
              
            end 

    (* debut d'une session *)
    method start_session () = 
            let message = "SESSION/\n" in
              output_string out_chan message;
              flush out_chan

	(*fin d'une session *)
    method end_session () = 
            let message =  "VAINQUEUR/" ^ (scores !clients) ^ "\n" in
            output_string out_chan message;
            flush out_chan
    
	(* debut d'un nouveau tour *)
	method nouveau_tour () = 
		let message = "TOUR/" ^ (array_to_string tirage) in
        output_string out_chan message;
        flush out_chan
  
	(* phase de recherche *)
  method trouve mot trajectoire =			
		print_endline (array_to_string tirage);
		let reponse = ref "" in
			print_endline trajectoire; 
      if (verif_traj trajectoire == true) then
        begin		
						if String.equal mot (find trajectoire tirage) then
	              begin
									
										if (List.exists (fun x -> mot = x) tour#getWords) then
											reponse := "MINVALID/" ^ mot ^ "ALREADY FOUND/\n"
										else 
									
	                  (* verifier l'existance d'un mot dans le dictionnaire*)
	                  if (List.exists (fun x -> x = (String.lowercase mot)) dictionnaire) then
			                  begin
			                    if String.length mot > 8 then
			                          score := !score + 11
			                        else
			                          begin
			                            match String.length mot with
			                              3 -> score := !score + 1
			                              | 4 -> score := !score + 1
			                              | 5 -> score := !score + 2
			                              | 6 -> score := !score + 3
			                              | 7 -> score := !score + 5
																		| _ -> score := !score
			                          end;
																
			                          (* TODO: a verifier *)
			                        clients := List.map (fun x -> if x.socket = s_descr then
																									begin
																				 					x.score := !score;
																									x.motsproposes := !(x.motsproposes) ^ " " ^  mot
																									end;
																									x
																								) !clients;
															tour#add_word mot;
															reponse := "MVALIDE/" ^ mot ^ "\n"
			                    end
	                  else
	                     reponse := "MINVALIDE/ le mot renseigné n'existe pas dans la langue française \n"   
	                end
	            else
	                reponse := "MINVALIDE/le mot renseigné est different de celui qu'on trouve avec votre trajectoire\n" 
	         end
      else
        begin
            reponse := "MINVALIDE/FAUSSE TRAJECTOIRE\n"  
        end;
        output_string out_chan !reponse;
        flush out_chan
      
      

	(* envoie de message public *)
	method broadcast_message msg = 
		ignore(
			let message = "RECEPTION/" ^ msg ^ "\n" in
			List.map (fun x -> output_string x.outchan message;
        			flush x.outchan) !clients)
							
	method send_message_to msg user = 
		ignore(
		try
				let message = "PRECEPTION/" ^ msg ^ "/" ^ user ^ "\n" and
				 client = List.find (fun x -> x.user = user) !clients in
				output_string client.outchan message;
        flush client.outchan
		 with Not_found -> ())
			

    method treat_request message =
			match (List.nth message 0) with
        "CONNEXION" ->  
                       if (List.length !clients = 0) then 
												begin
													ignore (Thread.create (fun x -> tour#start_tour 1)());
													self#start_session ()
												end;
												
												self#connect (List.nth message 1);						
                       self#signal_connexion (List.nth message 1)
												
        | "TROUVE" -> self#trouve (List.nth message 1) (List.nth message 2)
                                       
        | "SORT" -> self#deconnect (List.nth message 1)
				| "ENVOI" -> 	self#broadcast_message (List.nth message 1)													
														

				| "PENVOI" -> self#send_message_to (List.nth message 1) (List.nth message 2)
														
        | _ -> let message = "Commande Invalide\n" in
                	output_string out_chan message;
            			flush out_chan


    method run () =
      try 
        while true do
        let ligne = input_line inchan
        in if (ligne = "") || (ligne = "\013") then raise Fin ;
        let message = String.split_on_char '/' (String.trim ligne) in
          self#treat_request  message
        done
      with
      Fin -> ()
      | exn -> print_string (Printexc.to_string exn) ; print_newline()
        
end ;;
