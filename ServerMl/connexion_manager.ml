open Connexion;;

type infos = {user : string; socket : Unix.file_descr; score : int ref; outchan : out_channel };;

exception Fin ;;

(* pour la gestion de temps*)
let gestion_temps clients = 
  Thread.delay 0.30;
  List.map (fun x -> Unix.close x.socket) clients;;

  (*recuperer la les scores*)
let scores list = 
  let elements  = ref "" in
      List.map (fun x -> elements := !elements ^ ";" ^ (string_of_int !(x.score))) list;
    !elements ;; 


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

   
(* verifier la trajectoire *)
let verif_traj traj = 
  let verifier = ref true and i = ref 0 in
  if (String.length traj) mod 2 == 0 then
  begin
    while (!i+2) < (String.length traj) && !verifier == true do
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
    'A' -> tab.(0).(int_of_char case.[1])
    | 'B' -> tab.(1).(int_of_char case.[1])
    | 'C' -> tab.(2).(int_of_char case.[1])
    | 'D' -> tab.(3).(int_of_char case.[1])
    in
  lettre;;


(*pour trouver un mot selon sa trajctoire*)
let find traj tab = 
  let i = ref 0 and mot = "" in
  if (String.length traj) mod 2 == 0 then  
  begin
     while (!i+2) < String.length traj do
        mot = mot ^ (word_in_case (String.sub traj !i (!i+2)) tab);
        i := !i + 2
      done
  end;
    mot;;
  

class connexion_maj sd sa b clients tirage dictionnaire =
object(self)
inherit connexion sd sa b clients tirage dictionnaire
    
    val score = ref 0
    val inchan = Unix.in_channel_of_descr sd
    val out_chan = Unix.out_channel_of_descr sd

    method deconnect user = 
          clients := List.filter (fun x -> x.socket <> s_descr) !clients;
          let message =  "DECONNEXION/" ^ user ^ "\n" in
          List.map (
                      fun x -> 
                                output_string x.outchan message;
                								flush x.outchan;
                    ) !clients;
          Unix.close s_descr


    method already_connected client = 
          List.exists (fun x -> String.equal x.user client) !clients

(* Signalement de la connexion de ’user’ aux autres clients. *)
method signal_connexion client = 
  let others = List.filter (fun x -> x.socket <> s_descr) !clients in
  let message = "CONNECTE/" ^ client ^ "\n"   in
  List.map (
              fun x ->  
								output_string x.outchan message;
                flush x.outchan;          
              ) others;
  ()

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
                clients := !clients@[{user = client; socket = s_descr; score = ref 0; outchan = out_chan}];
                print_endline ("new Connexion from : " ^ client);

                let message = "Bienvenue : " ^ client ^ "\n" in
                        output_string out_chan message;
                        flush out_chan
              
            end 

    (* debut d'une session *)
    method start_session () = 
            let message = "SESSION/" ^ (array_to_string tirage) ^ "\n" in
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
    let reponse =  "" in  
      if verif_traj trajectoire == true then
          if String.equal mot (find trajectoire tirage) then
                begin
                  (* verifier l'existance d'un mot dans le dictionnaire*)
                  if (List.exists (fun x -> x = mot) dictionnaire) then
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
                          end;
                          (* TODO: a verifier *)
                          List.map (fun x -> if x.socket = s_descr then x.score := !score ) !clients;
                          reponse = "MVALIDE/" ^ mot ^ "\n" 
                    end
                      else
                        reponse = "MINVALIDE/ le mot renseigné n'existe pas dans la langue française \n"   
                end
            else
                reponse = "MINVALIDE/le mot renseigné est different de celui qu'on trouve avec votre trajectoire\n" 
              
      else
        begin
            reponse = "MINVALIDE/FAUSSE TRAJECTOIRE\n"  
        end;
        output_string out_chan reponse;
        flush out_chan    
      

    method treat_request message =
      match (List.nth message 0) with
        "CONNEXION" -> self#connect (List.nth message 1);
                       self#signal_connexion (List.nth message 1); 
                       if (List.length !clients = 1) then 
												begin
													self#start_session ()
												end;
													self#nouveau_tour ()
												
												
        | "TROUVE" -> print_endline ""(* self#trouve (List.nth message 1) (List.nth message 2) *)
                                       
        | "SORT" -> print_endline ("deconnexion of : " ^ List.nth message 1);
                            self#deconnect (List.nth message 1)
        | _ -> let message = "Commande Invalide\n" in
                	output_string out_chan message;
            			flush out_chan


    method run () =
      try 
        while true do
        let ligne = input_line inchan
        in if (ligne = "") || (ligne = "\013") then raise Fin ;
        let message = String.split_on_char '/' ligne in
          self#treat_request message
        done
      with
      Fin -> ()
      | exn -> print_string (Printexc.to_string exn) ; print_newline()
        
end ;;
