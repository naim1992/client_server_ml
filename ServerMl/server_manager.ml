open Server;;
open Connexion_manager;;


let expiration () = Thread.delay 10.0;
		print_endline "fin de temps repartie";
		Thread.exit ();;

class server_maj port n =
   object(self)
   inherit server port n

	 method  wait_expiration  () = Thread.create (fun x -> expiration ())()
		
   method treat s sa =
   ignore( (new connexion_maj s sa true clients tirage dictionnaire)#start());
	 ignore (self#wait_expiration ())
   end;;