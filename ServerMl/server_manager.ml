open Server;;
open Connexion_manager;;

class server_maj port n =
   object(s)
   inherit server port n

   method treat s sa =
   ignore( (new connexion_maj s sa true clients tirage dictionnaire)#start())
   end;;