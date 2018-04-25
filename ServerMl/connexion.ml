
class virtual connexion sd (sa : Unix.sockaddr) b tour=
object (self)
 val s_descr = sd
 val s_addr = sa
 
 method start () = Thread.create (fun x -> self#run x ; self#stop x) ()
 method stop() = Unix.close s_descr
 method virtual run : unit -> unit
 end;;

 