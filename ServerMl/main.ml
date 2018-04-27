open Server_manager;;
(*
   val main : unit -> unit
   *)
  
   let main () =
    
		(*if Array.length Sys.argv < 3
    then Printf.printf "usage : server port num\n"
    else
    let port = int_of_string(Sys.argv.(1))
    and n = int_of_string(Sys.argv.(2)) 
		in
    (new server_maj port n )#start();;
		*)
		if Sys.argv.(1) = "-port" 
		&& Sys.argv.(3) = "-tours" 
		(* &&  Sys.argv.(5) = "-grilles" *)
		  then
			let port = int_of_string(Sys.argv.(2)) 
			and nbtour = int_of_string(Sys.argv.(4))
			in
				(new server_maj port nbtour)#start()
		else
			print_endline "usage : server -port port -tours nbtour";;
   
    main();;