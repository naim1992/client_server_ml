open Server_manager;;
(*
   val main : unit -> unit
   *)
  
   let main () =
    if Array.length Sys.argv < 3
    then Printf.printf "usage : server port num\n"
    else
    let port = int_of_string(Sys.argv.(1))
    and n = int_of_string(Sys.argv.(2)) in
    (new server_maj port n )#start();;
   
    main();;