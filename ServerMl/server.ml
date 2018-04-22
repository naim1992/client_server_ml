

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
      


class virtual server port n = 
object (s)
val port_num = port
val nb_pending = n
val clients = ref []
val dictionnaire = read_file "dictionnaire.dat"
(*tirage : matrice 4*4 *)
val tirage = des ()
val sock = ThreadUnix.socket Unix.PF_INET Unix.SOCK_STREAM 0



method start () =
  let host = Unix.gethostbyname (Unix.gethostname ()) in 
  (* let host = Unix.gethostbyname (
                        Unix.string_of_inet_addr (
                        Unix.inet_addr_of_string  "132.227.112.144")) in *)
  let h_addr = host.Unix.h_addr_list.(0) in
  let sock_addr = Unix.ADDR_INET(h_addr, port_num) in
  Unix.setsockopt sock Unix.SO_REUSEADDR true; 
  Unix.bind sock sock_addr;
  Unix.listen sock n;

  print_endline ("serveur lancé sur : " ^ Unix.string_of_inet_addr h_addr);
  while true do
    let (service_sock, client_sock_addr) = 
      ThreadUnix.accept sock
      in
      s#treat service_sock client_sock_addr
  done
  method virtual treat : Unix.file_descr -> Unix.sockaddr -> unit
end;;

 



  
  
 
