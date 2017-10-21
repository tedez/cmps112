let rcsid = "$Id: ncat.ml,v 330.3 2003-02-04 17:09:21-08 - - $"
    ;;

(*
* NAME
*    ncat - list files to the standard output
*
* SYNOPSIS
*    ncat [filename...]
*
* DESCRIPTION
*    Each file given in the argument list is copied to the standard
*    output, in sequence, preceded by a file header.  Non-printing
*    characters ([\x00-\x1F\x7F-\xA0], except \n and \t) are printed
*    in hexadecimal.  If no filenames are given, stdin is copied.
*)

let stack = Stack.create ()

let complain message =
    ( flush stdout;
      Printf.eprintf "%s: %s\n" Sys.argv.(0) message;
      flush stderr );;

let display byte =
    let printable = byte = '\t'
                 || byte = '\n'
                 || ' ' <= byte && byte <= '~'
                 (* || '¡' <= byte && byte <= 'ÿ' *)
    in  if printable
        then Printf.printf "%c" byte
        else Printf.printf "\\x%02X" (int_of_char byte)
    ;;

let pushlines readline =
    let rec pushloop linecount =
        try  let nextline = readline ()
             in( (* String.iter display nextline; *)
                 (* print_newline (); *)
                 Stack.push nextline stack;
                 pushloop (linecount + 1) )
        with End_of_file -> flush stdout (* this is bugging me, switching to None gives an error. *)
        (* with End_of_file -> None *)
    in  pushloop 1
    ;;

let iterfile filename =
    try 
    	let file = open_in filename in
        ( pushlines (fun () -> input_line file) )
    with 
    	Sys_error message -> complain message
    ;;

let listfiles () =
    let argvfiles = Array.length Sys.argv - 1 in
      	if argvfiles = 0
        then pushlines read_line
        else Array.iter iterfile (Array.sub Sys.argv 1 argvfiles)
    ;;

if not ! Sys.interactive then listfiles ();;

