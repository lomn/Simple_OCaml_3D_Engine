#load "str.cma"

(*Verry not optimized way (I mean not at all) to get the number of lines of a given file*)
let file_length file = 
	let lines = ref [] and a = ref 0 in
	try
  	while true; do
    	lines := input_line file :: !lines;
    	a := !a + 1;
  	done; !a
	with End_of_file ->
  		seek_in file 0;
  		!a;;

let import_file file_name separator =
  let reg_separator = Str.regexp separator in
  let i = ref 0 in
  try
    let ic = open_in file_name in
	let value_array = Array.make_matrix (file_length ic) 9 0. in
    try
      while true; do
        (* Create a list of values from a line *)
        let line_list = Str.split reg_separator (input_line ic) in
        List.iteri
  			(fun j elem -> value_array.(!i).(j) <- float_of_string elem)
  			line_list;
        i := !i + 1
      done;
      value_array
    with 
      | End_of_file -> close_in ic; value_array
    with
      | e -> raise e;;


let file_to_face_mat filename =
	let arr = import_file filename "," in
	let mat = Array.make_matrix 3 ((Array.length arr) * 3) 0. in
	for k = 0 to (Array.length arr)-1 do
		for i = 0 to (Array.length arr.(k))-1 do
			mat.(i mod 3).(k+i/3) <- arr.(k).(i);
		done;
	done;
	mat;;















