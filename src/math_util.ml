(* Identity matrix n*n *)
let id_ n =
	let t = Array.make_matrix n n 0. in
	for k = 0 to n-1 do
		t.(k).(k) <- 1.;
	done;
	t;;


(* Multiply the matrix by a number *)
let mult_mat_scalar mat x =
	let t1 = (Array.make_matrix (Array.length mat) (Array.length mat.(0)) 0.)  in
	for i = 0 to (Array.length mat)-1 do
		for j = 0 to (Array.length mat.(0))-1 do
			t1.(i).(j) <- mat.(i).(j) *. x
		done;
	done;
	t1;;

(*Print the given matrix into the terminal, only works for litle matrices*)
let print_matrice mat = 
	for i = 0 to (Array.length mat)-1 do
		for j = 0 to (Array.length mat.(0))-1 do
		 print_string "  "; print_float mat.(i).(j)
		done;
		print_string "\n";
	done;;


(* We override fundamental operations of arrays so that it complies with matrices operations*)



(* Naive/most simple/ineficient multiplication of two matricies *)
let ( ** ) x y =
  let x0 = Array.length x
  and y0 = Array.length y in
  let y1 = if y0 = 0 then 0 else Array.length y.(0) in
  let z = Array.make_matrix x0 y1 0. in
  for i = 0 to x0-1 do
    for j = 0 to y1-1 do
      for k = 0 to y0-1 do
        z.(i).(j) <- z.(i).(j) +. x.(i).(k) *. y.(k).(j)
      done
    done
  done;
  z;;

(* Sum the two given matrices *)
let ( ++ ) mA mB =
 	if Array.length mA != Array.length mB || Array.length mA.(0) != Array.length mB.(0) 
 	then  failwith "can't sum of two matrices of different size";
 	let mR = Array.make_matrix (Array.length mA) (Array.length mA.(0)) 0. in
 	for k = 0 to (Array.length mA)-1 do
 		for i = 0 to (Array.length mA.(0))-1 do
 			mR.(k).(i) <- mA.(k).(i) +. mB.(k).(i);
 		done;
 	done;
 	mR;;


(*-_-_-__-_-_-__-_-_-__-_-_-_-__-_-_-_-__-_-_-__-_-*)


(* Projector of a given matrix onto the OxOy plan *)
let projector_on_OxOy arr =
	let proj = Array.make (Array.length arr.(0)) (0.,0.) in
	for k = 0 to (Array.length arr.(0))-1 do
		proj.(k) <- (arr.(0).(k),arr.(1).(k));
	done;
	proj;;

(* Convert a given matrix into a "plotable" array by projecting it onto the OxOy plan. *)
(* this function returns an array of triangles (result) to be plotted on screen *)
let mat_to_array_plt mA =
	let p = (projector_on_OxOy mA) and 
	result = Array.make (Array.length mA.(0)) (0,0) in
	for k = 0 to (Array.length p)-1 do
		result.(k) <- match p.(k) with |(a,b) -> (int_of_float (floor (a+.0.5)), int_of_float (floor (b+.0.5)));
	done;
	result;;

let mat_to_array_plt_zm mA =
	let p = (projector_on_OxOy mA) and 
	result = Array.make (Array.length mA.(0)) (0,0) in
	for k = 0 to (Array.length p)-1 do
		result.(k) <- match p.(k) with 
			|(a,b) -> (int_of_float (floor (a+.0.5)), int_of_float (floor (b+.0.5)));
	done;
	result;;

(*Rotation matrix of an angle theta in radians around the x axis*)
let matRx theta =
	let mat = Array.make_matrix 3 3 0. in
	mat.(0).(0) <- 1.;
	mat.(1).(1) <- cos theta;
	mat.(1).(2) <- -. sin theta;
	mat.(2).(1) <- sin theta;
	mat.(2).(2) <- cos theta;
	mat;;

(*See matRx comment*)
let matRy theta =
	let mat = Array.make_matrix 3 3 0. in
	mat.(0).(0) <- cos theta;
	mat.(0).(2) <- sin theta;
	mat.(1).(1) <- 1.;
	mat.(2).(0) <- -. sin theta;
	mat.(2).(2) <- cos theta;
	mat;;

(* See matRx comment *)
let matRz theta =
	let mat = Array.make_matrix 3 3 0. in
	mat.(0).(0) <- cos theta;
	mat.(0).(1) <- -. sin theta;
	mat.(1).(0) <- sin theta;
	mat.(1).(1) <- cos theta;
	mat.(2).(2) <- 1.;
	mat;;


(* Transposition of a matrix *)
let transpose mat =
	let tmat = Array.make_matrix (Array.length mat.(0)) (Array.length mat) 0. in
	for i = 0 to (Array.length mat)-1 do
		for j = 0 to (Array.length mat.(0))-1 do
			tmat.(j).(i) <- mat.(i).(j);
		done;
	done; tmat;;

(* Extract the k-1 vector of a matrix *)
let extract_vect mat k =
	let mat_temp = (Array.make_matrix (Array.length mat) 1 0.) in
	for i = 0 to (Array.length mat)-1 do
		mat_temp.(i).(0) <- mat.(i).(k);
	done;
	mat_temp;;

(* Translation by x,y,z *)
let translate mat x y z =
	let mat_temp = Array.make_matrix (Array.length mat) (Array.length mat.(0)) 0. in
	for k = 0 to (Array.length mat.(0))-1 do
		mat_temp.(0).(k) <- mat.(0).(k) +. x;
	done;
	for k = 0 to (Array.length mat.(0))-1 do
		mat_temp.(1).(k) <- mat.(1).(k) +. y;
	done;
	for k = 0 to (Array.length mat.(0))-1 do
		mat_temp.(2).(k) <- mat.(2).(k) +. z;
	done;
	mat_temp;;

(* Rotates a given shape by the angle theta (radians) around the x axis *)
let rotation_x mat theta =
	let mat1 = Array.make_matrix (Array.length mat.(0)) (Array.length mat) 0. in
	for k = 0 to (Array.length mat.(0))-1 do
		mat1.(k) <- [|((matRx theta) ** (extract_vect mat k)).(0).(0); 
			((matRx theta) ** (extract_vect mat k)).(1).(0);
			((matRx theta) ** (extract_vect mat k)).(2).(0)|];
	done;
	let mat2 = Array.make_matrix (Array.length mat) (Array.length mat.(0)) 0. in
	for i = 0 to (Array.length mat)-1 do
		for j = 0 to (Array.length mat.(0))-1 do
			mat2.(i).(j) <- mat1.(j).(i);
		done;
	done;
	mat2;;

(* See rotation_x comment *)
let rotation_y mat theta =
	let mat1 = Array.make_matrix (Array.length mat.(0)) (Array.length mat) 0. in
	for k = 0 to (Array.length mat.(0))-1 do
		mat1.(k) <- [|((matRy theta) ** (extract_vect mat k)).(0).(0); 
			((matRy theta) ** (extract_vect mat k)).(1).(0);
			((matRy theta) ** (extract_vect mat k)).(2).(0)|];
	done;
	let mat2 = Array.make_matrix (Array.length mat) (Array.length mat.(0)) 0. in
	for i = 0 to (Array.length mat)-1 do
		for j = 0 to (Array.length mat.(0))-1 do
			mat2.(i).(j) <- mat1.(j).(i);
		done;
	done;
	mat2;;

(* See rotation_x comment *)
let rotation_z mat theta =
	let mat1 = Array.make_matrix (Array.length mat.(0)) (Array.length mat) 0. in
	for k = 0 to (Array.length mat.(0))-1 do
		mat1.(k) <- [|((matRz theta) ** (extract_vect mat k)).(0).(0); 
			((matRz theta) ** (extract_vect mat k)).(1).(0);
			((matRz theta) ** (extract_vect mat k)).(2).(0)|];
	done;
	let mat2 = Array.make_matrix (Array.length mat) (Array.length mat.(0)) 0. in
	for i = 0 to (Array.length mat)-1 do
		for j = 0 to (Array.length mat.(0))-1 do
			mat2.(i).(j) <- mat1.(j).(i);
		done;
	done;
	mat2;;

let centre z1 z2 z3 = (z1+.z2+.z3)/.3.;;

(* Compute the center of mass of a triangle *)
let centre_tri (x1,y1,z1) (x2,y2,z2) (x3, y3, z3) = 
	(x1+.x2+.x3)/.3. , (y1+.y2+.y3)/.3., (z1+.z2+.z3)/.3.;;

let centre_tri_arr_z [|x1;y1;z1|] [|x2;y2;z2|] [|x3; y3; z3|] = 
	(z1+z2+z3)/3;;


(* Implementation of the quicksort algorithm *)
(*-_-_-_-_-_-_-_-_-_-_-_*)


let echange arr i j = let x = arr.(i) in
  arr.(i) <- arr.(j); 
  arr.(j) <- x;;

let arrange t debut fin =
 let pivot = t.(debut).(0) and ind = ref debut in
  for k = debut+1 to fin do
    if t.(k).(0) < pivot then begin 
      ind := !ind + 1;
      echange t k !ind;
    end;
  done;
  echange t !ind debut;
  !ind;;

let rec sort t i j =
  if i < j then begin
  let ind = arrange t i j in
  sort t i (ind-1); sort t (ind+1) j; end
  else ();;

let quicksort_mat t = sort t 0 ((Array.length t)-1);;

let int_array_of_float_array arr =
	let tab = Array.make (Array.length arr) 0 in
	for k = 0 to (Array.length arr)-1 do
		tab.(k) <- int_of_float arr.(k)
	done;
	tab;;

