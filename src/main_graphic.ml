#load "graphics.cma";;
#load "random.cma";;
#use "file_util.ml";;
#use "math_util.ml";;
open Graphics;;

Random.init 24953846394638;;

Graphics.open_graph " 700x700";;
Graphics.set_window_title "3D Engine POC";;


(* set background color to grey *)
let background = 0x222222;;
set_color background;;
fill_poly [|(0,0); (0,size_y ()); (size_x (), size_y ()); (size_x (), 0)|];;
set_color white;;


(*Some sleep function I wrote for convinience*)
let sleep n = for k = 0 to n do () done;;

(*Connect every vertices of a given shape together*)
(*The shape is describe by a matrix of vertices*)
let connect_3d_shape arr =
	for i = 0 to (Array.length arr)-1 do
		for  j = 0 to (Array.length arr)-1 do
			draw_poly [| arr.(i); arr.(j) |];
		done;
	done;;

(*Draws faces of a shape, 
 which is described by an array of triangles*)
let draw_shape_tri arr =
	for k = 0 to ((Array.length arr)/3)-1 do
		if (k mod 2) = 0 
		then set_color green
		else set_color yellow;
		set_color white;
		draw_poly [| arr.(3*k) ; arr.(3*k+1) ; arr.(3*k+2) |];
	done;;

(*Draw shape tri but with depth notion
   Same as above though here the color of a given triangle is defined by it's distance
   to the camera*)
let draw_shape_tri_depth arr =
	let n = ((Array.length arr.(0))/3) in
	let mat = Array.init n (fun _ -> Array.init 3 (fun _ -> (Array.make 3 0))) and
		faces = Array.make_matrix n 2 0 in

	for i = 0 to n-1 do
		mat.(i).(0).(0) <- int_of_float arr.(0).(3*i);
		mat.(i).(0).(1) <- int_of_float arr.(1).(3*i);
		mat.(i).(0).(2) <- int_of_float arr.(2).(3*i);

		mat.(i).(1).(0) <- int_of_float arr.(0).(3*i+1);
		mat.(i).(1).(1) <- int_of_float arr.(1).(3*i+1);
		mat.(i).(1).(2) <- int_of_float arr.(2).(3*i+1);

		mat.(i).(2).(0) <- int_of_float arr.(0).(3*i+2);
		mat.(i).(2).(1) <- int_of_float arr.(1).(3*i+2);
		mat.(i).(2).(2) <- int_of_float arr.(2).(3*i+2);

		faces.(i).(0) <- (centre_tri_arr_z mat.(i).(0) mat.(i).(1) mat.(i).(2));
		faces.(i).(1) <- i;
	done;
	quicksort_mat faces;

	for k = 0 to (n-1)/2 do
		let ji = faces.(k).(1) in
		set_color (abs(faces.(k).(0))/2);
		(*set_color (Random.int 1000000);*)
		fill_poly [|(mat.(ji).(0).(0) , mat.(ji).(0).(1)); 
					(mat.(ji).(1).(0) , mat.(ji).(1).(1));
					(mat.(ji).(2).(0) , mat.(ji).(2).(1))|];
	done;;



(*Rotates a given file by the angle theta around the x and y axis also multiply its size*)
let draw_rot_file filename theta = 
	let shape = (mult_mat_scalar (file_to_face_mat filename) 200.) in
	draw_shape_tri (mat_to_array_plt 
		(translate ((rotation_y (rotation_x shape theta) theta)) 500. 500. 500.));;

(*Simple animation which rotates a given file in the custom format ascstl*)
let anim2 n filename =
	let shape = (rotation_x (mult_mat_scalar (file_to_face_mat filename) 200.) (-.3.14/.2.)) and theta = ref 0. in
	let temp = ref (mat_to_array_plt (translate ((rotation_y (rotation_x shape !theta) (!theta*.1.5))) 300. 300. 300.)) in
	for k = 0 to n do
		theta := ((float_of_int k) *. 6.28 /. (float_of_int n));
		temp := (mat_to_array_plt 
		(translate ((rotation_y (rotation_x shape !theta) (!theta*.1.5))) 300. 300. 300.));
		clear_graph ();
		set_color background;
		fill_poly [|(0,0); (0,size_y ()); (size_x (), size_y ()); (size_x (), 0)|];
		set_color white;
		draw_shape_tri !temp;
	done;;

(*Plots the given file*)
let plot_file filename =
	let shape = (mult_mat_scalar (file_to_face_mat filename) 200.) in
	clear_graph ();
	set_color background;
	fill_poly [|(0,0); (0,size_y ()); (size_x (), size_y ()); (size_x (), 0)|];
	set_color white;
	draw_shape_tri (mat_to_array_plt 
		(translate (rotation_x shape (-.3.14/.2.))
			(float_of_int (size_x ())/.2.) 500. (float_of_int (size_y ())/.2.)));;



(*Rotates a given file around the y axis*)
let rotation_y_demo n filename =
	let shape = (rotation_x (mult_mat_scalar (file_to_face_mat filename) 200.) (-.3.14/.2.)) and theta = ref 0. in
	let temp = ref (mat_to_array_plt (translate ((rotation_y (rotation_x shape !theta) (!theta*.1.5))) 300. 300. 300.)) in
	for k = 0 to n do
		theta := ((float_of_int k) *. 6.28 /. (float_of_int n));
		temp := mat_to_array_plt (translate (rotation_y shape !theta) 300. 300. 100.);
		sleep 9999;
		clear_graph ();
		set_color background;
		fill_poly [|(0,0); (0,size_y ()); (size_x (), size_y ()); (size_x (), 0)|];
		set_color white;
		draw_shape_tri !temp;
	done;;

let rotation_full_demo n filename =
	let shape = (rotation_x (mult_mat_scalar (file_to_face_mat filename) 200.) (-.3.14/.2.)) and theta = ref 0. in
	let temp = ref ((translate ((rotation_y (rotation_x shape !theta) (!theta*.1.5))) 300. 300. 300.)) in
	for k = 0 to n do
		theta := ((float_of_int k) *. 6.28 /. (float_of_int n));
		temp := (translate (rotation_y shape !theta) 300. 300. 100.);
		sleep 9999;
		clear_graph ();
		set_color background;
		fill_poly [|(0,0); (0,size_y ()); (size_x (), size_y ()); (size_x (), 0)|];
		set_color white;
		draw_shape_tri_depth !temp;
	done;;

let depth_full_demo n filename =
	let shape = (rotation_x (mult_mat_scalar (file_to_face_mat filename) 200.) (-.3.14/.2.)) and theta = ref 0. in
	let temp = ref ( (translate ((rotation_y (rotation_x shape !theta) (!theta*.1.5))) 300. 300. 300.)) in
	for k = 0 to n do
		theta := ((float_of_int k) *. 6.28 /. (float_of_int n));
		temp := ( 
		(translate ((rotation_y (rotation_x shape !theta) (!theta*.1.5))) 300. 300. 300.));
		clear_graph ();
		set_color background;
		fill_poly [|(0,0); (0,size_y ()); (size_x (), size_y ()); (size_x (), 0)|];
		set_color white;
		draw_shape_tri_depth !temp;
	done;;


depth_full_demo 500 "../res/converted/sphere.stl.custom";;
rotation_y_demo 500 "../res/converted/cat.stl.custom";;
anim2 150 "../res/converted/monkey.stl.custom";;
