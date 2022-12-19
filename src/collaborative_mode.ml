open Core 
open Rating 
open Utils

let learningRate = 0.01;;
let regularization = 0.05;;
let factorNum = 30;;

type svd_param = {
  avg: float;
  bua: float array;
  bia: float array;
  pua: float array array;
  qia: float array array;
}

let split_train_test_data (rating_list: Rating.t) ~(test_size: float) : Rating.t * Rating.t = 
  let size = test_size *. (Float.of_int @@ List.length rating_list) |> Float.iround_nearest_exn in 
  shuffle rating_list |> Fn.flip List.split_n size

let average_rating (rating_list: Rating.t) = 
  let cnt = List.length rating_list in
  let sum = List.fold rating_list ~init:0.0 ~f:(fun acc elt -> acc +. elt.rating)  in 
  sum /. (Float.of_int cnt)

let inner_product (vect1: float array) (vect2: float array) : float = 
  Array.zip_exn vect1 vect2 
  |> Array.fold ~init:0.0 ~f:(fun acc (elt1, elt2) -> acc +. (elt1 *. elt2) )

let predict ~(svdp: svd_param) (elt: rating) : float = 
  let pred = svdp.avg 
    +. svdp.bua.(elt.userid)
    +. svdp.bia.(elt.movieid)
    +. inner_product svdp.pua.(elt.userid) svdp.qia.(elt.movieid) in 
  if Float.(pred < 1.0) then 1.0 
  else if Float.(pred > 5.0) then 5.0 
  else pred

let test_model ~(svdp: svd_param) (test_list: Rating.t) : float = 
  let cnt = List.length test_list in 
  let rmse = List.fold test_list ~init:0.0 ~f:(fun acc elt -> 
    let pred = predict ~svdp elt in 
    acc +. Float.int_pow (elt.rating -. pred) 2 
    ) in 
  Float.sqrt (rmse /. (Float.of_int cnt))

let rec update_pu_qi (elt: rating) ~(times:int) ~(svdp: svd_param) 
  ~(learningRate:float) ~(regularization:float) ~(eui:float) : unit = 
  if times = 0 then ()
  else 
    let k = times - 1 in
    let userid = elt.userid in 
    let itemid = elt.movieid in
    let tmp = svdp.pua.(userid).(k) in 
    svdp.pua.(userid).(k) <- svdp.pua.(userid).(k) +. 
    learningRate *. (eui *. svdp.qia.(itemid).(k) -. regularization *. svdp.pua.(userid).(k));
    svdp.qia.(itemid).(k) <- svdp.qia.(itemid).(k) +. 
    learningRate *. (tmp *. eui -. regularization *. svdp.qia.(itemid).(k));
    update_pu_qi ~times:(k) ~svdp ~learningRate ~regularization ~eui elt

let iterate_train (train_list: t) ~(svdp: svd_param)  
  ~(learningRate:float) ~(regularization:float) ~(factorNum:int) : unit = 
  List.iter train_list ~f:(fun elt -> 
    let userid = elt.userid in 
    let itemid = elt.movieid in 
    let pred = predict ~svdp elt in 
    let eui = elt.rating -. pred in 
    svdp.bua.(userid) <- svdp.bua.(userid) +. learningRate *. (eui -. (regularization *. svdp.bua.(userid)));
    svdp.bia.(itemid) <- svdp.bia.(itemid) +. learningRate *. (eui -. (regularization *. svdp.bia.(itemid)));
    update_pu_qi elt ~times:factorNum ~svdp ~learningRate ~regularization ~eui 
  )

let rec train_model ~(train_list: Rating.t) ~(test_list: Rating.t) ~(preRmse:float) ~(svdp: svd_param) ~(iterTimes:int) ~(learningRate:float) ~(regularization:float) ~(factorNum:int) : unit =
  if iterTimes = 0 then () 
  else (
    iterate_train train_list ~svdp ~learningRate ~regularization ~factorNum;
    let curRmse = test_model test_list ~svdp in 
    (* print_endline @@ Float.to_string curRmse; *)
    if Float.(curRmse > preRmse) then ()
    else train_model ~train_list ~test_list ~preRmse:curRmse ~svdp ~iterTimes:(iterTimes-1) ~learningRate ~regularization ~factorNum
  )

let getNum_helper (lst: 'a list) : int =
  lst 
  |> List.sort ~compare:Int.compare 
  |> List.group ~break:(<>)
  |> List.length

let get_userNum (all_list: Rating.t) : int = 
  List.map all_list ~f:(fun elt -> elt.userid) 
  |> getNum_helper

let get_movieNum (all_list: Rating.t) : int = 
  List.map all_list ~f:(fun elt -> elt.movieid) 
  |> getNum_helper

let mapping_helper (lst: 'a list) : (int, int) Hashtbl.t =
  lst   
  |> List.sort ~compare:Int.compare 
  |> List.group ~break:(<>)
  |> List.mapi ~f:(fun idx -> fun elt -> (List.hd_exn elt, idx))
  |> Hashtbl.of_alist_exn (module Int)

let mapping_userid_idx (all_list: Rating.t) : (int, int) Hashtbl.t  = (* userid, index *)
  List.map all_list ~f:(fun elt -> elt.userid) |> mapping_helper

let mapping_movieid_idx (all_list: Rating.t) : (int, int) Hashtbl.t  = (* movieid, index *)
  List.map all_list ~f:(fun elt -> elt.movieid) |> mapping_helper

let mapping_idx_movieid (all_list: Rating.t) : (int, int) Hashtbl.t = (* index, movieid *)
  mapping_movieid_idx all_list   
  |> Hashtbl.to_alist
  |> List.map ~f:(fun (a,b) -> b, a)
  |> Hashtbl.of_alist_exn (module Int)

let get_random ~(x:float) : float = 
  let r = Random.float 1.0 in 
  let rr = (if Float.(r = 0.0) then 0.1
    else if Float.(r = 1.0) then 0.9
    else r) in 
  0.1 *. rr /. x

let generate_random_matrix (row:int) (col:int) ~(x:float) : float array array = 
  let iteror = List.range 0 col in
  let matrix = Array.make_matrix 0.0 ~dimx:row ~dimy:col in
   Array.iter matrix ~f:(fun one_line -> 
    List.iter iteror ~f:(fun i -> one_line.(i) <- (get_random ~x))
    ); matrix

let init_model (train_list: Rating.t) ~(userNum:int) ~(movieNum:int) ~(factorNum:int): svd_param = 
  let tmp = Float.sqrt @@ Float.of_int factorNum in
  Random.self_init ();
  {
    avg = average_rating train_list;
    bua = Array.create 0.0 ~len:userNum;
    bia = Array.create 0.0 ~len:movieNum;
    pua = generate_random_matrix userNum factorNum ~x:tmp;
    qia = generate_random_matrix movieNum factorNum ~x:tmp;
  }

let transfer_list (rating_list: Rating.t) : Rating.t = 
  let userid_index_map = mapping_userid_idx rating_list in 
  let movieid_index_map = mapping_movieid_idx rating_list in 
  List.map rating_list ~f:(fun elt -> 
    {
      userid = Hashtbl.find_exn userid_index_map elt.userid;
      movieid = Hashtbl.find_exn movieid_index_map elt.movieid;
      rating = elt.rating;
    }
    ) 

let run_biasSVD ~(all_list: Rating.t) ~(factorNum:int) :svd_param =  
  let test_list, train_list = split_train_test_data all_list ~test_size:0.3 in 
  let userNum = get_userNum all_list in 
  let movieNum = get_movieNum all_list in
  let svdp = init_model train_list ~userNum ~movieNum ~factorNum in 
  train_model ~train_list ~test_list ~preRmse:1000.0 ~svdp ~iterTimes:100 ~learningRate:0.01 ~regularization:0.05 ~factorNum;
  svdp

let movieid_filter_helper ~(ur_list:Rating.t) (movieid:int) : bool =
  List.exists ur_list ~f:(fun elt -> elt.movieid = movieid)

let get_movieid_list ~(all_list: Rating.t) ~(ur_list: Rating.t) : int list =
  List.map all_list ~f:(fun elt -> elt.movieid) 
  |> List.sort ~compare:Int.compare 
  |> List.group ~break:(<>) 
  |> List.map ~f:(fun grp -> List.hd_exn grp)
  |> List.filter ~f:(fun id -> not @@ movieid_filter_helper ~ur_list id)

let predict_user ~(svdp:svd_param) ~(userid:int) ~(movieid_list: int list) : (int * float) list = 
  List.map movieid_list ~f:(fun movieid -> (
    let elt = {
      userid=userid;
      movieid=movieid;
      rating=0.0;
      } in 
    let pred = predict ~svdp elt in 
    movieid, pred
    ))

let get_recommendations ~(movie_list:Movie.t) ~(rating_list:Rating.t) ~(ur_list:Rating.t) ~(n: int) : Movie.t =
  let all_list = List.append rating_list ur_list in 
  let transferred_all_list = transfer_list all_list in
  let userid_index_map = mapping_userid_idx all_list in 
  let user_idx = (List.hd_exn ur_list).userid |> Hashtbl.find_exn userid_index_map in 
  let movieid_list = get_movieid_list ~all_list ~ur_list in 
  let movieid_index_map = mapping_movieid_idx all_list in
  let index_movieid_map = mapping_idx_movieid all_list in
  let movieidx_list = List.map movieid_list ~f:(fun elt -> Hashtbl.find_exn movieid_index_map elt) in
  let svdp = run_biasSVD ~all_list:transferred_all_list ~factorNum:factorNum in 
  predict_user ~svdp ~userid:user_idx ~movieid_list:movieidx_list 
  |> List.sort ~compare:(fun (_,score1) -> fun (_, score2) -> Float.descending score1 score2)
  |> Fn.flip List.take n 
  |> List.map ~f:(fun (i, _) -> Movie.find_movie_by_movieid ~movie_list (Hashtbl.find_exn index_movieid_map i))