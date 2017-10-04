module Make (F: Field.FieldType) => {
  type t = array (array F.t);
  let make (a: array (array F.t)) :t => a;
  let const m n (x: F.t) => Array.make_matrix m n x;
  let zero m n => const m n F.zero;
  let swap (i: int) (j: int) (a: t) :t =>
    a
    |> Array.mapi (
         fun k row =>
           if (k == i) {
             a.(j)
           } else if (k == j) {
             a.(i)
           } else {
             row
           }
       );
  let scale (i: int) (x: F.t) (a: t) :t =>
    a
    |> Array.mapi (
         fun k row =>
           if (k == i) {
             Array.map (fun y => F.mul x y) row
           } else {
             row
           }
       );
  let transvect (i: int) (j: int) (x: F.t) (a: t) :t =>
    a
    |> Array.mapi (
         fun k row =>
           if (k == j) {
             Array.mapi (fun l y => F.mul x a.(i).(l) |> F.add y) row
           } else {
             row
           }
       );
  let leadingColumns (a: t) :array int =>
    a |> Array.map (fun row => row |> Js_array.findIndex ((!==) F.zero));
  /*    |> Array.map (fun x => x == (-1) ? None : Some x);*/
  let swapFirstRowWith (a: t) :int => {
    let f (acc: int) (x: int) :int => x == (-1) ? acc : acc == (-1) ? x : min acc x;
    let m = a |> leadingColumns |> Array.fold_left f (-1);
    a |> leadingColumns |> Js_array.findIndex ((==) m)
  };
  let scaleFirstRow a => {
    let j = (leadingColumns a).(0);
    a |> scale 0 (F.inv a.(0).(j))
  };
  let clearLeadingColumn a => {
    let j = (leadingColumns a).(0);
    let f acc _ i =>
      i > 0 && a.(i).(j) !== F.zero ? acc |> transvect 0 (i + 1) (F.neg a.(i + 1).(j)) : acc;
    a |> Js_array.reducei f a
  };
  let rec rowEchelonForm a => {
    let m = Array.length a;
    let i = swapFirstRowWith a;
    if (i == (-1)) {
      a
    } else {
      let b = a |> swap 0 i |> scaleFirstRow |> clearLeadingColumn;
      m == 1 ? b : Array.concat [[|b.(0)|], Array.sub b 1 (m - 1) |> rowEchelonForm]
    }
  };
  let toString (a: t) :string =>
    a
    |> Array.map (fun row => row |> Array.map F.toString |> Array.to_list |> String.concat "   ")
    |> Array.to_list
    |> String.concat "\n";
  let toTex (a: t) :string =>
    {|\begin{bmatrix}|}
    ^ (
      a
      |> Array.map (fun row => row |> Array.map F.toString |> Array.to_list |> String.concat "&")
      |> Array.to_list
      |> String.concat {|\\|}
    )
    ^ {|\end{bmatrix}|};
  let printe (a: t) :unit => a |> toString |> print_endline;
};