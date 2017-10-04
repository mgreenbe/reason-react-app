let rec findIndex f (ell: list int) =>
  switch ell {
  | [] => None
  | [a, ...b] =>
    f a == true ?
      Some 0 :
      (
        switch (findIndex f b) {
        | None => None
        | Some k => Some (k + 1)
        }
      )
  };

let trim = Js.String.trim;

let split = Js.String.split;

let map = Js.Array.map;

module Make (F: Field.FieldType) => {
  module V = VSpace.Make F;
  type t = array V.t;
  let make a :t => a |> Array.map V.make;
  let fromString s => {
    let parseRow row => row |> trim |> split " " |> map trim |> map F.fromString;
    s |> trim |> split "\n" |> map parseRow |> make
  };
  let const m n x => V.const n x |> Array.make m;
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
             V.smul x row
           } else {
             row
           }
       );
  let transvect (i: int) (j: int) (x: F.t) (a: t) :t =>
    a
    |> Array.mapi (
         fun k row =>
           if (k == j) {
             V.smul x a.(i) |> V.add row
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
      |> Array.map (fun row => row |> Array.map F.toTex |> Array.to_list |> String.concat "&")
      |> Array.to_list
      |> String.concat {|\\|}
    )
    ^ {|\end{bmatrix}|};
  let printe (a: t) :unit => a |> toString |> print_endline;
};