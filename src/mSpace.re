exception SomethingWentWrong string;

/*let f x acc =>
  switch x {
  | None => acc
  | Some y => [y, ...acc]
  };*/
module Make (F: Field.FieldType) => {
  module V = VSpace.Make F;
  type t = list V.t;
  type entry = {
    i: int,
    j: int,
    value: F.t
  };
  type rowOp =
    | Swap int int
    | Scale F.t int
    | Transvect F.t int int;
  let make a :t => a |> List.map V.make;
  let fromString s =>
    s |> Js.String.trim |> Js.String.split "\n" |> Array.to_list |> List.map V.fromString |> make;
  /*  let fromString s => fromStringFlexible F.fromString;
      let fromStringStrict s => fromStringFlexible F.fromStringStrict;*/
  let const m n x => V.const n x |> Array.make m |> Array.to_list;
  let zero m n => const m n F.zero;
  let row (a: t) (i: int) :V.t => List.nth a i;
  let col (a: t) (j: int) :V.t => List.map (fun row => List.nth row j) a;
  let swap (i: int) (j: int) (a: t) :t =>
    a
    |> List.mapi (
         fun k u =>
           if (k == i) {
             row a j
           } else if (k == j) {
             row a i
           } else {
             u
           }
       );
  let scale (x: F.t) (i: int) (a: t) :t =>
    a
    |> List.mapi (
         fun k row =>
           if (k == i) {
             V.smul x row
           } else {
             row
           }
       );
  let transvect (x: F.t) (i: int) (j: int) (a: t) :t =>
    a
    |> List.mapi (
         fun k u =>
           if (k == j) {
             row a i |> V.smul x |> V.add u
           } else {
             u
           }
       );
  let applyRowOp op a =>
    a
    |> (
      switch op {
      | Swap i j => swap i j
      | Scale x i => scale x i
      | Transvect x i j => transvect x i j
      }
    );
  let rec firstPivot (a: list V.t) :option entry =>
    switch a {
    | [] => None
    | [v, ...w] =>
      switch (V.leadingEntry v, firstPivot w) {
      | (None, None) => None
      | (Some {index: j, value}, None) => Some {i: 0, j, value}
      | (None, Some {i, j, value}) => Some {i: i + 1, j, value}
      | (Some {index: j1, value: value1}, Some {i: i2, j: j2, value: value2}) =>
        j1 <= j2 ? Some {i: 0, j: j1, value: value1} : Some {i: i2 + 1, j: j2, value: value2}
      }
    };
  let rec nextRowOpToRef a =>
    switch a {
    | [] => None
    | [_, ...b] =>
      switch (firstPivot a) {
      | None => None
      | Some {i, j, value} =>
        if (i != 0) {
          Some (Swap i 0)
        } else if (i == 0 && value != F.one) {
          /*
           * Todo: Swap with row k > 0 if a_kj = 1.
           */
          Some (Scale (F.inv value) 0)
        } else {
          /* i == 0 && value == F.one */
          switch (col b j |> V.leadingEntry) {
          | None =>
            switch (nextRowOpToRef b) {
            | None => None
            | Some (Swap i j) => Some (Swap (i + 1) (j + 1))
            | Some (Scale x i) => Some (Scale x (i + 1))
            | Some (Transvect x i j) => Some (Transvect x (i + 1) (j + 1))
            }
          | Some {index, value: x} => Some (Transvect (F.neg x) 0 (index + 1))
          }
        }
      }
    };
  let rec rowEchelonForm (a: t) :t =>
    switch (firstPivot a) {
    | None => a
    | Some {i, j, value} =>
      switch (a |> swap 0 i |> scale (F.inv value) 0) {
      | [] => []
      | [u, ...v] =>
        let f row x => V.sub row (V.smul x u);
        let w = List.map2 f v (col v j);
        [u, ...rowEchelonForm w]
      }
    };
  let refToRref a => {
    let f i acc => {
      let u = row acc i;
      switch (V.leadingEntry u) {
      | None => acc
      | Some {index: j} =>
        acc |> List.mapi (fun k v => k < i ? V.sub v (V.smul (List.nth v j) u) : v)
      }
    };
    let rowIndices = List.mapi (fun i _ => i) a;
    List.fold_right f rowIndices a
  };
  let pivColsFromRef (a: t) => {
    /* pivot columns of a matrix in row echelon form *in reverse order* */
    let f acc u =>
      switch (V.leadingEntry u) {
      | None => acc
      | Some {index} => [col a index, ...acc]
      };
    List.fold_left f [] a
  };
  let rec nextRowOpfromPivCols pivCols => {
    let r = List.length pivCols;
    switch pivCols {
    | [] => None
    | [u, ...b] =>
      switch (V.leadingEntry u) {
      | None => raise (SomethingWentWrong "Something went wrong: u should have a leading entry.")
      | Some {index, value} =>
        if (index >= r - 1) {
          nextRowOpfromPivCols b
        } else {
          Some (Transvect (F.neg value) (r - 1) index)
        }
      }
    }
  };
  let nextRowOp a =>
    switch (nextRowOpToRef a) {
    | None => a |> pivColsFromRef |> nextRowOpfromPivCols
    | op => op
    };
  let toString (a: t) :string =>
    a
    |> List.map (fun row => row |> List.map F.toString |> String.concat "   ")
    |> String.concat "\n";
  let toTex (a: t) :string =>
    {|\begin{bmatrix}|}
    ^ (
      a
      |> List.map (fun row => row |> List.map F.toTex |> String.concat "&")
      |> String.concat {|\\|}
    )
    ^ {|\end{bmatrix}|};
};