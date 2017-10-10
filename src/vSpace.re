module Make (F: Field.FieldType) => {
  type k = F.t;
  type t = list F.t;
  type entry = {
    index: int,
    value: F.t
  };
  /* Do not expose */
  let rec find f (ell: list 'a) :option entry =>
    switch ell {
    | [] => None
    | [x, ...y] =>
      f x == true ?
        Some {index: 0, value: x} :
        (
          switch (find f y) {
          | None => None
          | Some {index, value} => Some {index: index + 1, value}
          }
        )
    };
  let make (a: list F.t) :t => a;
  let fromString s =>
    s
    |> Js.String.trim
    |> Js.String.split " "
    |> Array.to_list
    |> List.map (fun x => x |> Js.String.trim |> F.fromString);
  /*  let fromString s => fromStringFlexible F.fromString s;
      let fromStringStrict s => fromStringFlexible F.fromStringStrict s; */
  let const n (x: F.t) :t => Array.make n x |> Array.to_list;
  let zero n => const n F.zero;
  let leadingEntry (row: t) :option entry => find (fun x => x != F.zero) row;
  let add (v: t) (w: t) :t => List.map2 (fun x y => F.add x y) v w;
  let neg (v: t) :t => v |> List.map F.neg;
  let sub (v: t) (w: t) :t => add v (neg w);
  let smul (x: F.t) (v: t) :t => v |> List.map (F.mul x);
  let toString (v: t) :string => List.map F.toTex v |> String.concat "   ";
  let toTex (v: t) :string => v |> toString;
};