/*module type Dim = {let dim: int;};*/
module Make (F: Field.FieldType) => {
  type k = F.t;
  type t = array F.t;
  let make (a: array F.t) :t => a;
  let zero n => Array.make n F.zero;
  let const n (x: F.t) => Array.make n x;
  let add (v: t) (w: t) :t => v |> Array.mapi (fun i x => F.add x w.(i));
  let neg (v: t) :t => v |> Array.map F.neg;
  let sub (v: t) (w: t) :t => add v (neg w);
  let smul (x: F.t) (v: t) :t => v |> Array.map (F.mul x);
  let toString (v: t) :string => Array.to_list (Array.map F.toTex v) |> String.concat "   ";
  let toTex (v: t) :string => v |> toString;
};