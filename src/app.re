[%bs.raw {|require('./app.css')|}];

[%bs.raw {|require('katex/dist/katex.min.css')|}];

let make = Q.make;

module X = Map.Make String;

let m = X.empty;

let f (x: string) :string =>
  switch x {
  | "hi" => "hi"
  | "mom" => "mom"
  | _ => "neither hi nor mom"
  };

Js.log (f "dude");

module MQ = MatrixSpace.Make Q;

let m = MQ.make [|[|make 1 2, make 1 3|], [|make 1 4, make 1 5|]|];

let source = m |> MQ.toTex;

let component = ReasonReact.statelessComponent "App";

let make _children => {
  ...component,
  render: fun _self => <div> /*<Tex source /> <br /> */ <Echo /> </div>
};