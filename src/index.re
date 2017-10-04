[%bs.raw {|require('./index.css')|}];

/*Js.log Functor.str;

  Js.log Example.message;

  Js.log VectorSpace.u;

  Js.log VectorSpace.v;

  Js.log VectorSpace.x;

  module MR = MatrixSpace.Make R;

  let mk = R.make;

  let m = MR.make [|[|mk (-1.), mk 2.|], [|mk 3., mk (-4.)|]|];

  m |> MR.toTex |> Js.log;
  */
/*external register_service_worker : unit => unit = "default" [@@bs.module "./registerServiceWorker"];*/
ReactDOMRe.renderToElementWithId <App /> "root";
/*register_service_worker ();*/