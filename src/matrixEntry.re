let se = ReasonReact.stringToElement;

let int_of_string_opt s =>
  try (Some (int_of_string s)) {
  | _ => None
  };

module MQ = MSpace.Make Q;

type action =
  | UpdateValue string;

type state = {input: string};

/* let initialMatrixString = "0 -1/2 2/3\n-3/4 4/5 -5/6\n6/7 -7/8 8/9"; */
/* let initialMatrixString = "1 2 3\n4 5 6\n7 8 9"; */
/* let initialMatrixString = "0 1 2\n3 4 5\n6 7 8"; */
/* let initialMatrixString = "1 1 1 -2 1 8\n-1 1 0 3 0 -4\n0 1 1 0 0 1\n0 1 0 1 1 3\n-1 2 1 3 0 -3"; */
let initialMatrixString = "0 3 -6 6 4 -5\n3 -7 8 -5 8 9\n3 -9 12 -9 6 15";

let valueOfTarget event => (event |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj)##value;

let component = ReasonReact.reducerComponent "MatrixEntry";

let make ::updateMatrix _children => {
  ...component,
  initialState: fun () => {input: initialMatrixString},
  reducer: fun action _ =>
    switch action {
    | UpdateValue input => ReasonReact.Update {input: input}
    },
  render: fun self => {
    let {input} = self.state;
    let matrix = MQ.fromString input;
    let preview = matrix |> MQ.toTex;
    let matrixOpt = matrix |> List.flatten |> List.for_all Q.isFinite ? Some matrix : None;
    <div className="matrix-input-bar">
      <div>
        <div className="matrix-input-label"> (se "Enter matrix: ") </div>
        <textarea
          id="matrix-input-textarea"
          value=input
          onChange=(self.reduce (fun event => UpdateValue (valueOfTarget event)))
          rows=5
          cols=30
        />
      </div>
      <div>
        <div className="matrix-preview-label"> (se "Preview: ") </div>
        <Tex source=preview />
      </div>
      <button
        onClick=(updateMatrix matrixOpt)
        disabled=(matrixOpt |> Js.Option.isNone |> Js.Boolean.to_js_boolean)>
        (se "Work with this matrix.")
      </button>
    </div>
  }
};