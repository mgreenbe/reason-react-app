let se = ReasonReact.stringToElement;

let int_of_string_opt s =>
  try (Some (int_of_string s)) {
  | _ => None
  };

module MQ = MSpace.Make Q;

type kind = Q.kind;

type hint =
  | SwapHint
  | ScaleHint
  | TransvectHint;

type action =
  | MatrixChange string
  | Work
  | Row1Change string
  | Row2Change string
  | ScalarChange string
  | Swap
  | Scale
  | Transvect
  | Next;

type state = {
  matrixInput: string,
  matrixPreview: string,
  row1Input: string,
  row2Input: string,
  scalarInput: string,
  matrix: option MQ.t,
  row1: option int,
  row2: option int,
  scalar: option Q.t,
  hint: option hint
};

let valueOfTarget event => (event |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj)##value;

/* let initialMatrixString = "0 -1/2 2/3\n-3/4 4/5 -5/6\n6/7 -7/8 8/9"; */
/* let initialMatrixString = "1 2 3\n4 5 6\n7 8 9"; */
/* let initialMatrixString = "0 1 2\n3 4 5\n6 7 8"; */
/* let initialMatrixString = "1 1 1 -2 1 8\n-1 1 0 3 0 -4\n0 1 1 0 0 1\n0 1 0 1 1 3\n-1 2 1 3 0 -3"; */
let initialMatrixString = "0 3 -6 6 4 -5\n3 -7 8 -5 8 9\n3 -9 12 -9 6 15";

let i = <Tex source="i" />;

let iEquals = <Tex source="i=\\;" />;

let j = <Tex source="j" />;

let jEquals = <Tex source="j=\\;" />;

let k = <Tex source="k" />;

let kEquals = <Tex source="k=\\;" />;

let component = ReasonReact.reducerComponent "Echo";

let make _children => {
  ...component,
  initialState: fun () => (
    {
      matrixInput: initialMatrixString,
      matrixPreview: initialMatrixString |> MQ.fromString |> MQ.toTex,
      row1Input: "",
      row2Input: "",
      scalarInput: "",
      matrix: None,
      row1: None,
      row2: None,
      scalar: None,
      hint: None
    }: state
  ),
  reducer: fun action state => {
    let matrix = state.matrix;
    let row1 = int_of_string_opt state.row1Input;
    let row2 = int_of_string_opt state.row2Input;
    let scalar = Q.fromStringStrict state.scalarInput;
    switch action {
    | MatrixChange matrixInput =>
      ReasonReact.Update {
        ...state,
        matrixInput,
        matrixPreview: matrixInput |> MQ.fromString |> MQ.toTex
      }
    | Work =>
      let matrix = state.matrixInput |> MQ.fromString;
      matrix |> List.flatten |> List.for_all Q.isFinite ?
        ReasonReact.Update {...state, matrix: Some matrix, hint: None} : ReasonReact.NoUpdate
    | Row1Change row1Input => ReasonReact.Update {...state, row1Input, hint: None}
    | Row2Change row2Input => ReasonReact.Update {...state, row2Input, hint: None}
    | ScalarChange scalarInput => ReasonReact.Update {...state, scalarInput, hint: None}
    | Swap =>
      switch (row1, row2, matrix) {
      | (Some i, Some j, Some matrix) =>
        ReasonReact.Update {...state, matrix: Some (MQ.swap i j matrix), hint: None}
      | _ => ReasonReact.NoUpdate
      }
    | Scale =>
      switch (scalar, row1, matrix) {
      | (Some x, Some i, Some matrix) =>
        ReasonReact.Update {...state, matrix: Some (MQ.scale x i matrix), hint: None}
      | _ => ReasonReact.NoUpdate
      }
    | Transvect =>
      switch (scalar, row1, row2, matrix) {
      | (Some x, Some i, Some j, Some mat) =>
        ReasonReact.Update {...state, matrix: Some (MQ.transvect x i j mat), hint: None}
      | _ => ReasonReact.NoUpdate
      }
    | Next =>
      switch matrix {
      | None => ReasonReact.Update {...state, row1Input: "", row2Input: "", scalarInput: ""}
      | Some matrix =>
        switch (MQ.nextRowOp matrix) {
        | None =>
          ReasonReact.Update {...state, row1Input: "", row2Input: "", scalarInput: "", hint: None}
        | Some (MQ.Swap i j) =>
          ReasonReact.Update {
            ...state,
            row1Input: string_of_int i,
            row2Input: string_of_int j,
            scalarInput: "",
            hint: Some SwapHint
          }
        | Some (MQ.Scale x i) =>
          ReasonReact.Update {
            ...state,
            row1Input: string_of_int i,
            row2Input: "",
            scalarInput: Q.toString x,
            hint: Some ScaleHint
          }
        | Some (MQ.Transvect x i j) =>
          ReasonReact.Update {
            ...state,
            row1Input: string_of_int i,
            row2Input: string_of_int j,
            scalarInput: Q.toString x,
            hint: Some TransvectHint
          }
        }
      }
    }
  },
  render: fun self => {
    let handleMatrixChange = self.reduce (fun event => MatrixChange (valueOfTarget event));
    let handleRow1Change = self.reduce (fun event => Row1Change (valueOfTarget event));
    let handleRow2Change = self.reduce (fun event => Row2Change (valueOfTarget event));
    let handleScalarChange = self.reduce (fun event => ScalarChange (valueOfTarget event));
    let {matrixInput, matrixPreview, row1Input, row2Input, scalarInput, matrix, hint} = self.state;
    let row1 = int_of_string_opt row1Input;
    let row2 = int_of_string_opt row2Input;
    let scalar = Q.fromStringStrict scalarInput;
    let ii =
      switch row1 {
      | Some _ => <Tex source=row1Input />
      | _ => i
      };
    let jj =
      switch row2 {
      | Some _ => <Tex source=row2Input />
      | _ => j
      };
    let kk =
      switch scalar {
      | Some x => <Tex source=(Q.toTex x) />
      | _ => k
      };
    let inRef =
      switch matrix {
      | Some matrix =>
        switch (MQ.nextRowOpToRef matrix) {
        | Some _ => false
        | None => true
        }
      | None => false
      };
    let inRref =
      switch matrix {
      | Some matrix =>
        switch (MQ.nextRowOp matrix) {
        | Some _ => false
        | None => true
        }
      | None => false
      };
    let workable = matrixInput |> MQ.fromString |> List.flatten |> List.for_all Q.isFinite;
    let working =
      switch matrix {
      | Some _ => true
      | _ => false
      };
    let options =
      switch matrix {
      | None => ReasonReact.nullElement
      | Some matrix =>
        let blank = <option key="blank" value=""> (se "") </option>;
        [
          blank,
          ...matrix
             |> List.mapi (
                  fun i _ =>
                    <option key=(string_of_int i) value=(string_of_int i)>
                      (i |> string_of_int |> se)
                    </option>
                )
        ]
        |> Array.of_list
        |> ReasonReact.arrayToElement
      };
    <div>
      <div className="matrix-input-bar">
        <div>
          <div className="matrix-input-label"> (se "Enter matrix: ") </div>
          <textarea
            id="matrix-input-textarea"
            value=matrixInput
            onChange=handleMatrixChange
            rows=5
            cols=30
          />
        </div>
        <div>
          <div className="matrix-preview-label"> (se "Preview: ") </div>
          <Tex source=matrixPreview />
        </div>
      </div>
      <br />
      <button
        onClick=(self.reduce (fun _ => Work)) disabled=(not workable |> Js.Boolean.to_js_boolean)>
        (se "Work with this matrix.")
      </button>
      <br />
      <br />
      <div className="working-matrix-bar">
        <div>
          <div className="working-matrix-label"> (se "Working matrix:") </div>
          (
            switch matrix {
            | Some matrix => <Tex source=(MQ.toTex matrix) />
            | None => ReasonReact.nullElement
            }
          )
        </div>
        <div className="ef-bar">
          <span className=("ef-bar-badge " ^ (inRef ? "in-ref" : "not-in-ref"))> (se "REF") </span>
          <span className=("ef-bar-badge " ^ (inRref ? "in-rref" : "not-in-rref"))>
            (se "RREF")
          </span>
        </div>
      </div>
      <br />
      <br />
      <div className="input-bar">
        <div>
          <span> iEquals </span>
          <select
            value=row1Input
            onChange=handleRow1Change
            className=(
              "input"
              ^ (
                switch hint {
                | Some SwapHint
                | Some ScaleHint
                | Some TransvectHint => " input-highlight"
                | _ => ""
                }
              )
            )
            disabled=(not working |> Js.Boolean.to_js_boolean)>
            options
          </select>
        </div>
        <div>
          <span> jEquals </span>
          <select
            value=row2Input
            onChange=handleRow2Change
            className=(
              "input"
              ^ (
                switch hint {
                | Some SwapHint
                | Some TransvectHint => " input-highlight"
                | _ => ""
                }
              )
            )
            disabled=(not working |> Js.Boolean.to_js_boolean)>
            options
          </select>
        </div>
        <div>
          <span> kEquals </span>
          <input
            value=scalarInput
            onChange=handleScalarChange
            className=(
              "input scalar-input "
              ^ (
                switch hint {
                | Some ScaleHint
                | Some TransvectHint => " input-highlight"
                | _ => ""
                }
              )
            )
            disabled=(not working |> Js.Boolean.to_js_boolean)
          />
        </div>
      </div>
      <br />
      <br />
      <div className="button-bar">
        <button
          onClick=(self.reduce (fun _ => Swap))
          className=(
            "button"
            ^ (
              switch hint {
              | Some SwapHint => " button-highlight"
              | _ => ""
              }
            )
          )
          disabled=(
            switch (working, row1, row2) {
            | (true, Some _, Some _) => Js.Boolean.to_js_boolean false
            | _ => Js.Boolean.to_js_boolean true
            }
          )>
          (se "Swap rows ")
          ii
          (se " and ")
          jj
        </button>
        <button
          onClick=(self.reduce (fun _ => Scale))
          className=(
            "button"
            ^ (
              switch hint {
              | Some ScaleHint => " button-highlight"
              | _ => ""
              }
            )
          )
          disabled=(
            switch (working, row1, scalar) {
            | (true, Some _, Some _) => Js.Boolean.to_js_boolean false
            | _ => Js.Boolean.to_js_boolean true
            }
          )>
          (se "Multiply row ")
          ii
          (se " by ")
          kk
        </button>
        <button
          onClick=(self.reduce (fun _ => Transvect))
          className=(
            "button"
            ^ (
              switch hint {
              | Some TransvectHint => " button-highlight"
              | _ => ""
              }
            )
          )
          disabled=(
            switch (working, row1, row2, scalar) {
            | (true, Some _, Some _, Some _) => Js.Boolean.to_js_boolean false
            | _ => Js.Boolean.to_js_boolean true
            }
          )>
          (se "Add ")
          kk
          (se " times row ")
          ii
          (se " to row ")
          jj
          (se ".")
        </button>
        <button
          onClick=(self.reduce (fun _ => Next))
          disabled=((not working || inRref) |> Js.Boolean.to_js_boolean)
          className="button">
          (se "What now?")
        </button>
      </div>
    </div>
  }
};