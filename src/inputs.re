let se = ReasonReact.stringToElement;

let component = ReasonReact.statelessComponent "Inputs";

let make
    ::matrix
    ::row1
    ::row2
    ::scalar
    ::handleMatrixChange
    ::handleRow1Change
    ::handleRow2Change
    ::handleScalarChange
    _children => {
  ...component,
  render: fun _self =>
    <div>
      (se "Matrix: ")
      <textarea placeholder="..." value=matrix onChange=handleMatrixChange />
      <br />
      <br />
      (se "Row 1: ")
      <input placeholder="..." value=row1 onChange=handleRow1Change />
      <br />
      <br />
      (se "Row 2: ")
      <input placeholder="..." value=row2 onChange=handleRow2Change />
      <br />
      <br />
      (se "Scalar: ")
      <input placeholder="..." value=scalar onChange=handleScalarChange />
    </div>
};