let se = ReasonReact.stringToElement;

let component = ReasonReact.statelessComponent "Inputs";

let make
    ::row1Input
    ::row2Input
    ::scalarInput
    ::handleRow1Change
    ::handleRow2Change
    ::handleScalarChange
    _children => {
  ...component,
  render: fun _self =>
    <div>
      (se "Row 1: ")
      <input value=row1Input onChange=handleRow1Change />
      <br />
      <br />
      (se "Row 2: ")
      <input value=row2Input onChange=handleRow2Change />
      <br />
      <br />
      (se "Scalar: ")
      <input value=scalarInput onChange=handleScalarChange />
    </div>
};