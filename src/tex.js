import React, { Component } from "react";
//import katex from "katex";
//import "katex/dist/katex.min.css";

class tex extends Component {
  render() {
    return <b>{this.props.source}</b>;
  }
}
/*
export default class extends Component {
  componentDidMount() {
    katex.render(this.props.source, this.elt);
  }

  componentDidUpdate() {
    katex.render(this.props.source, this.elt);
  }

  render() {
    return <span ref={elt => (this.elt = elt)} />;
  }
}
*/
module.exports = { tex };
