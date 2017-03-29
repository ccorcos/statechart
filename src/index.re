let root = ReasonJs.Dom.Document.createElement "div" DomRe.document;

ReactDOMRe.render <Page /> root;

let document = ReasonJs.Dom.Document.asHtmlDocument DomRe.document;

let body =
  switch document {
  | None => None
  | Some document => ReasonJs.Dom.HtmlDocument.body document
  };

let () =
  switch body {
  | None => ()
  | Some body => ReasonJs.Dom.Element.appendChild root body
  };
