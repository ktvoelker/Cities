
const ListCubedOf = function(x) { return ListOf(ListOf(ListOf(x))); };

const ListSquaredOf = function(x) { return ListOf(ListOf(x)); };

const Layout = Struct(
    "Layout",
    [['lines', ListCubedOf(Pos)],
     ['remLines', ListOf(Line)],
     ['curLine', ListSquaredOf(Pos)],
     ['remEdges', ListOf(Edge)],
     ['curEdge', ListOf(Pos)]],
    function() { });

const initialLayout = typecheck("initialLayout", [Network], function(net) {
  var lines = net.lines;
  var edges = lines.head.edges;
  return new Layout(
    ListCubedOf(Pos).nil,
    lines,
    ListSquaredOf(Pos).nil,
    edges,
    new (ListOf(Pos))(edges.head.from, ListOf(Pos).nil));
});

