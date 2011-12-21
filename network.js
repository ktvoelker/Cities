
const Node = Struct("Node", [['pos', Pos], ['name', String]], function() { });

const Edge = Struct("Edge", [['from', Pos], ['to', Pos]], function() { });

const Color = Struct(
    "Color", [['red', Int], ['green', Int], ['blue', Int]], function() { });

const Shape = Enum(
    "Shape", ['Circle', 'Square', 'Diamond', 'Hexagon', 'UpTriangle', 'DownTriangle']);

const Symbol = Struct(
    "Symbol", [['color', Color], ['text', String], ['shape', Shape]], function() { });

const Line = Struct(
    "Line",
    [['edges', ListOf(Edge)],
     ['name', String],
     ['symbol', Symbol],
     ['color', Color]],
    function() { });

const Network = Struct(
    "Network",
    [['nodes', ListOf(Node)],
     ['lines', ListOf(Line)],
     ['lineCount', Int]],
    function() { });

