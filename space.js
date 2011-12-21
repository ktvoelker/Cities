
const Vec = Struct("Vec", [['xDiff', Int], ['yDiff', Int]], function() { });

const Pos = Struct("Pos", [['xPos', Int], ['yPos', Int]], function() {
  this.distance = typecheck("Pos.distance", [Pos], function(other) {
    return Math.sqrt(
      Math.pow(this.xPos - other.xPos, 2) + Math.pow(this.yPos - other.yPos, 2));
  });
  this.minus = typecheck("Pos.minus", [Pos], function(other) {
    return new Vec(this.xPos - other.xPos, this.yPos - other.yPos);
  });
  this.plus = typecheck("Pos.plus", [Vec], function(vec) {
    return new Pos(this.xPos + vec.xDiff, this.yPos + vec.yDiff);
  });
});

