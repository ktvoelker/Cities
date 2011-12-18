
const Vec = typecheck("Vec", [Int, Int], function(xDiff, yDiff) {
  this.xDiff = xDiff;
  this.yDiff = yDiff;
});

const Pos = typecheck("Pos", [Int, Int], function(xPos, yPos) {
  this.xPos = xPos;
  this.yPos = yPos;
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

