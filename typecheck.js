
const Type = function(name, pred) {
  this.isInstance = pred;
  this.toString = function() {
    return "<special type " + name + ">";
  };
};

const Int = new Type("Int", function(val) {
  return (typeof(val) == "number" || val instanceof Number) && val == Math.floor(val);
});

const TypeMismatchError = function(name, pos, exp, act) {
  this.toString = function() {
    return "Type mismatch in argument " + pos + " of a call to " + name + ": got "
      + act + "; expected " + exp + ".";
  };
};

const ArgumentCountError = function(name, exp, act) {
  this.toString = function() {
    return "Wrong number of arguments in call to " + name + ": got " + act
      + "; expected " + exp + ".";
  };
};

const typeObject = {number: Number, boolean: Boolean, string: String, object: Object};

const typecheck = function(name, types, target) {
  return function() {
    if (arguments.length != types.length) {
      throw new ArgumentCountError(name, arguments.length, types.length);
    }
    for (var i in types) {
      if (types[i] instanceof Type) {
        if (!types[i].isInstance(arguments[i])) {
          throw new TypeMismatchError(name, i, types[i], typeof(arguments[i]));
        }
      } else if (typeof(arguments[i]) == "object") {
        if (!(arguments[i] instanceof types[i])) {
          throw new TypeMismatchError(name, i, types[i], typeof(arguments[i]));
        }
      } else {
        var obj = typeObject[typeof(arguments[i])];
        if (obj != types[i]) {
          throw new TypeMismatchError(name, i, types[i], obj);
        }
      }
    }
    return target.apply(this, arguments);
  };
};

