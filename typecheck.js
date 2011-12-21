
const Type = function(name, pred) {
  this.name = name;
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

const typeObject = {
  number: Number,
  boolean: Boolean,
  string: String,
  object: Object,
  function: Function
};

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

const Struct = typecheck(
    "Struct", [String, Array, Function], function(name, sig, rest) {
  var names = sig.map(function(x) { return x[0]; });
  var types = sig.map(function(x) { return x[1]; });
  var struct = typecheck(name, types, function() {
    for (var i in names) {
      this[names[i]] = arguments[i];
    }
    return rest.apply(this);
  });
  struct.toString = function() {
    return "<Struct " + name + ">";
  };
  return struct;
});

const Enum = typecheck("Enum", [String, Array], function(name, values) {
  var base = function(v) {
    this.toString = function() {
      return v;
    };
  };
  for (var i in values) {
    base[values[i]] = new base(values[i]);
  }
  base.toString = function() {
    return "<Enum " + name + ">";
  };
  return base;
});

const ListTypes = {};

const ListOf = function(elemType) {
  var elemTypeName = elemType.toString();
  if (ListTypes.hasOwnProperty(elemTypeName)) {
    return ListTypes[elemTypeName];
  }
  var base;
  var type = new Type("ListOf(" + elemTypeName + ")", function(x) {
    return x === base.nil || x instanceof base;
  });
  base = Struct(
      "NonEmptyListOf(" + elemTypeName + ")",
      [['head', elemType], ['tail', type]],
      function() {
        this.isNil = function() {
          return false;
        };
        this.length = this.tail.length + 1;
      });
  base.nil = {};
  base.nil.toString = function() {
    "<EmptyListOf(" + elemTypeName + ")>";
  };
  base.nil.isNil = function() {
    return true;
  };
  base.nil.length = 0;
  base.type = type;
  ListTypes[elemTypeName] = base;
  return base;
};

