var a = 10,
    b,
    c;

function blarg(a, b, c) {
  var d = a + b * c
  
  return {
    result: a + d * c + a,
    method : function () {
      return {
        a : a,
        "b" : b,
        "c" : c
      }
    }
  }
}

do {
  a -= 2
  console.log("The value is: " + a, "Blarg")
  a += 10
  a -= 11
} while (a > 0);

(function (a, b) {
  return a + b;
})(1, 2)

var obj = {
  hello : "world",
  "complicated" : 42,
  nested : {
    a : 10,
    b : 11,
    empty : {},
    method : function (a, b) {
      while (a > b) {
        --a;
        b++;
        console.log(a, b);
        if (typeof a == typeof b) console.log("The same!")
      }

      return {
        a : a,
        b : b
      };
    }
  }
};
  