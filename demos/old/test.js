var array = [1,2,3,2 + 5, blarg(100)]

if (typeof abc === "undefined") {
  abc = def
} 

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

for (var i = 0; i < 10; i++) {
  console.log(i)
  for (var j = 0; j < i; j++) console.log(j)
}

while (a > 0) {
  a -= 2
  console.log("The value is: " + a, "Blarg")
  a += 10
  a -= 11
}

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
  