var el = document.getElementById.bind(document)

var el_arith     = el('arith')
var el_operators = el('operators')
var el_numbers   = el('numbers')

el_arith.addEventListener('input', function(ev) {
  var list = parse(ev.target.value)
  console.log(list.join(','))

  var pair = shunt(list)

  show_ops(pair[0])
  show_num(pair[1])
})

function parse(str) {
  return str.split('')
    .reduce(function name(acc, val) {
      var len = acc.length-1
      //     the test              the return value
      return val === ' '         ? acc
           : /\D/.test(val)      ? acc.concat(val)
           : /\D/.test(acc[len]) ? acc.concat(val)
                                 : acc.slice(0, -1).concat(""+acc[len]+val)
    }, [])
}


function shunt(list) {
  var ops = []
  var nums = []
  var prec = {'+': 1, '-': 2, '*': 3, '/': 4}

  for(var i = 0; i < list.length; i++) {
    var item = list[i]
    if(/\d+/.test(item))
      nums.push(item) // mutation is such sweet sorrow
    else {
      while(prec[item] <= prec[ops[ops.length-1]]) {
        var a = nums.pop(), b = nums.pop(), c = ops.pop()
        nums.push(eval("" + a + c + " " + b + "")) // oh dear
      }
      ops.push(item)
    }
  }

  return [ops, nums]
}

function show_ops(list) {
  // el_operators.innerText = list
  el_operators.value = list
}

function show_num(list) {
  el_numbers.value = list
}
