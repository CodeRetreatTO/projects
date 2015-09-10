// Pete, Dann and inaimathi

var $repl = document.querySelector("#repl")
var $q = document.querySelector("#queue")
var $stack = document.querySelector("#stack")

var ops = {
    '+': { precedence: 0, fn: function (a, b) { return a + b}},
    '-': { precedence: 0, fn: function (a, b) { return a - b}},
    '*': { precedence: 1, fn: function (a, b) { return a * b}},
    '/': { precedence: 1, fn: function (a, b) { return a / b}},
    
    // 'let': { precedence: 47, fn: function ()}
}

$repl.addEventListener('input', function (ev) {
    doStuff(ev.target.value)
})

function tokens(string) {
    return string.split(" ").filter(function (elem) {
	return elem != ""
    })
}

function step(token, q, stack) {
    if (!isNaN(+token)) {
	return {q: q.concat(+token), stack: stack}
    } else {
	var op = stack[stack.length-1]
	console.log("TOKEN:", token)
	if (stack.length == 0 || ops[token].precedence > ops[op].precedence) {
	    return {q:q, stack: stack.concat(token)}
	} else {
	    var newStack = stack.slice(0, -1)
	    var two = ops[op].fn.length
	    var elems = q.slice(-two)
	    var result = ops[op].fn.apply(null, elems) // [[op].concat(elems)]
	    var newQ = q.slice(0, -two).concat(result)
	    return step(token, newQ, newStack)
	}
    }
}

function process(toks) {
    var res = toks.reduce(function (memo, tok) {
	return step(tok, memo.q, memo.stack)
    }, {q: [], stack: []})
    $q.innerHTML = JSON.stringify(res.q)
    $stack.innerHTML = JSON.stringify(res.stack)
}

function doStuff(str) {
    process(tokens(str))
}

if ($repl.value) {
    doStuff($repl.value)
}
