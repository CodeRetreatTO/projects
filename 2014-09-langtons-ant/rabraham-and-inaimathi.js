// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        return f;
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

// Export Haste, A and E. Haste because we need to preserve exports, A and E
// because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = A(f, [mv.x]);
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.target.offsetLeft || 0),
	    posy - (e.target.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[0,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=[0],_1=[0,0],_2=[1],_3=unCStr("Failure in Data.Map.balanceL"),_4=new T(function(){return err(_3);}),_5=function(_6,_7,_8){var _9=E(_8);if(!_9[0]){var _a=_9[1],_b=E(_7);if(!_b[0]){var _c=_b[1],_d=_b[2];if(_c<=(imul(3,_a)|0)){return [0,(1+_c|0)+_a|0,E(E(_6)),E(_b),E(_9)];}else{var _e=E(_b[3]);if(!_e[0]){var _f=_e[1],_g=E(_b[4]);if(!_g[0]){var _h=_g[1],_i=_g[2],_j=_g[3];if(_h>=(imul(2,_f)|0)){var _k=function(_l){var _m=E(_g[4]);return _m[0]==0?[0,(1+_c|0)+_a|0,E(_i),E([0,(1+_f|0)+_l|0,E(_d),E(_e),E(_j)]),E([0,(1+_a|0)+_m[1]|0,E(E(_6)),E(_m),E(_9)])]:[0,(1+_c|0)+_a|0,E(_i),E([0,(1+_f|0)+_l|0,E(_d),E(_e),E(_j)]),E([0,1+_a|0,E(E(_6)),E(_2),E(_9)])];},_n=E(_j);return _n[0]==0?_k(_n[1]):_k(0);}else{return [0,(1+_c|0)+_a|0,E(_d),E(_e),E([0,(1+_a|0)+_h|0,E(E(_6)),E(_g),E(_9)])];}}else{return E(_4);}}else{return E(_4);}}}else{return [0,1+_a|0,E(E(_6)),E(_2),E(_9)];}}else{var _o=E(_7);if(!_o[0]){var _p=_o[1],_q=_o[2],_r=_o[4],_s=E(_o[3]);if(!_s[0]){var _t=_s[1],_u=E(_r);if(!_u[0]){var _v=_u[1],_w=_u[2],_x=_u[3];if(_v>=(imul(2,_t)|0)){var _y=function(_z){var _A=E(_u[4]);return _A[0]==0?[0,1+_p|0,E(_w),E([0,(1+_t|0)+_z|0,E(_q),E(_s),E(_x)]),E([0,1+_A[1]|0,E(E(_6)),E(_A),E(_2)])]:[0,1+_p|0,E(_w),E([0,(1+_t|0)+_z|0,E(_q),E(_s),E(_x)]),E([0,1,E(E(_6)),E(_2),E(_2)])];},_B=E(_x);return _B[0]==0?_y(_B[1]):_y(0);}else{return [0,1+_p|0,E(_q),E(_s),E([0,1+_v|0,E(E(_6)),E(_u),E(_2)])];}}else{return [0,3,E(_q),E(_s),E([0,1,E(E(_6)),E(_2),E(_2)])];}}else{var _C=E(_r);return _C[0]==0?[0,3,E(_C[2]),E([0,1,E(_q),E(_2),E(_2)]),E([0,1,E(E(_6)),E(_2),E(_2)])]:[0,2,E(E(_6)),E(_o),E(_2)];}}else{return [0,1,E(E(_6)),E(_2),E(_2)];}}},_D=unCStr("Failure in Data.Map.balanceR"),_E=new T(function(){return err(_D);}),_F=function(_G,_H,_I){var _J=E(_H);if(!_J[0]){var _K=_J[1],_L=E(_I);if(!_L[0]){var _M=_L[1],_N=_L[2];if(_M<=(imul(3,_K)|0)){return [0,(1+_K|0)+_M|0,E(E(_G)),E(_J),E(_L)];}else{var _O=E(_L[3]);if(!_O[0]){var _P=_O[1],_Q=_O[2],_R=_O[3],_S=E(_L[4]);if(!_S[0]){var _T=_S[1];if(_P>=(imul(2,_T)|0)){var _U=function(_V){var _W=E(_G),_X=E(_O[4]);return _X[0]==0?[0,(1+_K|0)+_M|0,E(_Q),E([0,(1+_K|0)+_V|0,E(_W),E(_J),E(_R)]),E([0,(1+_T|0)+_X[1]|0,E(_N),E(_X),E(_S)])]:[0,(1+_K|0)+_M|0,E(_Q),E([0,(1+_K|0)+_V|0,E(_W),E(_J),E(_R)]),E([0,1+_T|0,E(_N),E(_2),E(_S)])];},_Y=E(_R);return _Y[0]==0?_U(_Y[1]):_U(0);}else{return [0,(1+_K|0)+_M|0,E(_N),E([0,(1+_K|0)+_P|0,E(E(_G)),E(_J),E(_O)]),E(_S)];}}else{return E(_E);}}else{return E(_E);}}}else{return [0,1+_K|0,E(E(_G)),E(_J),E(_2)];}}else{var _Z=E(_I);if(!_Z[0]){var _10=_Z[1],_11=_Z[2],_12=_Z[4],_13=E(_Z[3]);if(!_13[0]){var _14=_13[1],_15=_13[2],_16=_13[3],_17=E(_12);if(!_17[0]){var _18=_17[1];if(_14>=(imul(2,_18)|0)){var _19=function(_1a){var _1b=E(_G),_1c=E(_13[4]);return _1c[0]==0?[0,1+_10|0,E(_15),E([0,1+_1a|0,E(_1b),E(_2),E(_16)]),E([0,(1+_18|0)+_1c[1]|0,E(_11),E(_1c),E(_17)])]:[0,1+_10|0,E(_15),E([0,1+_1a|0,E(_1b),E(_2),E(_16)]),E([0,1+_18|0,E(_11),E(_2),E(_17)])];},_1d=E(_16);return _1d[0]==0?_19(_1d[1]):_19(0);}else{return [0,1+_10|0,E(_11),E([0,1+_14|0,E(E(_G)),E(_2),E(_13)]),E(_17)];}}else{return [0,3,E(_15),E([0,1,E(E(_G)),E(_2),E(_2)]),E([0,1,E(_11),E(_2),E(_2)])];}}else{var _1e=E(_12);return _1e[0]==0?[0,3,E(_11),E([0,1,E(E(_G)),E(_2),E(_2)]),E(_1e)]:[0,2,E(E(_G)),E(_2),E(_Z)];}}else{return [0,1,E(E(_G)),E(_2),E(_2)];}}},_1f=function(_1g,_1h,_1i){var _1j=E(_1i);if(!_1j[0]){var _1k=_1j[3],_1l=_1j[4],_1m=E(_1j[2]),_1n=E(_1m[1])[1];if(_1g>=_1n){if(_1g!=_1n){return _F(_1m,_1k,_1f(_1g,_1h,_1l));}else{var _1o=E(_1m[2])[1];return _1h>=_1o?_1h!=_1o?_F(_1m,_1k,_1f(_1g,_1h,_1l)):[0,_1j[1],E([0,[0,_1g],[0,_1h]]),E(_1k),E(_1l)]:_5(_1m,_1f(_1g,_1h,_1k),_1l);}}else{return _5(_1m,_1f(_1g,_1h,_1k),_1l);}}else{return [0,1,E([0,[0,_1g],[0,_1h]]),E(_2),E(_2)];}},_1p=function(_1q,_1r,_1s){var _1t=E(_1s);if(!_1t[0]){var _1u=_1t[3],_1v=_1t[4],_1w=E(_1t[2]),_1x=E(_1w[1])[1];if(_1q>=_1x){if(_1q!=_1x){return _F(_1w,_1u,_1p(_1q,_1r,_1v));}else{var _1y=E(_1r),_1z=_1y[1],_1A=E(_1w[2])[1];return _1z>=_1A?_1z!=_1A?_F(_1w,_1u,_1f(_1q,_1z,_1v)):[0,_1t[1],E([0,[0,_1q],_1y]),E(_1u),E(_1v)]:_5(_1w,_1f(_1q,_1z,_1u),_1v);}}else{return _5(_1w,_1p(_1q,_1r,_1u),_1v);}}else{return [0,1,E([0,[0,_1q],_1r]),E(_2),E(_2)];}},_1B=function(_1C,_1D){var _1E=E(_1C),_1F=_1E[2],_1G=E(_1D);if(!_1G[0]){var _1H=_1G[3],_1I=_1G[4],_1J=E(_1G[2]),_1K=E(_1E[1])[1],_1L=E(_1J[1])[1];if(_1K>=_1L){if(_1K!=_1L){return _F(_1J,_1H,_1p(_1K,_1F,_1I));}else{var _1M=E(_1F)[1],_1N=E(_1J[2])[1];return _1M>=_1N?_1M!=_1N?_F(_1J,_1H,_1f(_1K,_1M,_1I)):[0,_1G[1],E(_1E),E(_1H),E(_1I)]:_5(_1J,_1f(_1K,_1M,_1H),_1I);}}else{return _5(_1J,_1p(_1K,_1F,_1H),_1I);}}else{return [0,1,E(_1E),E(_2),E(_2)];}},_1O=function(_1P,_1Q){while(1){var _1R=E(_1Q);if(!_1R[0]){return E(_1P);}else{var _1S=_1B(_1R[1],_1P);_1Q=_1R[2];_1P=_1S;continue;}}},_1T=function(_1U){return [0,1,E(E(_1U)),E(_2),E(_2)];},_1V=function(_1W,_1X){var _1Y=E(_1X);return _1Y[0]==0?_F(_1Y[2],_1Y[3],_1V(_1W,_1Y[4])):_1T(_1W);},_1Z=function(_20,_21){var _22=E(_21);return _22[0]==0?_5(_22[2],_1Z(_20,_22[3]),_22[4]):_1T(_20);},_23=function(_24,_25,_26,_27,_28,_29){var _2a=E(_25);if(!_2a[0]){var _2b=_2a[1],_2c=_2a[2],_2d=_2a[3],_2e=_2a[4];return (imul(3,_2b)|0)>=_26?(imul(3,_26)|0)>=_2b?[0,(_2b+_26|0)+1|0,E(E(_24)),E(_2a),E([0,_26,E(_27),E(_28),E(_29)])]:_F(_2c,_2d,_23(_24,_2e,_26,_27,_28,_29)):_5(_27,_2f(_24,_2b,_2c,_2d,_2e,_28),_29);}else{return _1Z(_24,[0,_26,E(_27),E(_28),E(_29)]);}},_2f=function(_2g,_2h,_2i,_2j,_2k,_2l){var _2m=E(_2l);if(!_2m[0]){var _2n=_2m[1],_2o=_2m[2],_2p=_2m[3],_2q=_2m[4];return (imul(3,_2h)|0)>=_2n?(imul(3,_2n)|0)>=_2h?[0,(_2h+_2n|0)+1|0,E(E(_2g)),E([0,_2h,E(_2i),E(_2j),E(_2k)]),E(_2m)]:_F(_2i,_2j,_23(_2g,_2k,_2n,_2o,_2p,_2q)):_5(_2o,_2f(_2g,_2h,_2i,_2j,_2k,_2p),_2q);}else{return _1V(_2g,[0,_2h,E(_2i),E(_2j),E(_2k)]);}},_2r=function(_2s,_2t,_2u){var _2v=E(_2t);if(!_2v[0]){var _2w=_2v[1],_2x=_2v[2],_2y=_2v[3],_2z=_2v[4],_2A=E(_2u);if(!_2A[0]){var _2B=_2A[1],_2C=_2A[2],_2D=_2A[3],_2E=_2A[4];return (imul(3,_2w)|0)>=_2B?(imul(3,_2B)|0)>=_2w?[0,(_2w+_2B|0)+1|0,E(E(_2s)),E(_2v),E(_2A)]:_F(_2x,_2y,_23(_2s,_2z,_2B,_2C,_2D,_2E)):_5(_2C,_2f(_2s,_2w,_2x,_2y,_2z,_2D),_2E);}else{return _1V(_2s,_2v);}}else{return _1Z(_2s,_2u);}},_2F=function(_2G,_2H,_2I,_2J){var _2K=E(_2G);if(_2K==1){var _2L=E(_2J);if(!_2L[0]){return [0,[0,1,E([0,[0,_2H],[0,_2I]]),E(_2),E(_2)],_0,_0];}else{var _2M=E(_2L[1]),_2N=E(_2M[1])[1];return _2H>=_2N?_2H!=_2N?[0,[0,1,E([0,[0,_2H],[0,_2I]]),E(_2),E(_2)],_0,_2L]:_2I>=E(_2M[2])[1]?[0,[0,1,E([0,[0,_2H],[0,_2I]]),E(_2),E(_2)],_0,_2L]:[0,[0,1,E([0,[0,_2H],[0,_2I]]),E(_2),E(_2)],_2L,_0]:[0,[0,1,E([0,[0,_2H],[0,_2I]]),E(_2),E(_2)],_2L,_0];}}else{var _2O=_2F(_2K>>1,_2H,_2I,_2J),_2P=_2O[1],_2Q=_2O[3],_2R=E(_2O[2]);if(!_2R[0]){return [0,_2P,_0,_2Q];}else{var _2S=_2R[1],_2T=E(_2R[2]);if(!_2T[0]){return [0,new T(function(){return _1V(_2S,_2P);}),_0,_2Q];}else{var _2U=_2T[2],_2V=E(_2S),_2W=E(_2T[1]),_2X=_2W[2],_2Y=E(_2V[1])[1],_2Z=E(_2W[1])[1];if(_2Y>=_2Z){if(_2Y!=_2Z){return [0,_2P,_0,_2R];}else{var _30=E(_2X)[1];if(E(_2V[2])[1]>=_30){return [0,_2P,_0,_2R];}else{var _31=_2F(_2K>>1,_2Z,_30,_2U);return [0,new T(function(){return _2r(_2V,_2P,_31[1]);}),_31[2],_31[3]];}}}else{var _32=_33(_2K>>1,_2Z,_2X,_2U);return [0,new T(function(){return _2r(_2V,_2P,_32[1]);}),_32[2],_32[3]];}}}}},_33=function(_34,_35,_36,_37){var _38=E(_34);if(_38==1){var _39=E(_37);if(!_39[0]){return [0,[0,1,E([0,[0,_35],_36]),E(_2),E(_2)],_0,_0];}else{var _3a=E(_39[1]),_3b=E(_3a[1])[1];if(_35>=_3b){if(_35!=_3b){return [0,[0,1,E([0,[0,_35],_36]),E(_2),E(_2)],_0,_39];}else{var _3c=E(_36);return _3c[1]>=E(_3a[2])[1]?[0,[0,1,E([0,[0,_35],_3c]),E(_2),E(_2)],_0,_39]:[0,[0,1,E([0,[0,_35],_3c]),E(_2),E(_2)],_39,_0];}}else{return [0,[0,1,E([0,[0,_35],_36]),E(_2),E(_2)],_39,_0];}}}else{var _3d=_33(_38>>1,_35,_36,_37),_3e=_3d[1],_3f=_3d[3],_3g=E(_3d[2]);if(!_3g[0]){return [0,_3e,_0,_3f];}else{var _3h=_3g[1],_3i=E(_3g[2]);if(!_3i[0]){return [0,new T(function(){return _1V(_3h,_3e);}),_0,_3f];}else{var _3j=_3i[2],_3k=E(_3h),_3l=E(_3i[1]),_3m=_3l[2],_3n=E(_3k[1])[1],_3o=E(_3l[1])[1];if(_3n>=_3o){if(_3n!=_3o){return [0,_3e,_0,_3g];}else{var _3p=E(_3m)[1];if(E(_3k[2])[1]>=_3p){return [0,_3e,_0,_3g];}else{var _3q=_2F(_38>>1,_3o,_3p,_3j);return [0,new T(function(){return _2r(_3k,_3e,_3q[1]);}),_3q[2],_3q[3]];}}}else{var _3r=_33(_38>>1,_3o,_3m,_3j);return [0,new T(function(){return _2r(_3k,_3e,_3r[1]);}),_3r[2],_3r[3]];}}}}},_3s=function(_3t,_3u,_3v){var _3w=E(_3v);if(!_3w[0]){return E(_3u);}else{var _3x=_3w[1],_3y=E(_3w[2]);if(!_3y[0]){return _1V(_3x,_3u);}else{var _3z=E(_3x),_3A=E(_3y[1]),_3B=_3A[2],_3C=E(_3z[1])[1],_3D=E(_3A[1])[1],_3E=new T(function(){var _3F=_33(_3t,_3D,_3B,_3y[2]),_3G=_3F[1],_3H=E(_3F[3]);return _3H[0]==0?_3s(_3t<<1,_2r(_3z,_3u,_3G),_3F[2]):_1O(_2r(_3z,_3u,_3G),_3H);});return _3C>=_3D?_3C!=_3D?_1O(_3u,_3w):E(_3z[2])[1]>=E(_3B)[1]?_1O(_3u,_3w):E(_3E):E(_3E);}}},_3I=function(_3J){var _3K=E(_3J);if(!_3K[0]){return [1];}else{var _3L=_3K[1],_3M=E(_3K[2]);if(!_3M[0]){return [0,1,E(E(_3L)),E(_2),E(_2)];}else{var _3N=E(_3L),_3O=E(_3M[1]),_3P=E(_3N[1])[1],_3Q=E(_3O[1])[1];return _3P>=_3Q?_3P!=_3Q?_1O([0,1,E(_3N),E(_2),E(_2)],_3M):E(_3N[2])[1]>=E(_3O[2])[1]?_1O([0,1,E(_3N),E(_2),E(_2)],_3M):_3s(1,[0,1,E(_3N),E(_2),E(_2)],_3M):_3s(1,[0,1,E(_3N),E(_2),E(_2)],_3M);}}},_3R=new T(function(){return _3I(_0);}),_3S=0,_3T=[0,4],_3U=[0,_3T,_3T],_3V=[0,_3U,_3S],_3W=2,_3X=[0,12],_3Y=[0,15],_3Z=[0,_3Y,_3X],_40=[0,_3Z,_3W],_41=[1,_40,_0],_42=3,_43=[0,7],_44=[0,3],_45=[0,_44,_43],_46=[0,_45,_42],_47=[1,_46,_41],_48=[1,_3V,_47],_49=[0,_48,_3R],_4a=[0,_49,_1],_4b=[0,1],_4c=function(_4d,_4e){var _4f=E(_4d);return _4f[0]==0?E(_4e):[1,_4f[1],new T(function(){return _4c(_4f[2],_4e);})];},_4g=function(_4h){while(1){var _4i=E(_4h);if(!_4i[0]){_4h=[1,I_fromInt(_4i[1])];continue;}else{return I_toString(_4i[1]);}}},_4j=function(_4k,_4l){return _4c(fromJSStr(_4g(_4k)),_4l);},_4m=function(_4n,_4o){var _4p=E(_4n);if(!_4p[0]){var _4q=_4p[1],_4r=E(_4o);return _4r[0]==0?_4q<_4r[1]:I_compareInt(_4r[1],_4q)>0;}else{var _4s=_4p[1],_4t=E(_4o);return _4t[0]==0?I_compareInt(_4s,_4t[1])<0:I_compare(_4s,_4t[1])<0;}},_4u=[0,41],_4v=[0,40],_4w=[0,0],_4x=function(_4y,_4z,_4A){return _4y<=6?_4j(_4z,_4A):!_4m(_4z,_4w)?_4j(_4z,_4A):[1,_4v,new T(function(){return _4c(fromJSStr(_4g(_4z)),[1,_4u,_4A]);})];},_4B=0,_4C=function(_4D,_4E,_4F,_4G){return A(_4D,[new T(function(){return function(_){var _4H=jsSet(E(_4E)[1],toJSStr(E(_4F)),toJSStr(E(_4G)));return _4B;};})]);},_4I=[1],_4J=unCStr("Failure in Data.Map.balanceL"),_4K=new T(function(){return err(_4J);}),_4L=function(_4M,_4N,_4O,_4P){var _4Q=E(_4P);if(!_4Q[0]){var _4R=_4Q[1],_4S=E(_4O);if(!_4S[0]){var _4T=_4S[1],_4U=_4S[2],_4V=_4S[3];if(_4T<=(imul(3,_4R)|0)){return [0,(1+_4T|0)+_4R|0,E(E(_4M)),_4N,E(_4S),E(_4Q)];}else{var _4W=E(_4S[4]);if(!_4W[0]){var _4X=_4W[1],_4Y=E(_4S[5]);if(!_4Y[0]){var _4Z=_4Y[1],_50=_4Y[2],_51=_4Y[3],_52=_4Y[4];if(_4Z>=(imul(2,_4X)|0)){var _53=function(_54){var _55=E(_4Y[5]);return _55[0]==0?[0,(1+_4T|0)+_4R|0,E(_50),_51,E([0,(1+_4X|0)+_54|0,E(_4U),_4V,E(_4W),E(_52)]),E([0,(1+_4R|0)+_55[1]|0,E(E(_4M)),_4N,E(_55),E(_4Q)])]:[0,(1+_4T|0)+_4R|0,E(_50),_51,E([0,(1+_4X|0)+_54|0,E(_4U),_4V,E(_4W),E(_52)]),E([0,1+_4R|0,E(E(_4M)),_4N,E(_4I),E(_4Q)])];},_56=E(_52);return _56[0]==0?_53(_56[1]):_53(0);}else{return [0,(1+_4T|0)+_4R|0,E(_4U),_4V,E(_4W),E([0,(1+_4R|0)+_4Z|0,E(E(_4M)),_4N,E(_4Y),E(_4Q)])];}}else{return E(_4K);}}else{return E(_4K);}}}else{return [0,1+_4R|0,E(E(_4M)),_4N,E(_4I),E(_4Q)];}}else{var _57=E(_4O);if(!_57[0]){var _58=_57[1],_59=_57[2],_5a=_57[3],_5b=_57[5],_5c=E(_57[4]);if(!_5c[0]){var _5d=_5c[1],_5e=E(_5b);if(!_5e[0]){var _5f=_5e[1],_5g=_5e[2],_5h=_5e[3],_5i=_5e[4];if(_5f>=(imul(2,_5d)|0)){var _5j=function(_5k){var _5l=E(_5e[5]);return _5l[0]==0?[0,1+_58|0,E(_5g),_5h,E([0,(1+_5d|0)+_5k|0,E(_59),_5a,E(_5c),E(_5i)]),E([0,1+_5l[1]|0,E(E(_4M)),_4N,E(_5l),E(_4I)])]:[0,1+_58|0,E(_5g),_5h,E([0,(1+_5d|0)+_5k|0,E(_59),_5a,E(_5c),E(_5i)]),E([0,1,E(E(_4M)),_4N,E(_4I),E(_4I)])];},_5m=E(_5i);return _5m[0]==0?_5j(_5m[1]):_5j(0);}else{return [0,1+_58|0,E(_59),_5a,E(_5c),E([0,1+_5f|0,E(E(_4M)),_4N,E(_5e),E(_4I)])];}}else{return [0,3,E(_59),_5a,E(_5c),E([0,1,E(E(_4M)),_4N,E(_4I),E(_4I)])];}}else{var _5n=E(_5b);return _5n[0]==0?[0,3,E(_5n[2]),_5n[3],E([0,1,E(_59),_5a,E(_4I),E(_4I)]),E([0,1,E(E(_4M)),_4N,E(_4I),E(_4I)])]:[0,2,E(E(_4M)),_4N,E(_57),E(_4I)];}}else{return [0,1,E(E(_4M)),_4N,E(_4I),E(_4I)];}}},_5o=unCStr("Failure in Data.Map.balanceR"),_5p=new T(function(){return err(_5o);}),_5q=function(_5r,_5s,_5t,_5u){var _5v=E(_5t);if(!_5v[0]){var _5w=_5v[1],_5x=E(_5u);if(!_5x[0]){var _5y=_5x[1],_5z=_5x[2],_5A=_5x[3];if(_5y<=(imul(3,_5w)|0)){return [0,(1+_5w|0)+_5y|0,E(E(_5r)),_5s,E(_5v),E(_5x)];}else{var _5B=E(_5x[4]);if(!_5B[0]){var _5C=_5B[1],_5D=_5B[2],_5E=_5B[3],_5F=_5B[4],_5G=E(_5x[5]);if(!_5G[0]){var _5H=_5G[1];if(_5C>=(imul(2,_5H)|0)){var _5I=function(_5J){var _5K=E(_5r),_5L=E(_5B[5]);return _5L[0]==0?[0,(1+_5w|0)+_5y|0,E(_5D),_5E,E([0,(1+_5w|0)+_5J|0,E(_5K),_5s,E(_5v),E(_5F)]),E([0,(1+_5H|0)+_5L[1]|0,E(_5z),_5A,E(_5L),E(_5G)])]:[0,(1+_5w|0)+_5y|0,E(_5D),_5E,E([0,(1+_5w|0)+_5J|0,E(_5K),_5s,E(_5v),E(_5F)]),E([0,1+_5H|0,E(_5z),_5A,E(_4I),E(_5G)])];},_5M=E(_5F);return _5M[0]==0?_5I(_5M[1]):_5I(0);}else{return [0,(1+_5w|0)+_5y|0,E(_5z),_5A,E([0,(1+_5w|0)+_5C|0,E(E(_5r)),_5s,E(_5v),E(_5B)]),E(_5G)];}}else{return E(_5p);}}else{return E(_5p);}}}else{return [0,1+_5w|0,E(E(_5r)),_5s,E(_5v),E(_4I)];}}else{var _5N=E(_5u);if(!_5N[0]){var _5O=_5N[1],_5P=_5N[2],_5Q=_5N[3],_5R=_5N[5],_5S=E(_5N[4]);if(!_5S[0]){var _5T=_5S[1],_5U=_5S[2],_5V=_5S[3],_5W=_5S[4],_5X=E(_5R);if(!_5X[0]){var _5Y=_5X[1];if(_5T>=(imul(2,_5Y)|0)){var _5Z=function(_60){var _61=E(_5r),_62=E(_5S[5]);return _62[0]==0?[0,1+_5O|0,E(_5U),_5V,E([0,1+_60|0,E(_61),_5s,E(_4I),E(_5W)]),E([0,(1+_5Y|0)+_62[1]|0,E(_5P),_5Q,E(_62),E(_5X)])]:[0,1+_5O|0,E(_5U),_5V,E([0,1+_60|0,E(_61),_5s,E(_4I),E(_5W)]),E([0,1+_5Y|0,E(_5P),_5Q,E(_4I),E(_5X)])];},_63=E(_5W);return _63[0]==0?_5Z(_63[1]):_5Z(0);}else{return [0,1+_5O|0,E(_5P),_5Q,E([0,1+_5T|0,E(E(_5r)),_5s,E(_4I),E(_5S)]),E(_5X)];}}else{return [0,3,E(_5U),_5V,E([0,1,E(E(_5r)),_5s,E(_4I),E(_4I)]),E([0,1,E(_5P),_5Q,E(_4I),E(_4I)])];}}else{var _64=E(_5R);return _64[0]==0?[0,3,E(_5P),_5Q,E([0,1,E(E(_5r)),_5s,E(_4I),E(_4I)]),E(_64)]:[0,2,E(E(_5r)),_5s,E(_4I),E(_5N)];}}else{return [0,1,E(E(_5r)),_5s,E(_4I),E(_4I)];}}},_65=function(_66,_67,_68,_69){var _6a=E(_69);if(!_6a[0]){var _6b=_6a[3],_6c=_6a[4],_6d=_6a[5],_6e=E(_6a[2]),_6f=E(_6e[1])[1];if(_66>=_6f){if(_66!=_6f){return _5q(_6e,_6b,_6c,_65(_66,_67,_68,_6d));}else{var _6g=E(_6e[2])[1];return _67>=_6g?_67!=_6g?_5q(_6e,_6b,_6c,_65(_66,_67,_68,_6d)):[0,_6a[1],E([0,[0,_66],[0,_67]]),_68,E(_6c),E(_6d)]:_4L(_6e,_6b,_65(_66,_67,_68,_6c),_6d);}}else{return _4L(_6e,_6b,_65(_66,_67,_68,_6c),_6d);}}else{return [0,1,E([0,[0,_66],[0,_67]]),_68,E(_4I),E(_4I)];}},_6h=function(_6i,_6j,_6k,_6l){var _6m=E(_6l);if(!_6m[0]){var _6n=_6m[3],_6o=_6m[4],_6p=_6m[5],_6q=E(_6m[2]),_6r=E(_6q[1])[1];if(_6i>=_6r){if(_6i!=_6r){return _5q(_6q,_6n,_6o,_6h(_6i,_6j,_6k,_6p));}else{var _6s=E(_6j),_6t=_6s[1],_6u=E(_6q[2])[1];return _6t>=_6u?_6t!=_6u?_5q(_6q,_6n,_6o,_65(_6i,_6t,_6k,_6p)):[0,_6m[1],E([0,[0,_6i],_6s]),_6k,E(_6o),E(_6p)]:_4L(_6q,_6n,_65(_6i,_6t,_6k,_6o),_6p);}}else{return _4L(_6q,_6n,_6h(_6i,_6j,_6k,_6o),_6p);}}else{return [0,1,E([0,[0,_6i],_6j]),_6k,E(_4I),E(_4I)];}},_6v=function(_6w,_6x,_6y){var _6z=E(_6w),_6A=_6z[2],_6B=E(_6y);if(!_6B[0]){var _6C=_6B[3],_6D=_6B[4],_6E=_6B[5],_6F=E(_6B[2]),_6G=E(_6z[1])[1],_6H=E(_6F[1])[1];if(_6G>=_6H){if(_6G!=_6H){return _5q(_6F,_6C,_6D,_6h(_6G,_6A,_6x,_6E));}else{var _6I=E(_6A)[1],_6J=E(_6F[2])[1];return _6I>=_6J?_6I!=_6J?_5q(_6F,_6C,_6D,_65(_6G,_6I,_6x,_6E)):[0,_6B[1],E(_6z),_6x,E(_6D),E(_6E)]:_4L(_6F,_6C,_65(_6G,_6I,_6x,_6D),_6E);}}else{return _4L(_6F,_6C,_6h(_6G,_6A,_6x,_6D),_6E);}}else{return [0,1,E(_6z),_6x,E(_4I),E(_4I)];}},_6K=function(_6L,_6M){while(1){var _6N=E(_6M);if(!_6N[0]){return E(_6L);}else{var _6O=E(_6N[1]),_6P=_6v(_6O[1],_6O[2],_6L);_6M=_6N[2];_6L=_6P;continue;}}},_6Q=function(_6R,_6S){return [0,1,E(E(_6R)),_6S,E(_4I),E(_4I)];},_6T=function(_6U,_6V,_6W){var _6X=E(_6W);return _6X[0]==0?_5q(_6X[2],_6X[3],_6X[4],_6T(_6U,_6V,_6X[5])):_6Q(_6U,_6V);},_6Y=function(_6Z,_70,_71){var _72=E(_71);return _72[0]==0?_4L(_72[2],_72[3],_6Y(_6Z,_70,_72[4]),_72[5]):_6Q(_6Z,_70);},_73=function(_74,_75,_76,_77,_78,_79,_7a,_7b){var _7c=E(_76);if(!_7c[0]){var _7d=_7c[1],_7e=_7c[2],_7f=_7c[3],_7g=_7c[4],_7h=_7c[5];return (imul(3,_7d)|0)>=_77?(imul(3,_77)|0)>=_7d?[0,(_7d+_77|0)+1|0,E(E(_74)),_75,E(_7c),E([0,_77,E(_78),_79,E(_7a),E(_7b)])]:_5q(_7e,_7f,_7g,_73(_74,_75,_7h,_77,_78,_79,_7a,_7b)):_4L(_78,_79,_7i(_74,_75,_7d,_7e,_7f,_7g,_7h,_7a),_7b);}else{return _6Y(_74,_75,[0,_77,E(_78),_79,E(_7a),E(_7b)]);}},_7i=function(_7j,_7k,_7l,_7m,_7n,_7o,_7p,_7q){var _7r=E(_7q);if(!_7r[0]){var _7s=_7r[1],_7t=_7r[2],_7u=_7r[3],_7v=_7r[4],_7w=_7r[5];return (imul(3,_7l)|0)>=_7s?(imul(3,_7s)|0)>=_7l?[0,(_7l+_7s|0)+1|0,E(E(_7j)),_7k,E([0,_7l,E(_7m),_7n,E(_7o),E(_7p)]),E(_7r)]:_5q(_7m,_7n,_7o,_73(_7j,_7k,_7p,_7s,_7t,_7u,_7v,_7w)):_4L(_7t,_7u,_7i(_7j,_7k,_7l,_7m,_7n,_7o,_7p,_7v),_7w);}else{return _6T(_7j,_7k,[0,_7l,E(_7m),_7n,E(_7o),E(_7p)]);}},_7x=function(_7y,_7z,_7A,_7B){var _7C=E(_7A);if(!_7C[0]){var _7D=_7C[1],_7E=_7C[2],_7F=_7C[3],_7G=_7C[4],_7H=_7C[5],_7I=E(_7B);if(!_7I[0]){var _7J=_7I[1],_7K=_7I[2],_7L=_7I[3],_7M=_7I[4],_7N=_7I[5];return (imul(3,_7D)|0)>=_7J?(imul(3,_7J)|0)>=_7D?[0,(_7D+_7J|0)+1|0,E(E(_7y)),_7z,E(_7C),E(_7I)]:_5q(_7E,_7F,_7G,_73(_7y,_7z,_7H,_7J,_7K,_7L,_7M,_7N)):_4L(_7K,_7L,_7i(_7y,_7z,_7D,_7E,_7F,_7G,_7H,_7M),_7N);}else{return _6T(_7y,_7z,_7C);}}else{return _6Y(_7y,_7z,_7B);}},_7O=function(_7P,_7Q,_7R,_7S,_7T){var _7U=E(_7P);if(_7U==1){var _7V=E(_7T);if(!_7V[0]){return [0,[0,1,E([0,[0,_7Q],[0,_7R]]),_7S,E(_4I),E(_4I)],_0,_0];}else{var _7W=E(E(_7V[1])[1]),_7X=E(_7W[1])[1];return _7Q>=_7X?_7Q!=_7X?[0,[0,1,E([0,[0,_7Q],[0,_7R]]),_7S,E(_4I),E(_4I)],_0,_7V]:_7R>=E(_7W[2])[1]?[0,[0,1,E([0,[0,_7Q],[0,_7R]]),_7S,E(_4I),E(_4I)],_0,_7V]:[0,[0,1,E([0,[0,_7Q],[0,_7R]]),_7S,E(_4I),E(_4I)],_7V,_0]:[0,[0,1,E([0,[0,_7Q],[0,_7R]]),_7S,E(_4I),E(_4I)],_7V,_0];}}else{var _7Y=_7O(_7U>>1,_7Q,_7R,_7S,_7T),_7Z=_7Y[1],_80=_7Y[3],_81=E(_7Y[2]);if(!_81[0]){return [0,_7Z,_0,_80];}else{var _82=E(_81[1]),_83=_82[1],_84=_82[2],_85=E(_81[2]);if(!_85[0]){return [0,new T(function(){return _6T(_83,_84,_7Z);}),_0,_80];}else{var _86=_85[2],_87=E(_85[1]),_88=_87[2],_89=E(_83),_8a=E(_87[1]),_8b=_8a[2],_8c=E(_89[1])[1],_8d=E(_8a[1])[1];if(_8c>=_8d){if(_8c!=_8d){return [0,_7Z,_0,_81];}else{var _8e=E(_8b)[1];if(E(_89[2])[1]>=_8e){return [0,_7Z,_0,_81];}else{var _8f=_7O(_7U>>1,_8d,_8e,_88,_86);return [0,new T(function(){return _7x(_89,_84,_7Z,_8f[1]);}),_8f[2],_8f[3]];}}}else{var _8g=_8h(_7U>>1,_8d,_8b,_88,_86);return [0,new T(function(){return _7x(_89,_84,_7Z,_8g[1]);}),_8g[2],_8g[3]];}}}}},_8h=function(_8i,_8j,_8k,_8l,_8m){var _8n=E(_8i);if(_8n==1){var _8o=E(_8m);if(!_8o[0]){return [0,[0,1,E([0,[0,_8j],_8k]),_8l,E(_4I),E(_4I)],_0,_0];}else{var _8p=E(E(_8o[1])[1]),_8q=E(_8p[1])[1];if(_8j>=_8q){if(_8j!=_8q){return [0,[0,1,E([0,[0,_8j],_8k]),_8l,E(_4I),E(_4I)],_0,_8o];}else{var _8r=E(_8k);return _8r[1]>=E(_8p[2])[1]?[0,[0,1,E([0,[0,_8j],_8r]),_8l,E(_4I),E(_4I)],_0,_8o]:[0,[0,1,E([0,[0,_8j],_8r]),_8l,E(_4I),E(_4I)],_8o,_0];}}else{return [0,[0,1,E([0,[0,_8j],_8k]),_8l,E(_4I),E(_4I)],_8o,_0];}}}else{var _8s=_8h(_8n>>1,_8j,_8k,_8l,_8m),_8t=_8s[1],_8u=_8s[3],_8v=E(_8s[2]);if(!_8v[0]){return [0,_8t,_0,_8u];}else{var _8w=E(_8v[1]),_8x=_8w[1],_8y=_8w[2],_8z=E(_8v[2]);if(!_8z[0]){return [0,new T(function(){return _6T(_8x,_8y,_8t);}),_0,_8u];}else{var _8A=_8z[2],_8B=E(_8z[1]),_8C=_8B[2],_8D=E(_8x),_8E=E(_8B[1]),_8F=_8E[2],_8G=E(_8D[1])[1],_8H=E(_8E[1])[1];if(_8G>=_8H){if(_8G!=_8H){return [0,_8t,_0,_8v];}else{var _8I=E(_8F)[1];if(E(_8D[2])[1]>=_8I){return [0,_8t,_0,_8v];}else{var _8J=_7O(_8n>>1,_8H,_8I,_8C,_8A);return [0,new T(function(){return _7x(_8D,_8y,_8t,_8J[1]);}),_8J[2],_8J[3]];}}}else{var _8K=_8h(_8n>>1,_8H,_8F,_8C,_8A);return [0,new T(function(){return _7x(_8D,_8y,_8t,_8K[1]);}),_8K[2],_8K[3]];}}}}},_8L=function(_8M,_8N,_8O){var _8P=E(_8O);if(!_8P[0]){return E(_8N);}else{var _8Q=E(_8P[1]),_8R=_8Q[1],_8S=_8Q[2],_8T=E(_8P[2]);if(!_8T[0]){return _6T(_8R,_8S,_8N);}else{var _8U=E(_8T[1]),_8V=E(_8R),_8W=E(_8U[1]),_8X=_8W[2],_8Y=E(_8V[1])[1],_8Z=E(_8W[1])[1],_90=new T(function(){var _91=_8h(_8M,_8Z,_8X,_8U[2],_8T[2]),_92=_91[1],_93=E(_91[3]);return _93[0]==0?_8L(_8M<<1,_7x(_8V,_8S,_8N,_92),_91[2]):_6K(_7x(_8V,_8S,_8N,_92),_93);});return _8Y>=_8Z?_8Y!=_8Z?_6K(_8N,_8P):E(_8V[2])[1]>=E(_8X)[1]?_6K(_8N,_8P):E(_90):E(_90);}}},_94=function(_95){var _96=E(_95);if(!_96[0]){return [1];}else{var _97=E(_96[1]),_98=_97[1],_99=_97[2],_9a=E(_96[2]);if(!_9a[0]){return [0,1,E(E(_98)),_99,E(_4I),E(_4I)];}else{var _9b=E(_98),_9c=E(E(_9a[1])[1]),_9d=E(_9b[1])[1],_9e=E(_9c[1])[1];return _9d>=_9e?_9d!=_9e?_6K([0,1,E(_9b),_99,E(_4I),E(_4I)],_9a):E(_9b[2])[1]>=E(_9c[2])[1]?_6K([0,1,E(_9b),_99,E(_4I),E(_4I)],_9a):_8L(1,[0,1,E(_9b),_99,E(_4I),E(_4I)],_9a):_8L(1,[0,1,E(_9b),_99,E(_4I),E(_4I)],_9a);}}},_9f=function(_9g,_9h){var _9i=E(_9g),_9j=E(_9h);return _9i[1]>_9j[1]?E(_9i):E(_9j);},_9k=function(_9l,_9m){var _9n=E(_9l),_9o=E(_9m);return _9n[1]>_9o[1]?E(_9o):E(_9n);},_9p=function(_9q,_9r){var _9s=E(_9q),_9t=E(_9r);return [0,new T(function(){return _9f(_9s[1],_9t[1]);}),new T(function(){return _9f(_9s[2],_9t[2]);})];},_9u=function(_9v,_9w){var _9x=E(_9v),_9y=E(_9w);return [0,new T(function(){return _9k(_9x[1],_9y[1]);}),new T(function(){return _9k(_9x[2],_9y[2]);})];},_9z=function(_9A,_9B,_9C){while(1){var _9D=(function(_9E,_9F,_9G){var _9H=E(_9G);if(!_9H[0]){return [0,_9E,_9F];}else{var _9I=E(_9H[1])[1];_9A=new T(function(){return _9u(_9E,_9I);});_9B=new T(function(){return _9p(_9F,_9I);});_9C=_9H[2];return null;}})(_9A,_9B,_9C);if(_9D!=null){return _9D;}}},_9J=[0,0],_9K=[0,_9J,_9J],_9L=[0,_9J,_9J],_9M=function(_9N,_9O){while(1){var _9P=(function(_9Q,_9R){var _9S=E(_9R);if(!_9S[0]){var _9T=E(_9S[2]),_9U=_9T[1],_9V=_9T[2],_9W=_9M(_9Q,_9S[3]);_9N=[0,new T(function(){var _9X=E(_9W[1]);return [0,new T(function(){return _9k(_9X[1],_9U);}),new T(function(){return _9k(_9X[2],_9V);})];}),new T(function(){var _9Y=E(_9W[2]);return [0,new T(function(){return _9f(_9Y[1],_9U);}),new T(function(){return _9f(_9Y[2],_9V);})];})];_9O=_9S[4];return null;}else{return E(_9Q);}})(_9N,_9O);if(_9P!=null){return _9P;}}},_9Z=[0,_9L,_9K],_a0=function(_a1,_a2){var _a3=new T(function(){var _a4=_9z(_9L,_9K,_a1);return [0,_a4[1],_a4[2]];}),_a5=new T(function(){return _9M(_9Z,_a2);});return [0,new T(function(){var _a6=E(E(_a5)[1]),_a7=E(E(_a3)[1]);return [0,new T(function(){return _9k(_a6[1],_a7[1]);}),new T(function(){return _9k(_a6[2],_a7[2]);})];}),new T(function(){var _a8=E(E(_a5)[2]),_a9=E(E(_a3)[2]);return [0,new T(function(){return _9f(_a8[1],_a9[1]);}),new T(function(){return _9f(_a8[2],_a9[2]);})];})];},_aa=function(_ab,_ac,_ad){while(1){var _ae=E(_ad);if(!_ae[0]){var _af=_ae[3],_ag=_ae[4],_ah=E(_ae[2]),_ai=E(_ah[1])[1];if(_ab>=_ai){if(_ab!=_ai){_ad=_ag;continue;}else{var _aj=E(_ah[2])[1];if(_ac>=_aj){if(_ac!=_aj){_ad=_ag;continue;}else{return true;}}else{_ad=_af;continue;}}}else{_ad=_af;continue;}}else{return false;}}},_ak=function(_al,_am,_an){while(1){var _ao=E(_an);if(!_ao[0]){var _ap=_ao[3],_aq=_ao[4],_ar=E(_ao[2]),_as=E(_ar[1])[1];if(_al>=_as){if(_al!=_as){_an=_aq;continue;}else{var _at=E(_am)[1],_au=E(_ar[2])[1];return _at>=_au?_at!=_au?_aa(_al,_at,_aq):true:_aa(_al,_at,_ap);}}else{_an=_ap;continue;}}else{return false;}}},_av=function(_aw,_ax,_ay){var _az=E(_ay);if(!_az[0]){var _aA=_az[3],_aB=_az[4],_aC=E(_az[2]),_aD=E(_aw)[1],_aE=E(_aC[1])[1];if(_aD>=_aE){if(_aD!=_aE){return _ak(_aD,_ax,_aB);}else{var _aF=E(_ax)[1],_aG=E(_aC[2])[1];return _aF>=_aG?_aF!=_aG?_aa(_aD,_aF,_aB):true:_aa(_aD,_aF,_aA);}}else{return _ak(_aD,_ax,_aA);}}else{return false;}},_aH=function(_aI,_aJ,_aK){while(1){var _aL=E(_aK);if(!_aL[0]){var _aM=_aL[4],_aN=_aL[5],_aO=E(_aL[2]),_aP=E(_aO[1])[1];if(_aI>=_aP){if(_aI!=_aP){_aK=_aN;continue;}else{var _aQ=E(_aO[2])[1];if(_aJ>=_aQ){if(_aJ!=_aQ){_aK=_aN;continue;}else{return [1,_aL[3]];}}else{_aK=_aM;continue;}}}else{_aK=_aM;continue;}}else{return [0];}}},_aR=function(_aS,_aT,_aU){while(1){var _aV=E(_aU);if(!_aV[0]){var _aW=_aV[4],_aX=_aV[5],_aY=E(_aV[2]),_aZ=E(_aY[1])[1];if(_aS>=_aZ){if(_aS!=_aZ){_aU=_aX;continue;}else{var _b0=E(_aT)[1],_b1=E(_aY[2])[1];return _b0>=_b1?_b0!=_b1?_aH(_aS,_b0,_aX):[1,_aV[3]]:_aH(_aS,_b0,_aW);}}else{_aU=_aW;continue;}}else{return [0];}}},_b2=function(_b3,_b4,_b5){var _b6=E(_b5);if(!_b6[0]){var _b7=_b6[4],_b8=_b6[5],_b9=E(_b6[2]),_ba=E(_b3)[1],_bb=E(_b9[1])[1];if(_ba>=_bb){if(_ba!=_bb){return _aR(_ba,_b4,_b8);}else{var _bc=E(_b4)[1],_bd=E(_b9[2])[1];return _bc>=_bd?_bc!=_bd?_aH(_ba,_bc,_b8):[1,_b6[3]]:_aH(_ba,_bc,_b7);}}else{return _aR(_ba,_b4,_b7);}}else{return [0];}},_be=function(_bf,_bg,_bh){while(1){var _bi=E(_bh);if(!_bi[0]){var _bj=_bi[4],_bk=_bi[5],_bl=E(_bi[2]),_bm=E(_bl[1])[1];if(_bf>=_bm){if(_bf!=_bm){_bh=_bk;continue;}else{var _bn=E(_bl[2])[1];if(_bg>=_bn){if(_bg!=_bn){_bh=_bk;continue;}else{return true;}}else{_bh=_bj;continue;}}}else{_bh=_bj;continue;}}else{return false;}}},_bo=function(_bp,_bq,_br){while(1){var _bs=E(_br);if(!_bs[0]){var _bt=_bs[4],_bu=_bs[5],_bv=E(_bs[2]),_bw=E(_bv[1])[1];if(_bp>=_bw){if(_bp!=_bw){_br=_bu;continue;}else{var _bx=E(_bq)[1],_by=E(_bv[2])[1];return _bx>=_by?_bx!=_by?_be(_bp,_bx,_bu):true:_be(_bp,_bx,_bt);}}else{_br=_bt;continue;}}else{return false;}}},_bz=function(_bA,_bB,_bC){var _bD=E(_bC);if(!_bD[0]){var _bE=_bD[4],_bF=_bD[5],_bG=E(_bD[2]),_bH=E(_bA)[1],_bI=E(_bG[1])[1];if(_bH>=_bI){if(_bH!=_bI){return _bo(_bH,_bB,_bF);}else{var _bJ=E(_bB)[1],_bK=E(_bG[2])[1];return _bJ>=_bK?_bJ!=_bK?_be(_bH,_bJ,_bF):true:_be(_bH,_bJ,_bE);}}else{return _bo(_bH,_bB,_bE);}}else{return false;}},_bL=[0,10],_bM=function(_bN){var _bO=E(_bN);return _bO[0]==0?[0]:_4c(_bO[1],[1,_bL,new T(function(){return _bM(_bO[2]);})]);},_bP=new T(function(){return _bM(_0);}),_bQ=[0,8592],_bR=[0,8595],_bS=[0,8594],_bT=[0,8593],_bU=[0,79],_bV=[0,32],_bW=function(_bX){var _bY=E(_bX);return [0,_bY[1],_bY[2]];},_bZ=function(_c0,_c1){if(_c0<=_c1){var _c2=function(_c3){return [1,[0,_c3],new T(function(){return _c3!=_c1?_c2(_c3+1|0):[0];})];};return _c2(_c0);}else{return [0];}},_c4=unCStr("Maybe.fromJust: Nothing"),_c5=new T(function(){return err(_c4);}),_c6=function(_c7,_c8){var _c9=E(_c8);return _c9[0]==0?[0]:[1,new T(function(){return A(_c7,[_c9[1]]);}),new T(function(){return _c6(_c7,_c9[2]);})];},_ca=function(_cb,_cc){var _cd=_a0(_cb,_cc),_ce=E(_cd[1]),_cf=E(_cd[2]),_cg=E(_ce[2])[1],_ch=E(_cf[2])[1];if(_cg<=_ch){var _ci=new T(function(){return _94(_c6(_bW,_cb));}),_cj=new T(function(){return _bZ(E(_ce[1])[1],E(_cf[1])[1]);}),_ck=function(_cl){return [1,new T(function(){var _cm=[0,_cl],_cn=function(_co){var _cp=E(_co);if(!_cp[0]){return [0];}else{var _cq=_cp[1];return [1,new T(function(){if(!_bz(_cq,_cm,_ci)){return !_av(_cq,_cm,_cc)?E(_bV):E(_bU);}else{var _cr=_b2(_cq,_cm,_ci);if(!_cr[0]){return E(_c5);}else{switch(E(_cr[1])){case 0:return E(_bT);case 1:return E(_bS);case 2:return E(_bR);default:return E(_bQ);}}}}),new T(function(){return _cn(_cp[2]);})];}};return _cn(_cj);}),new T(function(){return _cl!=_ch?_ck(_cl+1|0):[0];})];};return _bM(_ck(_cg));}else{return E(_bP);}},_cs=unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"),_ct=new T(function(){return err(_cs);}),_cu=function(_cv){var _cw=E(E(_cv)[1]);return _cw==(-2147483648)?E(_ct):[0,_cw-1|0];},_cx=unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"),_cy=new T(function(){return err(_cx);}),_cz=function(_cA){var _cB=E(E(_cA)[1]);return _cB==2147483647?E(_cy):[0,_cB+1|0];},_cC=1,_cD=function(_cE,_cF,_cG,_cH){var _cI=E(_cH);if(!_cI[0]){var _cJ=new T(function(){var _cK=_cD(_cI[1],_cI[2],_cI[3],_cI[4]);return [0,_cK[1],_cK[2]];});return [0,new T(function(){return E(E(_cJ)[1]);}),new T(function(){return _5(_cF,_cG,E(_cJ)[2]);})];}else{return [0,_cF,_cG];}},_cL=function(_cM,_cN,_cO,_cP){var _cQ=E(_cO);if(!_cQ[0]){var _cR=new T(function(){var _cS=_cL(_cQ[1],_cQ[2],_cQ[3],_cQ[4]);return [0,_cS[1],_cS[2]];});return [0,new T(function(){return E(E(_cR)[1]);}),new T(function(){return _F(_cN,E(_cR)[2],_cP);})];}else{return [0,_cN,_cP];}},_cT=function(_cU,_cV){var _cW=E(_cU);if(!_cW[0]){var _cX=_cW[1],_cY=E(_cV);if(!_cY[0]){var _cZ=_cY[1];if(_cX<=_cZ){var _d0=_cL(_cZ,_cY[2],_cY[3],_cY[4]);return _5(_d0[1],_cW,_d0[2]);}else{var _d1=_cD(_cX,_cW[2],_cW[3],_cW[4]);return _F(_d1[1],_d1[2],_cY);}}else{return E(_cW);}}else{return E(_cV);}},_d2=function(_d3,_d4,_d5){var _d6=E(_d5);if(!_d6[0]){var _d7=_d6[3],_d8=_d6[4],_d9=E(_d6[2]),_da=E(_d9[1])[1];if(_d3>=_da){if(_d3!=_da){return _5(_d9,_d7,_d2(_d3,_d4,_d8));}else{var _db=E(_d9[2])[1];return _d4>=_db?_d4!=_db?_5(_d9,_d7,_d2(_d3,_d4,_d8)):_cT(_d7,_d8):_F(_d9,_d2(_d3,_d4,_d7),_d8);}}else{return _F(_d9,_d2(_d3,_d4,_d7),_d8);}}else{return [1];}},_dc=function(_dd,_de,_df){var _dg=E(_df);if(!_dg[0]){var _dh=_dg[3],_di=_dg[4],_dj=E(_dg[2]),_dk=E(_dj[1])[1];if(_dd>=_dk){if(_dd!=_dk){return _5(_dj,_dh,_dc(_dd,_de,_di));}else{var _dl=E(_de)[1],_dm=E(_dj[2])[1];return _dl>=_dm?_dl!=_dm?_5(_dj,_dh,_d2(_dd,_dl,_di)):_cT(_dh,_di):_F(_dj,_d2(_dd,_dl,_dh),_di);}}else{return _F(_dj,_dc(_dd,_de,_dh),_di);}}else{return [1];}},_dn=function(_do,_dp,_dq){var _dr=E(_dq);if(!_dr[0]){var _ds=_dr[3],_dt=_dr[4],_du=E(_dr[2]),_dv=E(_do)[1],_dw=E(_du[1])[1];if(_dv>=_dw){if(_dv!=_dw){return _5(_du,_ds,_dc(_dv,_dp,_dt));}else{var _dx=E(_dp)[1],_dy=E(_du[2])[1];return _dx>=_dy?_dx!=_dy?_5(_du,_ds,_d2(_dv,_dx,_dt)):_cT(_ds,_dt):_F(_du,_d2(_dv,_dx,_ds),_dt);}}else{return _F(_du,_dc(_dv,_dp,_ds),_dt);}}else{return [1];}},_dz=function(_dA,_dB,_dC){return !_av(_dA,_dB,_dC)?_1B([0,_dA,_dB],_dC):_dn(_dA,_dB,_dC);},_dD=function(_dE,_dF){var _dG=E(_dE);return _dz(_dG[1],_dG[2],_dF);},_dH=function(_dI,_dJ){while(1){var _dK=E(_dJ);if(!_dK[0]){return E(_dI);}else{var _dL=_dD(E(_dK[1])[1],_dI);_dJ=_dK[2];_dI=_dL;continue;}}},_dM=function(_dN,_dO){return [0,new T(function(){return _c6(function(_dP){var _dQ=E(_dP),_dR=_dQ[2],_dS=E(_dQ[1]),_dT=_dS[1],_dU=_dS[2];if(!_av(_dT,_dU,_dO)){switch(E(_dR)){case 0:return [0,[0,new T(function(){return _cz(_dT);}),_dU],_cC];case 1:return [0,[0,_dT,new T(function(){return _cz(_dU);})],_3W];case 2:return [0,[0,new T(function(){return _cu(_dT);}),_dU],_42];default:return [0,[0,_dT,new T(function(){return _cu(_dU);})],_3S];}}else{switch(E(_dR)){case 0:return [0,[0,new T(function(){return _cu(_dT);}),_dU],_42];case 1:return [0,[0,_dT,new T(function(){return _cu(_dU);})],_3S];case 2:return [0,[0,new T(function(){return _cz(_dT);}),_dU],_cC];default:return [0,[0,_dT,new T(function(){return _cz(_dU);})],_3W];}}},_dN);}),new T(function(){return _dH(_dO,_dN);})];},_dV=unCStr("world"),_dW=[0,50],_dX=unCStr("generations"),_dY=unCStr("innerHTML"),_dZ=function(_e0){return E(_e0);},_e1=function(_e2,_e3){while(1){var _e4=E(_e2);if(!_e4[0]){var _e5=_e4[1],_e6=E(_e3);if(!_e6[0]){var _e7=_e6[1],_e8=addC(_e5,_e7);if(!E(_e8[2])){return [0,_e8[1]];}else{_e2=[1,I_fromInt(_e5)];_e3=[1,I_fromInt(_e7)];continue;}}else{_e2=[1,I_fromInt(_e5)];_e3=_e6;continue;}}else{var _e9=E(_e3);if(!_e9[0]){_e2=_e4;_e3=[1,I_fromInt(_e9[1])];continue;}else{return [1,I_add(_e4[1],_e9[1])];}}}},_ea=function(_eb){return [2];},_ec=function(_ed,_){while(1){var _ee=E(_ed);if(!_ee[0]){return _4B;}else{var _ef=_ee[2],_eg=E(_ee[1]);switch(_eg[0]){case 0:var _eh=A(_eg[1],[_]);_ed=_4c(_ef,[1,_eh,_0]);continue;case 1:_ed=_4c(_ef,_eg[1]);continue;default:_ed=_ef;continue;}}}},_ei=[2],_ej=function(_ek){return _ea(_ek);},_el=function(_em,_en,_eo){return [0,function(_){var _ep=E(_em)[1],_eq=rMV(_ep),_er=E(_eq);if(!_er[0]){var _=wMV(_ep,[0,_er[1],new T(function(){var _es=new T(function(){return A(_eo,[_4B]);});return _4c(_er[2],[1,[0,_en,function(_et){return E(_es);}],_0]);})]);return _ei;}else{var _eu=E(_er[1]);if(!_eu[0]){var _=wMV(_ep,[0,_en,_0]);return new T(function(){return A(_eo,[_4B]);});}else{var _=wMV(_ep,[1,_eu[2]]);return [1,[1,new T(function(){return A(_eo,[_4B]);}),[1,new T(function(){return A(_eu[1],[_en,_ej]);}),_0]]];}}}];},_ev=[1,_0],_ew=function(_ex,_ey){return [0,function(_){var _ez=E(_ex)[1],_eA=rMV(_ez),_eB=E(_eA);if(!_eB[0]){var _eC=_eB[1],_eD=E(_eB[2]);if(!_eD[0]){var _=wMV(_ez,_ev);return new T(function(){return A(_ey,[_eC]);});}else{var _eE=E(_eD[1]),_=wMV(_ez,[0,_eE[1],_eD[2]]);return [1,[1,new T(function(){return A(_ey,[_eC]);}),[1,new T(function(){return A(_eE[2],[_ej]);}),_0]]];}}else{var _=wMV(_ez,[1,new T(function(){return _4c(_eB[1],[1,function(_eF){var _eG=new T(function(){return A(_ey,[_eF]);});return function(_eH){return E(_eG);};},_0]);})]);return _ei;}}];},_eI=function(_eJ,_eK){return [0,function(_){var _eL=nMV(_ev),_eM=[0,_eL];return [0,function(_){var _eN=jsSetTimeout(E(_eJ)[1],function(_){return _ec([1,new T(function(){return _el(_eM,_4B,_ea);}),_0],_);});return new T(function(){return _ew(_eM,_eK);});}];}];},_eO=unCStr(" could be found!"),_eP=function(_eQ){return err(unAppCStr("No element with ID ",new T(function(){return _4c(_eQ,_eO);})));},_eR=function(_eS){var _eT=E(_eS),_eU=_eT[1],_eV=_eT[2],_eW=new T(function(){return _e1(_eV,_4b);}),_eX=new T(function(){return _4x(0,_eV,_0);});return function(_eY){return [0,function(_){var _eZ=E(_dV),_f0=jsFind(toJSStr(_eZ)),_f1=E(_f0);if(!_f1[0]){return _eP(_eZ);}else{var _f2=A(_4C,[_dZ,_f1[1],_dY,new T(function(){var _f3=E(_eU);return _ca(_f3[1],_f3[2]);}),_]);return [0,function(_){var _f4=E(_dX),_f5=jsFind(toJSStr(_f4)),_f6=E(_f5);if(!_f6[0]){return _eP(_f4);}else{var _f7=A(_4C,[_dZ,_f6[1],_dY,_eX,_]);return new T(function(){var _f8=new T(function(){return A(_eY,[[0,new T(function(){var _f9=E(_eU),_fa=_dM(_f9[1],_f9[2]);return [0,_fa[1],_fa[2]];}),_eW]]);});return _eI(_dW,function(_fb){return E(_f8);});});}}];}}];};},_fc=function(_fd,_fe){var _ff=new T(function(){return A(_fd,[_fe]);});return function(_fg){return A(_ff,[function(_fh){return A(_fc,[_fd,_fh,_fg]);}]);};},_fi=new T(function(){return A(_fc,[_eR,_4a,_ea]);}),_fj=[1,_fi,_0],_fk=function(_){return _ec(_fj,_);},_fl=function(_){return _fk(_);};
var hasteMain = function() {A(_fl, [0]);};window.onload = hasteMain;