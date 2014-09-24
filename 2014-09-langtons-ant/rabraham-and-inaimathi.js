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

var _0=unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"),_1=new T(function(){return err(_0);}),_2=0,_3=function(_4,_5,_6,_7){return A(_4,[new T(function(){return function(_){var _8=jsSet(E(_5)[1],toJSStr(E(_6)),toJSStr(E(_7)));return _2;};})]);},_9=function(_a,_b){var _c=E(_a);return _c[0]==0?E(_b):[1,_c[1],new T(function(){return _9(_c[2],_b);})];},_d=function(_e,_f){var _g=jsShowI(_e);return _9(fromJSStr(_g),_f);},_h=[0,41],_i=[0,40],_j=function(_k,_l,_m){return _l>=0?_d(_l,_m):_k<=6?_d(_l,_m):[1,_i,new T(function(){var _n=jsShowI(_l);return _9(fromJSStr(_n),[1,_h,_m]);})];},_o=[0],_p=function(_q){return E(_q);},_r=unCStr("world"),_s=unCStr("generations"),_t=unCStr("innerHTML"),_u=[1],_v=unCStr("Failure in Data.Map.balanceL"),_w=new T(function(){return err(_v);}),_x=function(_y,_z,_A,_B){var _C=E(_B);if(!_C[0]){var _D=_C[1],_E=E(_A);if(!_E[0]){var _F=_E[1],_G=_E[2],_H=_E[3];if(_F<=(imul(3,_D)|0)){return [0,(1+_F|0)+_D|0,E(E(_y)),_z,E(_E),E(_C)];}else{var _I=E(_E[4]);if(!_I[0]){var _J=_I[1],_K=E(_E[5]);if(!_K[0]){var _L=_K[1],_M=_K[2],_N=_K[3],_O=_K[4];if(_L>=(imul(2,_J)|0)){var _P=function(_Q){var _R=E(_K[5]);return _R[0]==0?[0,(1+_F|0)+_D|0,E(_M),_N,E([0,(1+_J|0)+_Q|0,E(_G),_H,E(_I),E(_O)]),E([0,(1+_D|0)+_R[1]|0,E(E(_y)),_z,E(_R),E(_C)])]:[0,(1+_F|0)+_D|0,E(_M),_N,E([0,(1+_J|0)+_Q|0,E(_G),_H,E(_I),E(_O)]),E([0,1+_D|0,E(E(_y)),_z,E(_u),E(_C)])];},_S=E(_O);return _S[0]==0?_P(_S[1]):_P(0);}else{return [0,(1+_F|0)+_D|0,E(_G),_H,E(_I),E([0,(1+_D|0)+_L|0,E(E(_y)),_z,E(_K),E(_C)])];}}else{return E(_w);}}else{return E(_w);}}}else{return [0,1+_D|0,E(E(_y)),_z,E(_u),E(_C)];}}else{var _T=E(_A);if(!_T[0]){var _U=_T[1],_V=_T[2],_W=_T[3],_X=_T[5],_Y=E(_T[4]);if(!_Y[0]){var _Z=_Y[1],_10=E(_X);if(!_10[0]){var _11=_10[1],_12=_10[2],_13=_10[3],_14=_10[4];if(_11>=(imul(2,_Z)|0)){var _15=function(_16){var _17=E(_10[5]);return _17[0]==0?[0,1+_U|0,E(_12),_13,E([0,(1+_Z|0)+_16|0,E(_V),_W,E(_Y),E(_14)]),E([0,1+_17[1]|0,E(E(_y)),_z,E(_17),E(_u)])]:[0,1+_U|0,E(_12),_13,E([0,(1+_Z|0)+_16|0,E(_V),_W,E(_Y),E(_14)]),E([0,1,E(E(_y)),_z,E(_u),E(_u)])];},_18=E(_14);return _18[0]==0?_15(_18[1]):_15(0);}else{return [0,1+_U|0,E(_V),_W,E(_Y),E([0,1+_11|0,E(E(_y)),_z,E(_10),E(_u)])];}}else{return [0,3,E(_V),_W,E(_Y),E([0,1,E(E(_y)),_z,E(_u),E(_u)])];}}else{var _19=E(_X);return _19[0]==0?[0,3,E(_19[2]),_19[3],E([0,1,E(_V),_W,E(_u),E(_u)]),E([0,1,E(E(_y)),_z,E(_u),E(_u)])]:[0,2,E(E(_y)),_z,E(_T),E(_u)];}}else{return [0,1,E(E(_y)),_z,E(_u),E(_u)];}}},_1a=unCStr("Failure in Data.Map.balanceR"),_1b=new T(function(){return err(_1a);}),_1c=function(_1d,_1e,_1f,_1g){var _1h=E(_1f);if(!_1h[0]){var _1i=_1h[1],_1j=E(_1g);if(!_1j[0]){var _1k=_1j[1],_1l=_1j[2],_1m=_1j[3];if(_1k<=(imul(3,_1i)|0)){return [0,(1+_1i|0)+_1k|0,E(E(_1d)),_1e,E(_1h),E(_1j)];}else{var _1n=E(_1j[4]);if(!_1n[0]){var _1o=_1n[1],_1p=_1n[2],_1q=_1n[3],_1r=_1n[4],_1s=E(_1j[5]);if(!_1s[0]){var _1t=_1s[1];if(_1o>=(imul(2,_1t)|0)){var _1u=function(_1v){var _1w=E(_1d),_1x=E(_1n[5]);return _1x[0]==0?[0,(1+_1i|0)+_1k|0,E(_1p),_1q,E([0,(1+_1i|0)+_1v|0,E(_1w),_1e,E(_1h),E(_1r)]),E([0,(1+_1t|0)+_1x[1]|0,E(_1l),_1m,E(_1x),E(_1s)])]:[0,(1+_1i|0)+_1k|0,E(_1p),_1q,E([0,(1+_1i|0)+_1v|0,E(_1w),_1e,E(_1h),E(_1r)]),E([0,1+_1t|0,E(_1l),_1m,E(_u),E(_1s)])];},_1y=E(_1r);return _1y[0]==0?_1u(_1y[1]):_1u(0);}else{return [0,(1+_1i|0)+_1k|0,E(_1l),_1m,E([0,(1+_1i|0)+_1o|0,E(E(_1d)),_1e,E(_1h),E(_1n)]),E(_1s)];}}else{return E(_1b);}}else{return E(_1b);}}}else{return [0,1+_1i|0,E(E(_1d)),_1e,E(_1h),E(_u)];}}else{var _1z=E(_1g);if(!_1z[0]){var _1A=_1z[1],_1B=_1z[2],_1C=_1z[3],_1D=_1z[5],_1E=E(_1z[4]);if(!_1E[0]){var _1F=_1E[1],_1G=_1E[2],_1H=_1E[3],_1I=_1E[4],_1J=E(_1D);if(!_1J[0]){var _1K=_1J[1];if(_1F>=(imul(2,_1K)|0)){var _1L=function(_1M){var _1N=E(_1d),_1O=E(_1E[5]);return _1O[0]==0?[0,1+_1A|0,E(_1G),_1H,E([0,1+_1M|0,E(_1N),_1e,E(_u),E(_1I)]),E([0,(1+_1K|0)+_1O[1]|0,E(_1B),_1C,E(_1O),E(_1J)])]:[0,1+_1A|0,E(_1G),_1H,E([0,1+_1M|0,E(_1N),_1e,E(_u),E(_1I)]),E([0,1+_1K|0,E(_1B),_1C,E(_u),E(_1J)])];},_1P=E(_1I);return _1P[0]==0?_1L(_1P[1]):_1L(0);}else{return [0,1+_1A|0,E(_1B),_1C,E([0,1+_1F|0,E(E(_1d)),_1e,E(_u),E(_1E)]),E(_1J)];}}else{return [0,3,E(_1G),_1H,E([0,1,E(E(_1d)),_1e,E(_u),E(_u)]),E([0,1,E(_1B),_1C,E(_u),E(_u)])];}}else{var _1Q=E(_1D);return _1Q[0]==0?[0,3,E(_1B),_1C,E([0,1,E(E(_1d)),_1e,E(_u),E(_u)]),E(_1Q)]:[0,2,E(E(_1d)),_1e,E(_u),E(_1z)];}}else{return [0,1,E(E(_1d)),_1e,E(_u),E(_u)];}}},_1R=function(_1S,_1T,_1U,_1V){var _1W=E(_1V);if(!_1W[0]){var _1X=_1W[3],_1Y=_1W[4],_1Z=_1W[5],_20=E(_1W[2]),_21=E(_20[1])[1];if(_1S>=_21){if(_1S!=_21){return _1c(_20,_1X,_1Y,_1R(_1S,_1T,_1U,_1Z));}else{var _22=E(_20[2])[1];return _1T>=_22?_1T!=_22?_1c(_20,_1X,_1Y,_1R(_1S,_1T,_1U,_1Z)):[0,_1W[1],E([0,[0,_1S],[0,_1T]]),_1U,E(_1Y),E(_1Z)]:_x(_20,_1X,_1R(_1S,_1T,_1U,_1Y),_1Z);}}else{return _x(_20,_1X,_1R(_1S,_1T,_1U,_1Y),_1Z);}}else{return [0,1,E([0,[0,_1S],[0,_1T]]),_1U,E(_u),E(_u)];}},_23=function(_24,_25,_26,_27){var _28=E(_27);if(!_28[0]){var _29=_28[3],_2a=_28[4],_2b=_28[5],_2c=E(_28[2]),_2d=E(_2c[1])[1];if(_24>=_2d){if(_24!=_2d){return _1c(_2c,_29,_2a,_23(_24,_25,_26,_2b));}else{var _2e=E(_25),_2f=_2e[1],_2g=E(_2c[2])[1];return _2f>=_2g?_2f!=_2g?_1c(_2c,_29,_2a,_1R(_24,_2f,_26,_2b)):[0,_28[1],E([0,[0,_24],_2e]),_26,E(_2a),E(_2b)]:_x(_2c,_29,_1R(_24,_2f,_26,_2a),_2b);}}else{return _x(_2c,_29,_23(_24,_25,_26,_2a),_2b);}}else{return [0,1,E([0,[0,_24],_25]),_26,E(_u),E(_u)];}},_2h=function(_2i,_2j,_2k){var _2l=E(_2i),_2m=_2l[2],_2n=E(_2k);if(!_2n[0]){var _2o=_2n[3],_2p=_2n[4],_2q=_2n[5],_2r=E(_2n[2]),_2s=E(_2l[1])[1],_2t=E(_2r[1])[1];if(_2s>=_2t){if(_2s!=_2t){return _1c(_2r,_2o,_2p,_23(_2s,_2m,_2j,_2q));}else{var _2u=E(_2m)[1],_2v=E(_2r[2])[1];return _2u>=_2v?_2u!=_2v?_1c(_2r,_2o,_2p,_1R(_2s,_2u,_2j,_2q)):[0,_2n[1],E(_2l),_2j,E(_2p),E(_2q)]:_x(_2r,_2o,_1R(_2s,_2u,_2j,_2p),_2q);}}else{return _x(_2r,_2o,_23(_2s,_2m,_2j,_2p),_2q);}}else{return [0,1,E(_2l),_2j,E(_u),E(_u)];}},_2w=function(_2x,_2y){while(1){var _2z=E(_2y);if(!_2z[0]){return E(_2x);}else{var _2A=E(_2z[1]),_2B=_2h(_2A[1],_2A[2],_2x);_2y=_2z[2];_2x=_2B;continue;}}},_2C=function(_2D,_2E){return [0,1,E(E(_2D)),_2E,E(_u),E(_u)];},_2F=function(_2G,_2H,_2I){var _2J=E(_2I);return _2J[0]==0?_1c(_2J[2],_2J[3],_2J[4],_2F(_2G,_2H,_2J[5])):_2C(_2G,_2H);},_2K=function(_2L,_2M,_2N){var _2O=E(_2N);return _2O[0]==0?_x(_2O[2],_2O[3],_2K(_2L,_2M,_2O[4]),_2O[5]):_2C(_2L,_2M);},_2P=function(_2Q,_2R,_2S,_2T,_2U,_2V,_2W,_2X){var _2Y=E(_2S);if(!_2Y[0]){var _2Z=_2Y[1],_30=_2Y[2],_31=_2Y[3],_32=_2Y[4],_33=_2Y[5];return (imul(3,_2Z)|0)>=_2T?(imul(3,_2T)|0)>=_2Z?[0,(_2Z+_2T|0)+1|0,E(E(_2Q)),_2R,E(_2Y),E([0,_2T,E(_2U),_2V,E(_2W),E(_2X)])]:_1c(_30,_31,_32,_2P(_2Q,_2R,_33,_2T,_2U,_2V,_2W,_2X)):_x(_2U,_2V,_34(_2Q,_2R,_2Z,_30,_31,_32,_33,_2W),_2X);}else{return _2K(_2Q,_2R,[0,_2T,E(_2U),_2V,E(_2W),E(_2X)]);}},_34=function(_35,_36,_37,_38,_39,_3a,_3b,_3c){var _3d=E(_3c);if(!_3d[0]){var _3e=_3d[1],_3f=_3d[2],_3g=_3d[3],_3h=_3d[4],_3i=_3d[5];return (imul(3,_37)|0)>=_3e?(imul(3,_3e)|0)>=_37?[0,(_37+_3e|0)+1|0,E(E(_35)),_36,E([0,_37,E(_38),_39,E(_3a),E(_3b)]),E(_3d)]:_1c(_38,_39,_3a,_2P(_35,_36,_3b,_3e,_3f,_3g,_3h,_3i)):_x(_3f,_3g,_34(_35,_36,_37,_38,_39,_3a,_3b,_3h),_3i);}else{return _2F(_35,_36,[0,_37,E(_38),_39,E(_3a),E(_3b)]);}},_3j=function(_3k,_3l,_3m,_3n){var _3o=E(_3m);if(!_3o[0]){var _3p=_3o[1],_3q=_3o[2],_3r=_3o[3],_3s=_3o[4],_3t=_3o[5],_3u=E(_3n);if(!_3u[0]){var _3v=_3u[1],_3w=_3u[2],_3x=_3u[3],_3y=_3u[4],_3z=_3u[5];return (imul(3,_3p)|0)>=_3v?(imul(3,_3v)|0)>=_3p?[0,(_3p+_3v|0)+1|0,E(E(_3k)),_3l,E(_3o),E(_3u)]:_1c(_3q,_3r,_3s,_2P(_3k,_3l,_3t,_3v,_3w,_3x,_3y,_3z)):_x(_3w,_3x,_34(_3k,_3l,_3p,_3q,_3r,_3s,_3t,_3y),_3z);}else{return _2F(_3k,_3l,_3o);}}else{return _2K(_3k,_3l,_3n);}},_3A=function(_3B,_3C,_3D,_3E,_3F){var _3G=E(_3B);if(_3G==1){var _3H=E(_3F);if(!_3H[0]){return [0,[0,1,E([0,[0,_3C],[0,_3D]]),_3E,E(_u),E(_u)],_o,_o];}else{var _3I=E(E(_3H[1])[1]),_3J=E(_3I[1])[1];return _3C>=_3J?_3C!=_3J?[0,[0,1,E([0,[0,_3C],[0,_3D]]),_3E,E(_u),E(_u)],_o,_3H]:_3D<E(_3I[2])[1]?[0,[0,1,E([0,[0,_3C],[0,_3D]]),_3E,E(_u),E(_u)],_3H,_o]:[0,[0,1,E([0,[0,_3C],[0,_3D]]),_3E,E(_u),E(_u)],_o,_3H]:[0,[0,1,E([0,[0,_3C],[0,_3D]]),_3E,E(_u),E(_u)],_3H,_o];}}else{var _3K=_3A(_3G>>1,_3C,_3D,_3E,_3F),_3L=_3K[1],_3M=_3K[3],_3N=E(_3K[2]);if(!_3N[0]){return [0,_3L,_o,_3M];}else{var _3O=E(_3N[1]),_3P=_3O[1],_3Q=_3O[2],_3R=E(_3N[2]);if(!_3R[0]){return [0,new T(function(){return _2F(_3P,_3Q,_3L);}),_o,_3M];}else{var _3S=_3R[2],_3T=E(_3R[1]),_3U=_3T[2],_3V=E(_3P),_3W=E(_3T[1]),_3X=_3W[2],_3Y=E(_3V[1])[1],_3Z=E(_3W[1])[1];if(_3Y>=_3Z){if(_3Y!=_3Z){return [0,_3L,_o,_3N];}else{var _40=E(_3X)[1];if(E(_3V[2])[1]<_40){var _41=_3A(_3G>>1,_3Z,_40,_3U,_3S);return [0,new T(function(){return _3j(_3V,_3Q,_3L,_41[1]);}),_41[2],_41[3]];}else{return [0,_3L,_o,_3N];}}}else{var _42=_43(_3G>>1,_3Z,_3X,_3U,_3S);return [0,new T(function(){return _3j(_3V,_3Q,_3L,_42[1]);}),_42[2],_42[3]];}}}}},_43=function(_44,_45,_46,_47,_48){var _49=E(_44);if(_49==1){var _4a=E(_48);if(!_4a[0]){return [0,[0,1,E([0,[0,_45],_46]),_47,E(_u),E(_u)],_o,_o];}else{var _4b=E(E(_4a[1])[1]),_4c=E(_4b[1])[1];if(_45>=_4c){if(_45!=_4c){return [0,[0,1,E([0,[0,_45],_46]),_47,E(_u),E(_u)],_o,_4a];}else{var _4d=E(_46);return _4d[1]<E(_4b[2])[1]?[0,[0,1,E([0,[0,_45],_4d]),_47,E(_u),E(_u)],_4a,_o]:[0,[0,1,E([0,[0,_45],_4d]),_47,E(_u),E(_u)],_o,_4a];}}else{return [0,[0,1,E([0,[0,_45],_46]),_47,E(_u),E(_u)],_4a,_o];}}}else{var _4e=_43(_49>>1,_45,_46,_47,_48),_4f=_4e[1],_4g=_4e[3],_4h=E(_4e[2]);if(!_4h[0]){return [0,_4f,_o,_4g];}else{var _4i=E(_4h[1]),_4j=_4i[1],_4k=_4i[2],_4l=E(_4h[2]);if(!_4l[0]){return [0,new T(function(){return _2F(_4j,_4k,_4f);}),_o,_4g];}else{var _4m=_4l[2],_4n=E(_4l[1]),_4o=_4n[2],_4p=E(_4j),_4q=E(_4n[1]),_4r=_4q[2],_4s=E(_4p[1])[1],_4t=E(_4q[1])[1];if(_4s>=_4t){if(_4s!=_4t){return [0,_4f,_o,_4h];}else{var _4u=E(_4r)[1];if(E(_4p[2])[1]<_4u){var _4v=_3A(_49>>1,_4t,_4u,_4o,_4m);return [0,new T(function(){return _3j(_4p,_4k,_4f,_4v[1]);}),_4v[2],_4v[3]];}else{return [0,_4f,_o,_4h];}}}else{var _4w=_43(_49>>1,_4t,_4r,_4o,_4m);return [0,new T(function(){return _3j(_4p,_4k,_4f,_4w[1]);}),_4w[2],_4w[3]];}}}}},_4x=function(_4y,_4z,_4A){var _4B=E(_4A);if(!_4B[0]){return E(_4z);}else{var _4C=E(_4B[1]),_4D=_4C[1],_4E=_4C[2],_4F=E(_4B[2]);if(!_4F[0]){return _2F(_4D,_4E,_4z);}else{var _4G=E(_4F[1]),_4H=E(_4D),_4I=E(_4G[1]),_4J=_4I[2],_4K=E(_4H[1])[1],_4L=E(_4I[1])[1],_4M=new T(function(){var _4N=_43(_4y,_4L,_4J,_4G[2],_4F[2]),_4O=_4N[1],_4P=E(_4N[3]);return _4P[0]==0?_4x(_4y<<1,_3j(_4H,_4E,_4z,_4O),_4N[2]):_2w(_3j(_4H,_4E,_4z,_4O),_4P);});return _4K>=_4L?_4K!=_4L?_2w(_4z,_4B):E(_4H[2])[1]<E(_4J)[1]?E(_4M):_2w(_4z,_4B):E(_4M);}}},_4Q=function(_4R){var _4S=E(_4R);if(!_4S[0]){return [1];}else{var _4T=E(_4S[1]),_4U=_4T[1],_4V=_4T[2],_4W=E(_4S[2]);if(!_4W[0]){return [0,1,E(E(_4U)),_4V,E(_u),E(_u)];}else{var _4X=E(_4U),_4Y=E(E(_4W[1])[1]),_4Z=E(_4X[1])[1],_50=E(_4Y[1])[1];return _4Z>=_50?_4Z!=_50?_2w([0,1,E(_4X),_4V,E(_u),E(_u)],_4W):E(_4X[2])[1]<E(_4Y[2])[1]?_4x(1,[0,1,E(_4X),_4V,E(_u),E(_u)],_4W):_2w([0,1,E(_4X),_4V,E(_u),E(_u)],_4W):_4x(1,[0,1,E(_4X),_4V,E(_u),E(_u)],_4W);}}},_51=function(_52,_53){var _54=E(_52),_55=E(_53);return _54[1]>_55[1]?E(_54):E(_55);},_56=function(_57,_58){var _59=E(_57),_5a=E(_58);return _59[1]>_5a[1]?E(_5a):E(_59);},_5b=function(_5c,_5d,_5e){while(1){var _5f=(function(_5g,_5h,_5i){var _5j=E(_5i);if(!_5j[0]){return [0,_5g,_5h];}else{var _5k=E(_5j[1])[1];_5c=new T(function(){var _5l=E(_5g),_5m=E(_5k);return [0,new T(function(){return _56(_5l[1],_5m[1]);}),new T(function(){return _56(_5l[2],_5m[2]);})];});_5d=new T(function(){var _5n=E(_5h),_5o=E(_5k);return [0,new T(function(){return _51(_5n[1],_5o[1]);}),new T(function(){return _51(_5n[2],_5o[2]);})];});_5e=_5j[2];return null;}})(_5c,_5d,_5e);if(_5f!=null){return _5f;}}},_5p=[0,0],_5q=[0,_5p,_5p],_5r=[0,_5p,_5p],_5s=function(_5t,_5u){while(1){var _5v=(function(_5w,_5x){var _5y=E(_5x);if(!_5y[0]){var _5z=E(_5y[2]),_5A=_5z[1],_5B=_5z[2],_5C=_5s(_5w,_5y[3]);_5t=[0,new T(function(){var _5D=E(_5C[1]);return [0,new T(function(){return _56(_5D[1],_5A);}),new T(function(){return _56(_5D[2],_5B);})];}),new T(function(){var _5E=E(_5C[2]);return [0,new T(function(){return _51(_5E[1],_5A);}),new T(function(){return _51(_5E[2],_5B);})];})];_5u=_5y[4];return null;}else{return E(_5w);}})(_5t,_5u);if(_5v!=null){return _5v;}}},_5F=[0,_5r,_5q],_5G=function(_5H,_5I){var _5J=new T(function(){var _5K=_5b(_5r,_5q,_5H);return [0,_5K[1],_5K[2]];}),_5L=new T(function(){return _5s(_5F,_5I);});return [0,new T(function(){var _5M=E(E(_5L)[1]),_5N=E(E(_5J)[1]);return [0,new T(function(){return _56(_5M[1],_5N[1]);}),new T(function(){return _56(_5M[2],_5N[2]);})];}),new T(function(){var _5O=E(E(_5L)[2]),_5P=E(E(_5J)[2]);return [0,new T(function(){return _51(_5O[1],_5P[1]);}),new T(function(){return _51(_5O[2],_5P[2]);})];})];},_5Q=function(_5R,_5S,_5T){while(1){var _5U=E(_5T);if(!_5U[0]){var _5V=_5U[3],_5W=_5U[4],_5X=E(_5U[2]),_5Y=E(_5X[1])[1];if(_5R>=_5Y){if(_5R!=_5Y){_5T=_5W;continue;}else{var _5Z=E(_5X[2])[1];if(_5S>=_5Z){if(_5S!=_5Z){_5T=_5W;continue;}else{return true;}}else{_5T=_5V;continue;}}}else{_5T=_5V;continue;}}else{return false;}}},_60=function(_61,_62,_63){while(1){var _64=E(_63);if(!_64[0]){var _65=_64[3],_66=_64[4],_67=E(_64[2]),_68=E(_67[1])[1];if(_61>=_68){if(_61!=_68){_63=_66;continue;}else{var _69=E(_62)[1],_6a=E(_67[2])[1];return _69>=_6a?_69!=_6a?_5Q(_61,_69,_66):true:_5Q(_61,_69,_65);}}else{_63=_65;continue;}}else{return false;}}},_6b=function(_6c,_6d,_6e){var _6f=E(_6e);if(!_6f[0]){var _6g=_6f[3],_6h=_6f[4],_6i=E(_6f[2]),_6j=E(_6c)[1],_6k=E(_6i[1])[1];if(_6j>=_6k){if(_6j!=_6k){return _60(_6j,_6d,_6h);}else{var _6l=E(_6d)[1],_6m=E(_6i[2])[1];return _6l>=_6m?_6l!=_6m?_5Q(_6j,_6l,_6h):true:_5Q(_6j,_6l,_6g);}}else{return _60(_6j,_6d,_6g);}}else{return false;}},_6n=function(_6o,_6p){if(_6o<=_6p){var _6q=function(_6r){return [1,[0,_6r],new T(function(){return _6r!=_6p?_6q(_6r+1|0):[0];})];};return _6q(_6o);}else{return [0];}},_6s=unCStr("Maybe.fromJust: Nothing"),_6t=new T(function(){return err(_6s);}),_6u=function(_6v){return E(E(_6v)[2]);},_6w=function(_6x,_6y,_6z){var _6A=new T(function(){return _6u(_6x);});return (function(_6B,_6C){while(1){var _6D=E(_6B),_6E=E(_6C);if(!_6E[0]){switch(A(_6A,[_6D,_6E[2]])){case 0:_6B=_6D;_6C=_6E[4];continue;case 1:return [1,_6E[3]];default:_6B=_6D;_6C=_6E[5];continue;}}else{return [0];}}})(_6y,_6z);},_6F=function(_6G,_6H){var _6I=E(_6H);return _6I[0]==0?[0]:[1,new T(function(){return A(_6G,[_6I[1]]);}),new T(function(){return _6F(_6G,_6I[2]);})];},_6J=function(_6K,_6L,_6M){var _6N=new T(function(){return _6u(_6K);});return (function(_6O,_6P){while(1){var _6Q=E(_6O),_6R=E(_6P);if(!_6R[0]){switch(A(_6N,[_6Q,_6R[2]])){case 0:_6O=_6Q;_6P=_6R[4];continue;case 1:return true;default:_6O=_6Q;_6P=_6R[5];continue;}}else{return false;}}})(_6L,_6M);},_6S=[0,32],_6T=function(_6U){var _6V=E(_6U);return [0,_6V[1],_6V[2]];},_6W=[0,10],_6X=function(_6Y){var _6Z=E(_6Y);return _6Z[0]==0?[0]:_9(_6Z[1],[1,_6W,new T(function(){return _6X(_6Z[2]);})]);},_70=new T(function(){return _6X(_o);}),_71=[0,8592],_72=[0,8595],_73=[0,8594],_74=[0,8593],_75=[0,79],_76=function(_77){return E(E(_77)[3]);},_78=function(_79,_7a,_7b,_7c,_7d,_7e){switch(A(_79,[_7b,_7d])){case 0:return true;case 1:return A(_76,[_7a,_7c,_7e]);default:return false;}},_7f=function(_7g,_7h,_7i,_7j,_7k){var _7l=E(_7j),_7m=E(_7k);return _78(E(_7h)[2],_7i,_7l[1],_7l[2],_7m[1],_7m[2]);},_7n=function(_7o){return E(E(_7o)[6]);},_7p=function(_7q,_7r,_7s,_7t,_7u,_7v){switch(A(_7q,[_7s,_7u])){case 0:return true;case 1:return A(_7n,[_7r,_7t,_7v]);default:return false;}},_7w=function(_7x,_7y,_7z,_7A,_7B){var _7C=E(_7A),_7D=E(_7B);return _7p(E(_7y)[2],_7z,_7C[1],_7C[2],_7D[1],_7D[2]);},_7E=function(_7F){return E(E(_7F)[5]);},_7G=function(_7H,_7I,_7J,_7K,_7L,_7M){switch(A(_7H,[_7J,_7L])){case 0:return false;case 1:return A(_7E,[_7I,_7K,_7M]);default:return true;}},_7N=function(_7O,_7P,_7Q,_7R,_7S){var _7T=E(_7R),_7U=E(_7S);return _7G(E(_7P)[2],_7Q,_7T[1],_7T[2],_7U[1],_7U[2]);},_7V=function(_7W){return E(E(_7W)[4]);},_7X=function(_7Y,_7Z,_80,_81,_82,_83){switch(A(_7Y,[_80,_82])){case 0:return false;case 1:return A(_7V,[_7Z,_81,_83]);default:return true;}},_84=function(_85,_86,_87,_88,_89){var _8a=E(_88),_8b=E(_89);return _7X(E(_86)[2],_87,_8a[1],_8a[2],_8b[1],_8b[2]);},_8c=function(_8d,_8e,_8f,_8g,_8h,_8i){switch(A(_8d,[_8f,_8h])){case 0:return 0;case 1:return A(_6u,[_8e,_8g,_8i]);default:return 2;}},_8j=function(_8k,_8l,_8m,_8n,_8o){var _8p=E(_8n),_8q=E(_8o);return _8c(E(_8l)[2],_8m,_8p[1],_8p[2],_8q[1],_8q[2]);},_8r=function(_8s,_8t,_8u,_8v,_8w){var _8x=E(_8v),_8y=_8x[1],_8z=_8x[2],_8A=E(_8w),_8B=_8A[1],_8C=_8A[2];switch(A(E(_8t)[2],[_8y,_8B])){case 0:return [0,_8B,_8C];case 1:return !A(_7n,[_8u,_8z,_8C])?[0,_8y,_8z]:[0,_8B,_8C];default:return [0,_8y,_8z];}},_8D=function(_8E,_8F,_8G,_8H,_8I){var _8J=E(_8H),_8K=_8J[1],_8L=_8J[2],_8M=E(_8I),_8N=_8M[1],_8O=_8M[2];switch(A(E(_8F)[2],[_8K,_8N])){case 0:return [0,_8K,_8L];case 1:return !A(_7n,[_8G,_8L,_8O])?[0,_8N,_8O]:[0,_8K,_8L];default:return [0,_8N,_8O];}},_8P=function(_8Q,_8R,_8S){return [0,_8Q,function(_8T,_8U){return _8j(_8Q,_8R,_8S,_8T,_8U);},function(_8T,_8U){return _7f(_8Q,_8R,_8S,_8T,_8U);},function(_8T,_8U){return _84(_8Q,_8R,_8S,_8T,_8U);},function(_8T,_8U){return _7N(_8Q,_8R,_8S,_8T,_8U);},function(_8T,_8U){return _7w(_8Q,_8R,_8S,_8T,_8U);},function(_8T,_8U){return _8r(_8Q,_8R,_8S,_8T,_8U);},function(_8T,_8U){return _8D(_8Q,_8R,_8S,_8T,_8U);}];},_8V=function(_8W,_8X){return E(_8W)[1]==E(_8X)[1];},_8Y=function(_8Z,_90){return E(_8Z)[1]!=E(_90)[1];},_91=[0,_8V,_8Y],_92=function(_93,_94){return _93>=_94?_93!=_94?2:1:0;},_95=function(_96,_97){return _92(E(_96)[1],E(_97)[1]);},_98=function(_99,_9a){return E(_99)[1]>=E(_9a)[1];},_9b=function(_9c,_9d){return E(_9c)[1]>E(_9d)[1];},_9e=function(_9f,_9g){return E(_9f)[1]<=E(_9g)[1];},_9h=function(_9i,_9j){return E(_9i)[1]<E(_9j)[1];},_9k=[0,_91,_95,_9h,_98,_9b,_9e,_51,_56],_9l=function(_9m){return E(E(_9m)[1]);},_9n=function(_9o,_9p,_9q,_9r,_9s,_9t){return !A(_9o,[_9q,_9s])?true:!A(_9l,[_9p,_9r,_9t])?true:false;},_9u=function(_9v,_9w,_9x,_9y){var _9z=E(_9x),_9A=E(_9y);return _9n(E(_9v)[1],_9w,_9z[1],_9z[2],_9A[1],_9A[2]);},_9B=function(_9C,_9D,_9E,_9F,_9G,_9H){return !A(_9C,[_9E,_9G])?false:A(_9l,[_9D,_9F,_9H]);},_9I=function(_9J,_9K,_9L,_9M){var _9N=E(_9L),_9O=E(_9M);return _9B(E(_9J)[1],_9K,_9N[1],_9N[2],_9O[1],_9O[2]);},_9P=function(_9Q,_9R){return [0,function(_8T,_8U){return _9I(_9Q,_9R,_8T,_8U);},function(_8T,_8U){return _9u(_9Q,_9R,_8T,_8U);}];},_9S=new T(function(){return _9P(_91,_91);}),_9T=new T(function(){return _8P(_9S,_9k,_9k);}),_9U=function(_9V,_9W){var _9X=_5G(_9V,_9W),_9Y=E(_9X[1]),_9Z=E(_9X[2]),_a0=E(_9Y[2])[1],_a1=E(_9Z[2])[1];if(_a0<=_a1){var _a2=new T(function(){return _4Q(_6F(_6T,_9V));}),_a3=new T(function(){return _6n(E(_9Y[1])[1],E(_9Z[1])[1]);}),_a4=function(_a5){return [1,new T(function(){var _a6=[0,_a5],_a7=function(_a8){var _a9=E(_a8);if(!_a9[0]){return [0];}else{var _aa=_a9[1];return [1,new T(function(){var _ab=[0,_aa,_a6];if(!_6J(_9T,_ab,_a2)){return !_6b(_aa,_a6,_9W)?E(_6S):E(_75);}else{var _ac=_6w(_9T,_ab,_a2);if(!_ac[0]){return E(_6t);}else{switch(E(_ac[1])){case 0:return E(_74);case 1:return E(_73);case 2:return E(_72);default:return E(_71);}}}}),new T(function(){return _a7(_a9[2]);})];}};return _a7(_a3);}),new T(function(){return _a5!=_a1?_a4(_a5+1|0):[0];})];};return _6X(_a4(_a0));}else{return E(_70);}},_ad=function(_ae){var _af=E(_ae);return _9U(_af[1],_af[2]);},_ag=function(_ah){var _ai=E(E(_ah)[1]);return _ai==(-2147483648)?E(_1):[0,_ai-1|0];},_aj=unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"),_ak=new T(function(){return err(_aj);}),_al=function(_am){var _an=E(E(_am)[1]);return _an==2147483647?E(_ak):[0,_an+1|0];},_ao=2,_ap=3,_aq=1,_ar=0,_as=[1],_at=unCStr("Failure in Data.Map.balanceL"),_au=new T(function(){return err(_at);}),_av=function(_aw,_ax,_ay){var _az=E(_ay);if(!_az[0]){var _aA=_az[1],_aB=E(_ax);if(!_aB[0]){var _aC=_aB[1],_aD=_aB[2];if(_aC<=(imul(3,_aA)|0)){return [0,(1+_aC|0)+_aA|0,E(E(_aw)),E(_aB),E(_az)];}else{var _aE=E(_aB[3]);if(!_aE[0]){var _aF=_aE[1],_aG=E(_aB[4]);if(!_aG[0]){var _aH=_aG[1],_aI=_aG[2],_aJ=_aG[3];if(_aH>=(imul(2,_aF)|0)){var _aK=function(_aL){var _aM=E(_aG[4]);return _aM[0]==0?[0,(1+_aC|0)+_aA|0,E(_aI),E([0,(1+_aF|0)+_aL|0,E(_aD),E(_aE),E(_aJ)]),E([0,(1+_aA|0)+_aM[1]|0,E(E(_aw)),E(_aM),E(_az)])]:[0,(1+_aC|0)+_aA|0,E(_aI),E([0,(1+_aF|0)+_aL|0,E(_aD),E(_aE),E(_aJ)]),E([0,1+_aA|0,E(E(_aw)),E(_as),E(_az)])];},_aN=E(_aJ);return _aN[0]==0?_aK(_aN[1]):_aK(0);}else{return [0,(1+_aC|0)+_aA|0,E(_aD),E(_aE),E([0,(1+_aA|0)+_aH|0,E(E(_aw)),E(_aG),E(_az)])];}}else{return E(_au);}}else{return E(_au);}}}else{return [0,1+_aA|0,E(E(_aw)),E(_as),E(_az)];}}else{var _aO=E(_ax);if(!_aO[0]){var _aP=_aO[1],_aQ=_aO[2],_aR=_aO[4],_aS=E(_aO[3]);if(!_aS[0]){var _aT=_aS[1],_aU=E(_aR);if(!_aU[0]){var _aV=_aU[1],_aW=_aU[2],_aX=_aU[3];if(_aV>=(imul(2,_aT)|0)){var _aY=function(_aZ){var _b0=E(_aU[4]);return _b0[0]==0?[0,1+_aP|0,E(_aW),E([0,(1+_aT|0)+_aZ|0,E(_aQ),E(_aS),E(_aX)]),E([0,1+_b0[1]|0,E(E(_aw)),E(_b0),E(_as)])]:[0,1+_aP|0,E(_aW),E([0,(1+_aT|0)+_aZ|0,E(_aQ),E(_aS),E(_aX)]),E([0,1,E(E(_aw)),E(_as),E(_as)])];},_b1=E(_aX);return _b1[0]==0?_aY(_b1[1]):_aY(0);}else{return [0,1+_aP|0,E(_aQ),E(_aS),E([0,1+_aV|0,E(E(_aw)),E(_aU),E(_as)])];}}else{return [0,3,E(_aQ),E(_aS),E([0,1,E(E(_aw)),E(_as),E(_as)])];}}else{var _b2=E(_aR);return _b2[0]==0?[0,3,E(_b2[2]),E([0,1,E(_aQ),E(_as),E(_as)]),E([0,1,E(E(_aw)),E(_as),E(_as)])]:[0,2,E(E(_aw)),E(_aO),E(_as)];}}else{return [0,1,E(E(_aw)),E(_as),E(_as)];}}},_b3=unCStr("Failure in Data.Map.balanceR"),_b4=new T(function(){return err(_b3);}),_b5=function(_b6,_b7,_b8){var _b9=E(_b7);if(!_b9[0]){var _ba=_b9[1],_bb=E(_b8);if(!_bb[0]){var _bc=_bb[1],_bd=_bb[2];if(_bc<=(imul(3,_ba)|0)){return [0,(1+_ba|0)+_bc|0,E(E(_b6)),E(_b9),E(_bb)];}else{var _be=E(_bb[3]);if(!_be[0]){var _bf=_be[1],_bg=_be[2],_bh=_be[3],_bi=E(_bb[4]);if(!_bi[0]){var _bj=_bi[1];if(_bf>=(imul(2,_bj)|0)){var _bk=function(_bl){var _bm=E(_b6),_bn=E(_be[4]);return _bn[0]==0?[0,(1+_ba|0)+_bc|0,E(_bg),E([0,(1+_ba|0)+_bl|0,E(_bm),E(_b9),E(_bh)]),E([0,(1+_bj|0)+_bn[1]|0,E(_bd),E(_bn),E(_bi)])]:[0,(1+_ba|0)+_bc|0,E(_bg),E([0,(1+_ba|0)+_bl|0,E(_bm),E(_b9),E(_bh)]),E([0,1+_bj|0,E(_bd),E(_as),E(_bi)])];},_bo=E(_bh);return _bo[0]==0?_bk(_bo[1]):_bk(0);}else{return [0,(1+_ba|0)+_bc|0,E(_bd),E([0,(1+_ba|0)+_bf|0,E(E(_b6)),E(_b9),E(_be)]),E(_bi)];}}else{return E(_b4);}}else{return E(_b4);}}}else{return [0,1+_ba|0,E(E(_b6)),E(_b9),E(_as)];}}else{var _bp=E(_b8);if(!_bp[0]){var _bq=_bp[1],_br=_bp[2],_bs=_bp[4],_bt=E(_bp[3]);if(!_bt[0]){var _bu=_bt[1],_bv=_bt[2],_bw=_bt[3],_bx=E(_bs);if(!_bx[0]){var _by=_bx[1];if(_bu>=(imul(2,_by)|0)){var _bz=function(_bA){var _bB=E(_b6),_bC=E(_bt[4]);return _bC[0]==0?[0,1+_bq|0,E(_bv),E([0,1+_bA|0,E(_bB),E(_as),E(_bw)]),E([0,(1+_by|0)+_bC[1]|0,E(_br),E(_bC),E(_bx)])]:[0,1+_bq|0,E(_bv),E([0,1+_bA|0,E(_bB),E(_as),E(_bw)]),E([0,1+_by|0,E(_br),E(_as),E(_bx)])];},_bD=E(_bw);return _bD[0]==0?_bz(_bD[1]):_bz(0);}else{return [0,1+_bq|0,E(_br),E([0,1+_bu|0,E(E(_b6)),E(_as),E(_bt)]),E(_bx)];}}else{return [0,3,E(_bv),E([0,1,E(E(_b6)),E(_as),E(_as)]),E([0,1,E(_br),E(_as),E(_as)])];}}else{var _bE=E(_bs);return _bE[0]==0?[0,3,E(_br),E([0,1,E(E(_b6)),E(_as),E(_as)]),E(_bE)]:[0,2,E(E(_b6)),E(_as),E(_bp)];}}else{return [0,1,E(E(_b6)),E(_as),E(_as)];}}},_bF=function(_bG,_bH,_bI){var _bJ=E(_bI);if(!_bJ[0]){var _bK=_bJ[3],_bL=_bJ[4],_bM=E(_bJ[2]),_bN=E(_bM[1])[1];if(_bG>=_bN){if(_bG!=_bN){return _b5(_bM,_bK,_bF(_bG,_bH,_bL));}else{var _bO=E(_bM[2])[1];return _bH>=_bO?_bH!=_bO?_b5(_bM,_bK,_bF(_bG,_bH,_bL)):[0,_bJ[1],E([0,[0,_bG],[0,_bH]]),E(_bK),E(_bL)]:_av(_bM,_bF(_bG,_bH,_bK),_bL);}}else{return _av(_bM,_bF(_bG,_bH,_bK),_bL);}}else{return [0,1,E([0,[0,_bG],[0,_bH]]),E(_as),E(_as)];}},_bP=function(_bQ,_bR,_bS){var _bT=E(_bS);if(!_bT[0]){var _bU=_bT[3],_bV=_bT[4],_bW=E(_bT[2]),_bX=E(_bW[1])[1];if(_bQ>=_bX){if(_bQ!=_bX){return _b5(_bW,_bU,_bP(_bQ,_bR,_bV));}else{var _bY=E(_bR),_bZ=_bY[1],_c0=E(_bW[2])[1];return _bZ>=_c0?_bZ!=_c0?_b5(_bW,_bU,_bF(_bQ,_bZ,_bV)):[0,_bT[1],E([0,[0,_bQ],_bY]),E(_bU),E(_bV)]:_av(_bW,_bF(_bQ,_bZ,_bU),_bV);}}else{return _av(_bW,_bP(_bQ,_bR,_bU),_bV);}}else{return [0,1,E([0,[0,_bQ],_bR]),E(_as),E(_as)];}},_c1=function(_c2,_c3){var _c4=E(_c2),_c5=_c4[2],_c6=E(_c3);if(!_c6[0]){var _c7=_c6[3],_c8=_c6[4],_c9=E(_c6[2]),_ca=E(_c4[1])[1],_cb=E(_c9[1])[1];if(_ca>=_cb){if(_ca!=_cb){return _b5(_c9,_c7,_bP(_ca,_c5,_c8));}else{var _cc=E(_c5)[1],_cd=E(_c9[2])[1];return _cc>=_cd?_cc!=_cd?_b5(_c9,_c7,_bF(_ca,_cc,_c8)):[0,_c6[1],E(_c4),E(_c7),E(_c8)]:_av(_c9,_bF(_ca,_cc,_c7),_c8);}}else{return _av(_c9,_bP(_ca,_c5,_c7),_c8);}}else{return [0,1,E(_c4),E(_as),E(_as)];}},_ce=function(_cf,_cg,_ch,_ci){var _cj=E(_ci);if(!_cj[0]){var _ck=new T(function(){var _cl=_ce(_cj[1],_cj[2],_cj[3],_cj[4]);return [0,_cl[1],_cl[2]];});return [0,new T(function(){return E(E(_ck)[1]);}),new T(function(){return _av(_cg,_ch,E(_ck)[2]);})];}else{return [0,_cg,_ch];}},_cm=function(_cn,_co,_cp,_cq){var _cr=E(_cp);if(!_cr[0]){var _cs=new T(function(){var _ct=_cm(_cr[1],_cr[2],_cr[3],_cr[4]);return [0,_ct[1],_ct[2]];});return [0,new T(function(){return E(E(_cs)[1]);}),new T(function(){return _b5(_co,E(_cs)[2],_cq);})];}else{return [0,_co,_cq];}},_cu=function(_cv,_cw){var _cx=E(_cv);if(!_cx[0]){var _cy=_cx[1],_cz=E(_cw);if(!_cz[0]){var _cA=_cz[1];if(_cy<=_cA){var _cB=_cm(_cA,_cz[2],_cz[3],_cz[4]);return _av(_cB[1],_cx,_cB[2]);}else{var _cC=_ce(_cy,_cx[2],_cx[3],_cx[4]);return _b5(_cC[1],_cC[2],_cz);}}else{return E(_cx);}}else{return E(_cw);}},_cD=function(_cE,_cF,_cG){var _cH=E(_cG);if(!_cH[0]){var _cI=_cH[3],_cJ=_cH[4],_cK=E(_cH[2]),_cL=E(_cK[1])[1];if(_cE>=_cL){if(_cE!=_cL){return _av(_cK,_cI,_cD(_cE,_cF,_cJ));}else{var _cM=E(_cK[2])[1];return _cF>=_cM?_cF!=_cM?_av(_cK,_cI,_cD(_cE,_cF,_cJ)):_cu(_cI,_cJ):_b5(_cK,_cD(_cE,_cF,_cI),_cJ);}}else{return _b5(_cK,_cD(_cE,_cF,_cI),_cJ);}}else{return [1];}},_cN=function(_cO,_cP,_cQ){var _cR=E(_cQ);if(!_cR[0]){var _cS=_cR[3],_cT=_cR[4],_cU=E(_cR[2]),_cV=E(_cU[1])[1];if(_cO>=_cV){if(_cO!=_cV){return _av(_cU,_cS,_cN(_cO,_cP,_cT));}else{var _cW=E(_cP)[1],_cX=E(_cU[2])[1];return _cW>=_cX?_cW!=_cX?_av(_cU,_cS,_cD(_cO,_cW,_cT)):_cu(_cS,_cT):_b5(_cU,_cD(_cO,_cW,_cS),_cT);}}else{return _b5(_cU,_cN(_cO,_cP,_cS),_cT);}}else{return [1];}},_cY=function(_cZ,_d0,_d1){var _d2=E(_d1);if(!_d2[0]){var _d3=_d2[3],_d4=_d2[4],_d5=E(_d2[2]),_d6=E(_cZ)[1],_d7=E(_d5[1])[1];if(_d6>=_d7){if(_d6!=_d7){return _av(_d5,_d3,_cN(_d6,_d0,_d4));}else{var _d8=E(_d0)[1],_d9=E(_d5[2])[1];return _d8>=_d9?_d8!=_d9?_av(_d5,_d3,_cD(_d6,_d8,_d4)):_cu(_d3,_d4):_b5(_d5,_cD(_d6,_d8,_d3),_d4);}}else{return _b5(_d5,_cN(_d6,_d0,_d3),_d4);}}else{return [1];}},_da=function(_db,_dc,_dd){return !_6b(_db,_dc,_dd)?_c1([0,_db,_dc],_dd):_cY(_db,_dc,_dd);},_de=function(_df,_dg){var _dh=E(_df);return _da(_dh[1],_dh[2],_dg);},_di=function(_dj,_dk){while(1){var _dl=E(_dk);if(!_dl[0]){return E(_dj);}else{var _dm=_de(E(_dl[1])[1],_dj);_dk=_dl[2];_dj=_dm;continue;}}},_dn=function(_do,_dp){return [0,new T(function(){return _6F(function(_dq){var _dr=E(_dq),_ds=_dr[2],_dt=E(_dr[1]),_du=_dt[1],_dv=_dt[2];if(!_6b(_du,_dv,_dp)){switch(E(_ds)){case 0:return [0,[0,new T(function(){return _al(_du);}),_dv],_aq];case 1:return [0,[0,_du,new T(function(){return _al(_dv);})],_ao];case 2:return [0,[0,new T(function(){return _ag(_du);}),_dv],_ap];default:return [0,[0,_du,new T(function(){return _ag(_dv);})],_ar];}}else{switch(E(_ds)){case 0:return [0,[0,new T(function(){return _ag(_du);}),_dv],_ap];case 1:return [0,[0,_du,new T(function(){return _ag(_dv);})],_ar];case 2:return [0,[0,new T(function(){return _al(_du);}),_dv],_aq];default:return [0,[0,_du,new T(function(){return _al(_dv);})],_ao];}}},_do);}),new T(function(){return _di(_dp,_do);})];},_dw=function(_dx){var _dy=E(_dx),_dz=_dn(_dy[1],_dy[2]);return [0,_dz[1],_dz[2]];},_dA=unCStr(" could be found!"),_dB=function(_dC){return err(unAppCStr("No element with ID ",new T(function(){return _9(_dC,_dA);})));},_dD=function(_dE,_dF,_dG,_){var _dH=function(_dI,_dJ,_){var _dK=E(_r),_dL=jsFind(toJSStr(_dK)),_dM=E(_dL);if(!_dM[0]){return _dB(_dK);}else{var _dN=A(_3,[_p,_dM[1],_t,new T(function(){return _ad(_dJ);}),_]),_dO=E(_s),_dP=jsFind(toJSStr(_dO)),_dQ=E(_dP);return _dQ[0]==0?_dB(_dO):A(_3,[_p,_dQ[1],_t,new T(function(){return _j(0,E(_dG)[1]-_dI|0,_o);}),_]);}},_dR=function(_dS,_dT,_){var _dU=E(_dT);if(!_dU){var _dV=jsSetTimeout(_dE,function(_){return _dH(0,_dS,_);});return _2;}else{var _dW=_dH(_dU,_dS,_),_dX=jsSetTimeout(_dE,function(_){var _dY=E(_dT);return _dY==(-2147483648)?E(_1):_dR(new T(function(){return _dw(_dS);}),_dY-1|0,_);});return _2;}},_dZ=jsSetTimeout(_dE,function(_){return _dR(_dF,E(_dG)[1],_);});return _2;},_e0=[0,4000],_e1=function(_e2,_e3){while(1){var _e4=E(_e3);if(!_e4[0]){return E(_e2);}else{var _e5=_c1(_e4[1],_e2);_e3=_e4[2];_e2=_e5;continue;}}},_e6=function(_e7){return [0,1,E(E(_e7)),E(_as),E(_as)];},_e8=function(_e9,_ea){var _eb=E(_ea);return _eb[0]==0?_b5(_eb[2],_eb[3],_e8(_e9,_eb[4])):_e6(_e9);},_ec=function(_ed,_ee){var _ef=E(_ee);return _ef[0]==0?_av(_ef[2],_ec(_ed,_ef[3]),_ef[4]):_e6(_ed);},_eg=function(_eh,_ei,_ej,_ek,_el,_em){var _en=E(_ei);if(!_en[0]){var _eo=_en[1],_ep=_en[2],_eq=_en[3],_er=_en[4];return (imul(3,_eo)|0)>=_ej?(imul(3,_ej)|0)>=_eo?[0,(_eo+_ej|0)+1|0,E(E(_eh)),E(_en),E([0,_ej,E(_ek),E(_el),E(_em)])]:_b5(_ep,_eq,_eg(_eh,_er,_ej,_ek,_el,_em)):_av(_ek,_es(_eh,_eo,_ep,_eq,_er,_el),_em);}else{return _ec(_eh,[0,_ej,E(_ek),E(_el),E(_em)]);}},_es=function(_et,_eu,_ev,_ew,_ex,_ey){var _ez=E(_ey);if(!_ez[0]){var _eA=_ez[1],_eB=_ez[2],_eC=_ez[3],_eD=_ez[4];return (imul(3,_eu)|0)>=_eA?(imul(3,_eA)|0)>=_eu?[0,(_eu+_eA|0)+1|0,E(E(_et)),E([0,_eu,E(_ev),E(_ew),E(_ex)]),E(_ez)]:_b5(_ev,_ew,_eg(_et,_ex,_eA,_eB,_eC,_eD)):_av(_eB,_es(_et,_eu,_ev,_ew,_ex,_eC),_eD);}else{return _e8(_et,[0,_eu,E(_ev),E(_ew),E(_ex)]);}},_eE=function(_eF,_eG,_eH){var _eI=E(_eG);if(!_eI[0]){var _eJ=_eI[1],_eK=_eI[2],_eL=_eI[3],_eM=_eI[4],_eN=E(_eH);if(!_eN[0]){var _eO=_eN[1],_eP=_eN[2],_eQ=_eN[3],_eR=_eN[4];return (imul(3,_eJ)|0)>=_eO?(imul(3,_eO)|0)>=_eJ?[0,(_eJ+_eO|0)+1|0,E(E(_eF)),E(_eI),E(_eN)]:_b5(_eK,_eL,_eg(_eF,_eM,_eO,_eP,_eQ,_eR)):_av(_eP,_es(_eF,_eJ,_eK,_eL,_eM,_eQ),_eR);}else{return _e8(_eF,_eI);}}else{return _ec(_eF,_eH);}},_eS=function(_eT,_eU,_eV,_eW){var _eX=E(_eT);if(_eX==1){var _eY=E(_eW);if(!_eY[0]){return [0,[0,1,E([0,[0,_eU],[0,_eV]]),E(_as),E(_as)],_o,_o];}else{var _eZ=E(_eY[1]),_f0=E(_eZ[1])[1];return _eU>=_f0?_eU!=_f0?[0,[0,1,E([0,[0,_eU],[0,_eV]]),E(_as),E(_as)],_o,_eY]:_eV<E(_eZ[2])[1]?[0,[0,1,E([0,[0,_eU],[0,_eV]]),E(_as),E(_as)],_eY,_o]:[0,[0,1,E([0,[0,_eU],[0,_eV]]),E(_as),E(_as)],_o,_eY]:[0,[0,1,E([0,[0,_eU],[0,_eV]]),E(_as),E(_as)],_eY,_o];}}else{var _f1=_eS(_eX>>1,_eU,_eV,_eW),_f2=_f1[1],_f3=_f1[3],_f4=E(_f1[2]);if(!_f4[0]){return [0,_f2,_o,_f3];}else{var _f5=_f4[1],_f6=E(_f4[2]);if(!_f6[0]){return [0,new T(function(){return _e8(_f5,_f2);}),_o,_f3];}else{var _f7=_f6[2],_f8=E(_f5),_f9=E(_f6[1]),_fa=_f9[2],_fb=E(_f8[1])[1],_fc=E(_f9[1])[1];if(_fb>=_fc){if(_fb!=_fc){return [0,_f2,_o,_f4];}else{var _fd=E(_fa)[1];if(E(_f8[2])[1]<_fd){var _fe=_eS(_eX>>1,_fc,_fd,_f7);return [0,new T(function(){return _eE(_f8,_f2,_fe[1]);}),_fe[2],_fe[3]];}else{return [0,_f2,_o,_f4];}}}else{var _ff=_fg(_eX>>1,_fc,_fa,_f7);return [0,new T(function(){return _eE(_f8,_f2,_ff[1]);}),_ff[2],_ff[3]];}}}}},_fg=function(_fh,_fi,_fj,_fk){var _fl=E(_fh);if(_fl==1){var _fm=E(_fk);if(!_fm[0]){return [0,[0,1,E([0,[0,_fi],_fj]),E(_as),E(_as)],_o,_o];}else{var _fn=E(_fm[1]),_fo=E(_fn[1])[1];if(_fi>=_fo){if(_fi!=_fo){return [0,[0,1,E([0,[0,_fi],_fj]),E(_as),E(_as)],_o,_fm];}else{var _fp=E(_fj);return _fp[1]<E(_fn[2])[1]?[0,[0,1,E([0,[0,_fi],_fp]),E(_as),E(_as)],_fm,_o]:[0,[0,1,E([0,[0,_fi],_fp]),E(_as),E(_as)],_o,_fm];}}else{return [0,[0,1,E([0,[0,_fi],_fj]),E(_as),E(_as)],_fm,_o];}}}else{var _fq=_fg(_fl>>1,_fi,_fj,_fk),_fr=_fq[1],_fs=_fq[3],_ft=E(_fq[2]);if(!_ft[0]){return [0,_fr,_o,_fs];}else{var _fu=_ft[1],_fv=E(_ft[2]);if(!_fv[0]){return [0,new T(function(){return _e8(_fu,_fr);}),_o,_fs];}else{var _fw=_fv[2],_fx=E(_fu),_fy=E(_fv[1]),_fz=_fy[2],_fA=E(_fx[1])[1],_fB=E(_fy[1])[1];if(_fA>=_fB){if(_fA!=_fB){return [0,_fr,_o,_ft];}else{var _fC=E(_fz)[1];if(E(_fx[2])[1]<_fC){var _fD=_eS(_fl>>1,_fB,_fC,_fw);return [0,new T(function(){return _eE(_fx,_fr,_fD[1]);}),_fD[2],_fD[3]];}else{return [0,_fr,_o,_ft];}}}else{var _fE=_fg(_fl>>1,_fB,_fz,_fw);return [0,new T(function(){return _eE(_fx,_fr,_fE[1]);}),_fE[2],_fE[3]];}}}}},_fF=function(_fG,_fH,_fI){var _fJ=E(_fI);if(!_fJ[0]){return E(_fH);}else{var _fK=_fJ[1],_fL=E(_fJ[2]);if(!_fL[0]){return _e8(_fK,_fH);}else{var _fM=E(_fK),_fN=E(_fL[1]),_fO=_fN[2],_fP=E(_fM[1])[1],_fQ=E(_fN[1])[1],_fR=new T(function(){var _fS=_fg(_fG,_fQ,_fO,_fL[2]),_fT=_fS[1],_fU=E(_fS[3]);return _fU[0]==0?_fF(_fG<<1,_eE(_fM,_fH,_fT),_fS[2]):_e1(_eE(_fM,_fH,_fT),_fU);});return _fP>=_fQ?_fP!=_fQ?_e1(_fH,_fJ):E(_fM[2])[1]<E(_fO)[1]?E(_fR):_e1(_fH,_fJ):E(_fR);}}},_fV=function(_fW){var _fX=E(_fW);if(!_fX[0]){return [1];}else{var _fY=_fX[1],_fZ=E(_fX[2]);if(!_fZ[0]){return [0,1,E(E(_fY)),E(_as),E(_as)];}else{var _g0=E(_fY),_g1=E(_fZ[1]),_g2=E(_g0[1])[1],_g3=E(_g1[1])[1];return _g2>=_g3?_g2!=_g3?_e1([0,1,E(_g0),E(_as),E(_as)],_fZ):E(_g0[2])[1]<E(_g1[2])[1]?_fF(1,[0,1,E(_g0),E(_as),E(_as)],_fZ):_e1([0,1,E(_g0),E(_as),E(_as)],_fZ):_fF(1,[0,1,E(_g0),E(_as),E(_as)],_fZ);}}},_g4=new T(function(){return _fV(_o);}),_g5=[0,4],_g6=[0,_g5,_g5],_g7=[0,_g6,_ar],_g8=[0,12],_g9=[0,15],_ga=[0,_g9,_g8],_gb=[0,_ga,_ao],_gc=[1,_gb,_o],_gd=[0,7],_ge=[0,3],_gf=[0,_ge,_gd],_gg=[0,_gf,_ap],_gh=[1,_gg,_gc],_gi=[1,_g7,_gh],_gj=[0,_gi,_g4],_gk=function(_){return _dD(10,_gj,_e0,_);},_gl=function(_){return _gk(_);};
var hasteMain = function() {A(_gl, [0]);};window.onload = hasteMain;