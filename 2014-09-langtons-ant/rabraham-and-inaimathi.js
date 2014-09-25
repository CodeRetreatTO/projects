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

var _0=0,_1=function(_2,_3){var _4=E(_2);return _4[0]==0?E(_3):[1,_4[1],new T(function(){return _1(_4[2],_3);})];},_5=[0],_6=function(_7,_){while(1){var _8=E(_7);if(!_8[0]){return _0;}else{var _9=_8[2],_a=E(_8[1]);switch(_a[0]){case 0:var _b=A(_a[1],[_]);_7=_1(_9,[1,_b,_5]);continue;case 1:_7=_1(_9,_a[1]);continue;default:_7=_9;continue;}}}},_c=function(_d){return [2];},_e=function(_f,_g){var _h=new T(function(){return A(_f,[_g]);});return function(_i){return A(_h,[function(_j){return A(_e,[_f,_j,_i]);}]);};},_k=[0,0],_l=[1],_m=unCStr("Failure in Data.Map.balanceL"),_n=new T(function(){return err(_m);}),_o=function(_p,_q,_r){var _s=E(_r);if(!_s[0]){var _t=_s[1],_u=E(_q);if(!_u[0]){var _v=_u[1],_w=_u[2];if(_v<=(imul(3,_t)|0)){return [0,(1+_v|0)+_t|0,E(E(_p)),E(_u),E(_s)];}else{var _x=E(_u[3]);if(!_x[0]){var _y=_x[1],_z=E(_u[4]);if(!_z[0]){var _A=_z[1],_B=_z[2],_C=_z[3];if(_A>=(imul(2,_y)|0)){var _D=function(_E){var _F=E(_z[4]);return _F[0]==0?[0,(1+_v|0)+_t|0,E(_B),E([0,(1+_y|0)+_E|0,E(_w),E(_x),E(_C)]),E([0,(1+_t|0)+_F[1]|0,E(E(_p)),E(_F),E(_s)])]:[0,(1+_v|0)+_t|0,E(_B),E([0,(1+_y|0)+_E|0,E(_w),E(_x),E(_C)]),E([0,1+_t|0,E(E(_p)),E(_l),E(_s)])];},_G=E(_C);return _G[0]==0?_D(_G[1]):_D(0);}else{return [0,(1+_v|0)+_t|0,E(_w),E(_x),E([0,(1+_t|0)+_A|0,E(E(_p)),E(_z),E(_s)])];}}else{return E(_n);}}else{return E(_n);}}}else{return [0,1+_t|0,E(E(_p)),E(_l),E(_s)];}}else{var _H=E(_q);if(!_H[0]){var _I=_H[1],_J=_H[2],_K=_H[4],_L=E(_H[3]);if(!_L[0]){var _M=_L[1],_N=E(_K);if(!_N[0]){var _O=_N[1],_P=_N[2],_Q=_N[3];if(_O>=(imul(2,_M)|0)){var _R=function(_S){var _T=E(_N[4]);return _T[0]==0?[0,1+_I|0,E(_P),E([0,(1+_M|0)+_S|0,E(_J),E(_L),E(_Q)]),E([0,1+_T[1]|0,E(E(_p)),E(_T),E(_l)])]:[0,1+_I|0,E(_P),E([0,(1+_M|0)+_S|0,E(_J),E(_L),E(_Q)]),E([0,1,E(E(_p)),E(_l),E(_l)])];},_U=E(_Q);return _U[0]==0?_R(_U[1]):_R(0);}else{return [0,1+_I|0,E(_J),E(_L),E([0,1+_O|0,E(E(_p)),E(_N),E(_l)])];}}else{return [0,3,E(_J),E(_L),E([0,1,E(E(_p)),E(_l),E(_l)])];}}else{var _V=E(_K);return _V[0]==0?[0,3,E(_V[2]),E([0,1,E(_J),E(_l),E(_l)]),E([0,1,E(E(_p)),E(_l),E(_l)])]:[0,2,E(E(_p)),E(_H),E(_l)];}}else{return [0,1,E(E(_p)),E(_l),E(_l)];}}},_W=unCStr("Failure in Data.Map.balanceR"),_X=new T(function(){return err(_W);}),_Y=function(_Z,_10,_11){var _12=E(_10);if(!_12[0]){var _13=_12[1],_14=E(_11);if(!_14[0]){var _15=_14[1],_16=_14[2];if(_15<=(imul(3,_13)|0)){return [0,(1+_13|0)+_15|0,E(E(_Z)),E(_12),E(_14)];}else{var _17=E(_14[3]);if(!_17[0]){var _18=_17[1],_19=_17[2],_1a=_17[3],_1b=E(_14[4]);if(!_1b[0]){var _1c=_1b[1];if(_18>=(imul(2,_1c)|0)){var _1d=function(_1e){var _1f=E(_Z),_1g=E(_17[4]);return _1g[0]==0?[0,(1+_13|0)+_15|0,E(_19),E([0,(1+_13|0)+_1e|0,E(_1f),E(_12),E(_1a)]),E([0,(1+_1c|0)+_1g[1]|0,E(_16),E(_1g),E(_1b)])]:[0,(1+_13|0)+_15|0,E(_19),E([0,(1+_13|0)+_1e|0,E(_1f),E(_12),E(_1a)]),E([0,1+_1c|0,E(_16),E(_l),E(_1b)])];},_1h=E(_1a);return _1h[0]==0?_1d(_1h[1]):_1d(0);}else{return [0,(1+_13|0)+_15|0,E(_16),E([0,(1+_13|0)+_18|0,E(E(_Z)),E(_12),E(_17)]),E(_1b)];}}else{return E(_X);}}else{return E(_X);}}}else{return [0,1+_13|0,E(E(_Z)),E(_12),E(_l)];}}else{var _1i=E(_11);if(!_1i[0]){var _1j=_1i[1],_1k=_1i[2],_1l=_1i[4],_1m=E(_1i[3]);if(!_1m[0]){var _1n=_1m[1],_1o=_1m[2],_1p=_1m[3],_1q=E(_1l);if(!_1q[0]){var _1r=_1q[1];if(_1n>=(imul(2,_1r)|0)){var _1s=function(_1t){var _1u=E(_Z),_1v=E(_1m[4]);return _1v[0]==0?[0,1+_1j|0,E(_1o),E([0,1+_1t|0,E(_1u),E(_l),E(_1p)]),E([0,(1+_1r|0)+_1v[1]|0,E(_1k),E(_1v),E(_1q)])]:[0,1+_1j|0,E(_1o),E([0,1+_1t|0,E(_1u),E(_l),E(_1p)]),E([0,1+_1r|0,E(_1k),E(_l),E(_1q)])];},_1w=E(_1p);return _1w[0]==0?_1s(_1w[1]):_1s(0);}else{return [0,1+_1j|0,E(_1k),E([0,1+_1n|0,E(E(_Z)),E(_l),E(_1m)]),E(_1q)];}}else{return [0,3,E(_1o),E([0,1,E(E(_Z)),E(_l),E(_l)]),E([0,1,E(_1k),E(_l),E(_l)])];}}else{var _1x=E(_1l);return _1x[0]==0?[0,3,E(_1k),E([0,1,E(E(_Z)),E(_l),E(_l)]),E(_1x)]:[0,2,E(E(_Z)),E(_l),E(_1i)];}}else{return [0,1,E(E(_Z)),E(_l),E(_l)];}}},_1y=function(_1z,_1A,_1B){var _1C=E(_1B);if(!_1C[0]){var _1D=_1C[3],_1E=_1C[4],_1F=E(_1C[2]),_1G=E(_1F[1])[1];if(_1z>=_1G){if(_1z!=_1G){return _Y(_1F,_1D,_1y(_1z,_1A,_1E));}else{var _1H=E(_1F[2])[1];return _1A>=_1H?_1A!=_1H?_Y(_1F,_1D,_1y(_1z,_1A,_1E)):[0,_1C[1],E([0,[0,_1z],[0,_1A]]),E(_1D),E(_1E)]:_o(_1F,_1y(_1z,_1A,_1D),_1E);}}else{return _o(_1F,_1y(_1z,_1A,_1D),_1E);}}else{return [0,1,E([0,[0,_1z],[0,_1A]]),E(_l),E(_l)];}},_1I=function(_1J,_1K,_1L){var _1M=E(_1L);if(!_1M[0]){var _1N=_1M[3],_1O=_1M[4],_1P=E(_1M[2]),_1Q=E(_1P[1])[1];if(_1J>=_1Q){if(_1J!=_1Q){return _Y(_1P,_1N,_1I(_1J,_1K,_1O));}else{var _1R=E(_1K),_1S=_1R[1],_1T=E(_1P[2])[1];return _1S>=_1T?_1S!=_1T?_Y(_1P,_1N,_1y(_1J,_1S,_1O)):[0,_1M[1],E([0,[0,_1J],_1R]),E(_1N),E(_1O)]:_o(_1P,_1y(_1J,_1S,_1N),_1O);}}else{return _o(_1P,_1I(_1J,_1K,_1N),_1O);}}else{return [0,1,E([0,[0,_1J],_1K]),E(_l),E(_l)];}},_1U=function(_1V,_1W){var _1X=E(_1V),_1Y=_1X[2],_1Z=E(_1W);if(!_1Z[0]){var _20=_1Z[3],_21=_1Z[4],_22=E(_1Z[2]),_23=E(_1X[1])[1],_24=E(_22[1])[1];if(_23>=_24){if(_23!=_24){return _Y(_22,_20,_1I(_23,_1Y,_21));}else{var _25=E(_1Y)[1],_26=E(_22[2])[1];return _25>=_26?_25!=_26?_Y(_22,_20,_1y(_23,_25,_21)):[0,_1Z[1],E(_1X),E(_20),E(_21)]:_o(_22,_1y(_23,_25,_20),_21);}}else{return _o(_22,_1I(_23,_1Y,_20),_21);}}else{return [0,1,E(_1X),E(_l),E(_l)];}},_27=function(_28,_29){while(1){var _2a=E(_29);if(!_2a[0]){return E(_28);}else{var _2b=_1U(_2a[1],_28);_29=_2a[2];_28=_2b;continue;}}},_2c=function(_2d){return [0,1,E(E(_2d)),E(_l),E(_l)];},_2e=function(_2f,_2g){var _2h=E(_2g);return _2h[0]==0?_Y(_2h[2],_2h[3],_2e(_2f,_2h[4])):_2c(_2f);},_2i=function(_2j,_2k){var _2l=E(_2k);return _2l[0]==0?_o(_2l[2],_2i(_2j,_2l[3]),_2l[4]):_2c(_2j);},_2m=function(_2n,_2o,_2p,_2q,_2r,_2s){var _2t=E(_2o);if(!_2t[0]){var _2u=_2t[1],_2v=_2t[2],_2w=_2t[3],_2x=_2t[4];return (imul(3,_2u)|0)>=_2p?(imul(3,_2p)|0)>=_2u?[0,(_2u+_2p|0)+1|0,E(E(_2n)),E(_2t),E([0,_2p,E(_2q),E(_2r),E(_2s)])]:_Y(_2v,_2w,_2m(_2n,_2x,_2p,_2q,_2r,_2s)):_o(_2q,_2y(_2n,_2u,_2v,_2w,_2x,_2r),_2s);}else{return _2i(_2n,[0,_2p,E(_2q),E(_2r),E(_2s)]);}},_2y=function(_2z,_2A,_2B,_2C,_2D,_2E){var _2F=E(_2E);if(!_2F[0]){var _2G=_2F[1],_2H=_2F[2],_2I=_2F[3],_2J=_2F[4];return (imul(3,_2A)|0)>=_2G?(imul(3,_2G)|0)>=_2A?[0,(_2A+_2G|0)+1|0,E(E(_2z)),E([0,_2A,E(_2B),E(_2C),E(_2D)]),E(_2F)]:_Y(_2B,_2C,_2m(_2z,_2D,_2G,_2H,_2I,_2J)):_o(_2H,_2y(_2z,_2A,_2B,_2C,_2D,_2I),_2J);}else{return _2e(_2z,[0,_2A,E(_2B),E(_2C),E(_2D)]);}},_2K=function(_2L,_2M,_2N){var _2O=E(_2M);if(!_2O[0]){var _2P=_2O[1],_2Q=_2O[2],_2R=_2O[3],_2S=_2O[4],_2T=E(_2N);if(!_2T[0]){var _2U=_2T[1],_2V=_2T[2],_2W=_2T[3],_2X=_2T[4];return (imul(3,_2P)|0)>=_2U?(imul(3,_2U)|0)>=_2P?[0,(_2P+_2U|0)+1|0,E(E(_2L)),E(_2O),E(_2T)]:_Y(_2Q,_2R,_2m(_2L,_2S,_2U,_2V,_2W,_2X)):_o(_2V,_2y(_2L,_2P,_2Q,_2R,_2S,_2W),_2X);}else{return _2e(_2L,_2O);}}else{return _2i(_2L,_2N);}},_2Y=function(_2Z,_30,_31,_32){var _33=E(_2Z);if(_33==1){var _34=E(_32);if(!_34[0]){return [0,[0,1,E([0,[0,_30],[0,_31]]),E(_l),E(_l)],_5,_5];}else{var _35=E(_34[1]),_36=E(_35[1])[1];return _30>=_36?_30!=_36?[0,[0,1,E([0,[0,_30],[0,_31]]),E(_l),E(_l)],_5,_34]:_31>=E(_35[2])[1]?[0,[0,1,E([0,[0,_30],[0,_31]]),E(_l),E(_l)],_5,_34]:[0,[0,1,E([0,[0,_30],[0,_31]]),E(_l),E(_l)],_34,_5]:[0,[0,1,E([0,[0,_30],[0,_31]]),E(_l),E(_l)],_34,_5];}}else{var _37=_2Y(_33>>1,_30,_31,_32),_38=_37[1],_39=_37[3],_3a=E(_37[2]);if(!_3a[0]){return [0,_38,_5,_39];}else{var _3b=_3a[1],_3c=E(_3a[2]);if(!_3c[0]){return [0,new T(function(){return _2e(_3b,_38);}),_5,_39];}else{var _3d=_3c[2],_3e=E(_3b),_3f=E(_3c[1]),_3g=_3f[2],_3h=E(_3e[1])[1],_3i=E(_3f[1])[1];if(_3h>=_3i){if(_3h!=_3i){return [0,_38,_5,_3a];}else{var _3j=E(_3g)[1];if(E(_3e[2])[1]>=_3j){return [0,_38,_5,_3a];}else{var _3k=_2Y(_33>>1,_3i,_3j,_3d);return [0,new T(function(){return _2K(_3e,_38,_3k[1]);}),_3k[2],_3k[3]];}}}else{var _3l=_3m(_33>>1,_3i,_3g,_3d);return [0,new T(function(){return _2K(_3e,_38,_3l[1]);}),_3l[2],_3l[3]];}}}}},_3m=function(_3n,_3o,_3p,_3q){var _3r=E(_3n);if(_3r==1){var _3s=E(_3q);if(!_3s[0]){return [0,[0,1,E([0,[0,_3o],_3p]),E(_l),E(_l)],_5,_5];}else{var _3t=E(_3s[1]),_3u=E(_3t[1])[1];if(_3o>=_3u){if(_3o!=_3u){return [0,[0,1,E([0,[0,_3o],_3p]),E(_l),E(_l)],_5,_3s];}else{var _3v=E(_3p);return _3v[1]>=E(_3t[2])[1]?[0,[0,1,E([0,[0,_3o],_3v]),E(_l),E(_l)],_5,_3s]:[0,[0,1,E([0,[0,_3o],_3v]),E(_l),E(_l)],_3s,_5];}}else{return [0,[0,1,E([0,[0,_3o],_3p]),E(_l),E(_l)],_3s,_5];}}}else{var _3w=_3m(_3r>>1,_3o,_3p,_3q),_3x=_3w[1],_3y=_3w[3],_3z=E(_3w[2]);if(!_3z[0]){return [0,_3x,_5,_3y];}else{var _3A=_3z[1],_3B=E(_3z[2]);if(!_3B[0]){return [0,new T(function(){return _2e(_3A,_3x);}),_5,_3y];}else{var _3C=_3B[2],_3D=E(_3A),_3E=E(_3B[1]),_3F=_3E[2],_3G=E(_3D[1])[1],_3H=E(_3E[1])[1];if(_3G>=_3H){if(_3G!=_3H){return [0,_3x,_5,_3z];}else{var _3I=E(_3F)[1];if(E(_3D[2])[1]>=_3I){return [0,_3x,_5,_3z];}else{var _3J=_2Y(_3r>>1,_3H,_3I,_3C);return [0,new T(function(){return _2K(_3D,_3x,_3J[1]);}),_3J[2],_3J[3]];}}}else{var _3K=_3m(_3r>>1,_3H,_3F,_3C);return [0,new T(function(){return _2K(_3D,_3x,_3K[1]);}),_3K[2],_3K[3]];}}}}},_3L=function(_3M,_3N,_3O){var _3P=E(_3O);if(!_3P[0]){return E(_3N);}else{var _3Q=_3P[1],_3R=E(_3P[2]);if(!_3R[0]){return _2e(_3Q,_3N);}else{var _3S=E(_3Q),_3T=E(_3R[1]),_3U=_3T[2],_3V=E(_3S[1])[1],_3W=E(_3T[1])[1],_3X=new T(function(){var _3Y=_3m(_3M,_3W,_3U,_3R[2]),_3Z=_3Y[1],_40=E(_3Y[3]);return _40[0]==0?_3L(_3M<<1,_2K(_3S,_3N,_3Z),_3Y[2]):_27(_2K(_3S,_3N,_3Z),_40);});return _3V>=_3W?_3V!=_3W?_27(_3N,_3P):E(_3S[2])[1]>=E(_3U)[1]?_27(_3N,_3P):E(_3X):E(_3X);}}},_41=function(_42){var _43=E(_42);if(!_43[0]){return [1];}else{var _44=_43[1],_45=E(_43[2]);if(!_45[0]){return [0,1,E(E(_44)),E(_l),E(_l)];}else{var _46=E(_44),_47=E(_45[1]),_48=E(_46[1])[1],_49=E(_47[1])[1];return _48>=_49?_48!=_49?_27([0,1,E(_46),E(_l),E(_l)],_45):E(_46[2])[1]>=E(_47[2])[1]?_27([0,1,E(_46),E(_l),E(_l)],_45):_3L(1,[0,1,E(_46),E(_l),E(_l)],_45):_3L(1,[0,1,E(_46),E(_l),E(_l)],_45);}}},_4a=new T(function(){return _41(_5);}),_4b=0,_4c=[0,4],_4d=[0,_4c,_4c],_4e=[0,_4d,_4b],_4f=2,_4g=[0,12],_4h=[0,15],_4i=[0,_4h,_4g],_4j=[0,_4i,_4f],_4k=[1,_4j,_5],_4l=3,_4m=[0,7],_4n=[0,3],_4o=[0,_4n,_4m],_4p=[0,_4o,_4l],_4q=[1,_4p,_4k],_4r=[1,_4e,_4q],_4s=[0,_4r,_4a],_4t=[0,_4s,_k],_4u=[0,1],_4v=function(_4w){while(1){var _4x=E(_4w);if(!_4x[0]){_4w=[1,I_fromInt(_4x[1])];continue;}else{return I_toString(_4x[1]);}}},_4y=function(_4z,_4A){return _1(fromJSStr(_4v(_4z)),_4A);},_4B=function(_4C,_4D){var _4E=E(_4C);if(!_4E[0]){var _4F=_4E[1],_4G=E(_4D);return _4G[0]==0?_4F<_4G[1]:I_compareInt(_4G[1],_4F)>0;}else{var _4H=_4E[1],_4I=E(_4D);return _4I[0]==0?I_compareInt(_4H,_4I[1])<0:I_compare(_4H,_4I[1])<0;}},_4J=[0,41],_4K=[0,40],_4L=[0,0],_4M=function(_4N,_4O,_4P){return _4N<=6?_4y(_4O,_4P):!_4B(_4O,_4L)?_4y(_4O,_4P):[1,_4K,new T(function(){return _1(fromJSStr(_4v(_4O)),[1,_4J,_4P]);})];},_4Q=function(_4R,_4S,_4T,_4U){return A(_4R,[new T(function(){return function(_){var _4V=jsSet(E(_4S)[1],toJSStr(E(_4T)),toJSStr(E(_4U)));return _0;};})]);},_4W=[1],_4X=unCStr("Failure in Data.Map.balanceL"),_4Y=new T(function(){return err(_4X);}),_4Z=function(_50,_51,_52,_53){var _54=E(_53);if(!_54[0]){var _55=_54[1],_56=E(_52);if(!_56[0]){var _57=_56[1],_58=_56[2],_59=_56[3];if(_57<=(imul(3,_55)|0)){return [0,(1+_57|0)+_55|0,E(E(_50)),_51,E(_56),E(_54)];}else{var _5a=E(_56[4]);if(!_5a[0]){var _5b=_5a[1],_5c=E(_56[5]);if(!_5c[0]){var _5d=_5c[1],_5e=_5c[2],_5f=_5c[3],_5g=_5c[4];if(_5d>=(imul(2,_5b)|0)){var _5h=function(_5i){var _5j=E(_5c[5]);return _5j[0]==0?[0,(1+_57|0)+_55|0,E(_5e),_5f,E([0,(1+_5b|0)+_5i|0,E(_58),_59,E(_5a),E(_5g)]),E([0,(1+_55|0)+_5j[1]|0,E(E(_50)),_51,E(_5j),E(_54)])]:[0,(1+_57|0)+_55|0,E(_5e),_5f,E([0,(1+_5b|0)+_5i|0,E(_58),_59,E(_5a),E(_5g)]),E([0,1+_55|0,E(E(_50)),_51,E(_4W),E(_54)])];},_5k=E(_5g);return _5k[0]==0?_5h(_5k[1]):_5h(0);}else{return [0,(1+_57|0)+_55|0,E(_58),_59,E(_5a),E([0,(1+_55|0)+_5d|0,E(E(_50)),_51,E(_5c),E(_54)])];}}else{return E(_4Y);}}else{return E(_4Y);}}}else{return [0,1+_55|0,E(E(_50)),_51,E(_4W),E(_54)];}}else{var _5l=E(_52);if(!_5l[0]){var _5m=_5l[1],_5n=_5l[2],_5o=_5l[3],_5p=_5l[5],_5q=E(_5l[4]);if(!_5q[0]){var _5r=_5q[1],_5s=E(_5p);if(!_5s[0]){var _5t=_5s[1],_5u=_5s[2],_5v=_5s[3],_5w=_5s[4];if(_5t>=(imul(2,_5r)|0)){var _5x=function(_5y){var _5z=E(_5s[5]);return _5z[0]==0?[0,1+_5m|0,E(_5u),_5v,E([0,(1+_5r|0)+_5y|0,E(_5n),_5o,E(_5q),E(_5w)]),E([0,1+_5z[1]|0,E(E(_50)),_51,E(_5z),E(_4W)])]:[0,1+_5m|0,E(_5u),_5v,E([0,(1+_5r|0)+_5y|0,E(_5n),_5o,E(_5q),E(_5w)]),E([0,1,E(E(_50)),_51,E(_4W),E(_4W)])];},_5A=E(_5w);return _5A[0]==0?_5x(_5A[1]):_5x(0);}else{return [0,1+_5m|0,E(_5n),_5o,E(_5q),E([0,1+_5t|0,E(E(_50)),_51,E(_5s),E(_4W)])];}}else{return [0,3,E(_5n),_5o,E(_5q),E([0,1,E(E(_50)),_51,E(_4W),E(_4W)])];}}else{var _5B=E(_5p);return _5B[0]==0?[0,3,E(_5B[2]),_5B[3],E([0,1,E(_5n),_5o,E(_4W),E(_4W)]),E([0,1,E(E(_50)),_51,E(_4W),E(_4W)])]:[0,2,E(E(_50)),_51,E(_5l),E(_4W)];}}else{return [0,1,E(E(_50)),_51,E(_4W),E(_4W)];}}},_5C=unCStr("Failure in Data.Map.balanceR"),_5D=new T(function(){return err(_5C);}),_5E=function(_5F,_5G,_5H,_5I){var _5J=E(_5H);if(!_5J[0]){var _5K=_5J[1],_5L=E(_5I);if(!_5L[0]){var _5M=_5L[1],_5N=_5L[2],_5O=_5L[3];if(_5M<=(imul(3,_5K)|0)){return [0,(1+_5K|0)+_5M|0,E(E(_5F)),_5G,E(_5J),E(_5L)];}else{var _5P=E(_5L[4]);if(!_5P[0]){var _5Q=_5P[1],_5R=_5P[2],_5S=_5P[3],_5T=_5P[4],_5U=E(_5L[5]);if(!_5U[0]){var _5V=_5U[1];if(_5Q>=(imul(2,_5V)|0)){var _5W=function(_5X){var _5Y=E(_5F),_5Z=E(_5P[5]);return _5Z[0]==0?[0,(1+_5K|0)+_5M|0,E(_5R),_5S,E([0,(1+_5K|0)+_5X|0,E(_5Y),_5G,E(_5J),E(_5T)]),E([0,(1+_5V|0)+_5Z[1]|0,E(_5N),_5O,E(_5Z),E(_5U)])]:[0,(1+_5K|0)+_5M|0,E(_5R),_5S,E([0,(1+_5K|0)+_5X|0,E(_5Y),_5G,E(_5J),E(_5T)]),E([0,1+_5V|0,E(_5N),_5O,E(_4W),E(_5U)])];},_60=E(_5T);return _60[0]==0?_5W(_60[1]):_5W(0);}else{return [0,(1+_5K|0)+_5M|0,E(_5N),_5O,E([0,(1+_5K|0)+_5Q|0,E(E(_5F)),_5G,E(_5J),E(_5P)]),E(_5U)];}}else{return E(_5D);}}else{return E(_5D);}}}else{return [0,1+_5K|0,E(E(_5F)),_5G,E(_5J),E(_4W)];}}else{var _61=E(_5I);if(!_61[0]){var _62=_61[1],_63=_61[2],_64=_61[3],_65=_61[5],_66=E(_61[4]);if(!_66[0]){var _67=_66[1],_68=_66[2],_69=_66[3],_6a=_66[4],_6b=E(_65);if(!_6b[0]){var _6c=_6b[1];if(_67>=(imul(2,_6c)|0)){var _6d=function(_6e){var _6f=E(_5F),_6g=E(_66[5]);return _6g[0]==0?[0,1+_62|0,E(_68),_69,E([0,1+_6e|0,E(_6f),_5G,E(_4W),E(_6a)]),E([0,(1+_6c|0)+_6g[1]|0,E(_63),_64,E(_6g),E(_6b)])]:[0,1+_62|0,E(_68),_69,E([0,1+_6e|0,E(_6f),_5G,E(_4W),E(_6a)]),E([0,1+_6c|0,E(_63),_64,E(_4W),E(_6b)])];},_6h=E(_6a);return _6h[0]==0?_6d(_6h[1]):_6d(0);}else{return [0,1+_62|0,E(_63),_64,E([0,1+_67|0,E(E(_5F)),_5G,E(_4W),E(_66)]),E(_6b)];}}else{return [0,3,E(_68),_69,E([0,1,E(E(_5F)),_5G,E(_4W),E(_4W)]),E([0,1,E(_63),_64,E(_4W),E(_4W)])];}}else{var _6i=E(_65);return _6i[0]==0?[0,3,E(_63),_64,E([0,1,E(E(_5F)),_5G,E(_4W),E(_4W)]),E(_6i)]:[0,2,E(E(_5F)),_5G,E(_4W),E(_61)];}}else{return [0,1,E(E(_5F)),_5G,E(_4W),E(_4W)];}}},_6j=function(_6k,_6l,_6m,_6n){var _6o=E(_6n);if(!_6o[0]){var _6p=_6o[3],_6q=_6o[4],_6r=_6o[5],_6s=E(_6o[2]),_6t=E(_6s[1])[1];if(_6k>=_6t){if(_6k!=_6t){return _5E(_6s,_6p,_6q,_6j(_6k,_6l,_6m,_6r));}else{var _6u=E(_6s[2])[1];return _6l>=_6u?_6l!=_6u?_5E(_6s,_6p,_6q,_6j(_6k,_6l,_6m,_6r)):[0,_6o[1],E([0,[0,_6k],[0,_6l]]),_6m,E(_6q),E(_6r)]:_4Z(_6s,_6p,_6j(_6k,_6l,_6m,_6q),_6r);}}else{return _4Z(_6s,_6p,_6j(_6k,_6l,_6m,_6q),_6r);}}else{return [0,1,E([0,[0,_6k],[0,_6l]]),_6m,E(_4W),E(_4W)];}},_6v=function(_6w,_6x,_6y,_6z){var _6A=E(_6z);if(!_6A[0]){var _6B=_6A[3],_6C=_6A[4],_6D=_6A[5],_6E=E(_6A[2]),_6F=E(_6E[1])[1];if(_6w>=_6F){if(_6w!=_6F){return _5E(_6E,_6B,_6C,_6v(_6w,_6x,_6y,_6D));}else{var _6G=E(_6x),_6H=_6G[1],_6I=E(_6E[2])[1];return _6H>=_6I?_6H!=_6I?_5E(_6E,_6B,_6C,_6j(_6w,_6H,_6y,_6D)):[0,_6A[1],E([0,[0,_6w],_6G]),_6y,E(_6C),E(_6D)]:_4Z(_6E,_6B,_6j(_6w,_6H,_6y,_6C),_6D);}}else{return _4Z(_6E,_6B,_6v(_6w,_6x,_6y,_6C),_6D);}}else{return [0,1,E([0,[0,_6w],_6x]),_6y,E(_4W),E(_4W)];}},_6J=function(_6K,_6L,_6M){var _6N=E(_6K),_6O=_6N[2],_6P=E(_6M);if(!_6P[0]){var _6Q=_6P[3],_6R=_6P[4],_6S=_6P[5],_6T=E(_6P[2]),_6U=E(_6N[1])[1],_6V=E(_6T[1])[1];if(_6U>=_6V){if(_6U!=_6V){return _5E(_6T,_6Q,_6R,_6v(_6U,_6O,_6L,_6S));}else{var _6W=E(_6O)[1],_6X=E(_6T[2])[1];return _6W>=_6X?_6W!=_6X?_5E(_6T,_6Q,_6R,_6j(_6U,_6W,_6L,_6S)):[0,_6P[1],E(_6N),_6L,E(_6R),E(_6S)]:_4Z(_6T,_6Q,_6j(_6U,_6W,_6L,_6R),_6S);}}else{return _4Z(_6T,_6Q,_6v(_6U,_6O,_6L,_6R),_6S);}}else{return [0,1,E(_6N),_6L,E(_4W),E(_4W)];}},_6Y=function(_6Z,_70){while(1){var _71=E(_70);if(!_71[0]){return E(_6Z);}else{var _72=E(_71[1]),_73=_6J(_72[1],_72[2],_6Z);_70=_71[2];_6Z=_73;continue;}}},_74=function(_75,_76){return [0,1,E(E(_75)),_76,E(_4W),E(_4W)];},_77=function(_78,_79,_7a){var _7b=E(_7a);return _7b[0]==0?_5E(_7b[2],_7b[3],_7b[4],_77(_78,_79,_7b[5])):_74(_78,_79);},_7c=function(_7d,_7e,_7f){var _7g=E(_7f);return _7g[0]==0?_4Z(_7g[2],_7g[3],_7c(_7d,_7e,_7g[4]),_7g[5]):_74(_7d,_7e);},_7h=function(_7i,_7j,_7k,_7l,_7m,_7n,_7o,_7p){var _7q=E(_7k);if(!_7q[0]){var _7r=_7q[1],_7s=_7q[2],_7t=_7q[3],_7u=_7q[4],_7v=_7q[5];return (imul(3,_7r)|0)>=_7l?(imul(3,_7l)|0)>=_7r?[0,(_7r+_7l|0)+1|0,E(E(_7i)),_7j,E(_7q),E([0,_7l,E(_7m),_7n,E(_7o),E(_7p)])]:_5E(_7s,_7t,_7u,_7h(_7i,_7j,_7v,_7l,_7m,_7n,_7o,_7p)):_4Z(_7m,_7n,_7w(_7i,_7j,_7r,_7s,_7t,_7u,_7v,_7o),_7p);}else{return _7c(_7i,_7j,[0,_7l,E(_7m),_7n,E(_7o),E(_7p)]);}},_7w=function(_7x,_7y,_7z,_7A,_7B,_7C,_7D,_7E){var _7F=E(_7E);if(!_7F[0]){var _7G=_7F[1],_7H=_7F[2],_7I=_7F[3],_7J=_7F[4],_7K=_7F[5];return (imul(3,_7z)|0)>=_7G?(imul(3,_7G)|0)>=_7z?[0,(_7z+_7G|0)+1|0,E(E(_7x)),_7y,E([0,_7z,E(_7A),_7B,E(_7C),E(_7D)]),E(_7F)]:_5E(_7A,_7B,_7C,_7h(_7x,_7y,_7D,_7G,_7H,_7I,_7J,_7K)):_4Z(_7H,_7I,_7w(_7x,_7y,_7z,_7A,_7B,_7C,_7D,_7J),_7K);}else{return _77(_7x,_7y,[0,_7z,E(_7A),_7B,E(_7C),E(_7D)]);}},_7L=function(_7M,_7N,_7O,_7P){var _7Q=E(_7O);if(!_7Q[0]){var _7R=_7Q[1],_7S=_7Q[2],_7T=_7Q[3],_7U=_7Q[4],_7V=_7Q[5],_7W=E(_7P);if(!_7W[0]){var _7X=_7W[1],_7Y=_7W[2],_7Z=_7W[3],_80=_7W[4],_81=_7W[5];return (imul(3,_7R)|0)>=_7X?(imul(3,_7X)|0)>=_7R?[0,(_7R+_7X|0)+1|0,E(E(_7M)),_7N,E(_7Q),E(_7W)]:_5E(_7S,_7T,_7U,_7h(_7M,_7N,_7V,_7X,_7Y,_7Z,_80,_81)):_4Z(_7Y,_7Z,_7w(_7M,_7N,_7R,_7S,_7T,_7U,_7V,_80),_81);}else{return _77(_7M,_7N,_7Q);}}else{return _7c(_7M,_7N,_7P);}},_82=function(_83,_84,_85,_86,_87){var _88=E(_83);if(_88==1){var _89=E(_87);if(!_89[0]){return [0,[0,1,E([0,[0,_84],[0,_85]]),_86,E(_4W),E(_4W)],_5,_5];}else{var _8a=E(E(_89[1])[1]),_8b=E(_8a[1])[1];return _84>=_8b?_84!=_8b?[0,[0,1,E([0,[0,_84],[0,_85]]),_86,E(_4W),E(_4W)],_5,_89]:_85>=E(_8a[2])[1]?[0,[0,1,E([0,[0,_84],[0,_85]]),_86,E(_4W),E(_4W)],_5,_89]:[0,[0,1,E([0,[0,_84],[0,_85]]),_86,E(_4W),E(_4W)],_89,_5]:[0,[0,1,E([0,[0,_84],[0,_85]]),_86,E(_4W),E(_4W)],_89,_5];}}else{var _8c=_82(_88>>1,_84,_85,_86,_87),_8d=_8c[1],_8e=_8c[3],_8f=E(_8c[2]);if(!_8f[0]){return [0,_8d,_5,_8e];}else{var _8g=E(_8f[1]),_8h=_8g[1],_8i=_8g[2],_8j=E(_8f[2]);if(!_8j[0]){return [0,new T(function(){return _77(_8h,_8i,_8d);}),_5,_8e];}else{var _8k=_8j[2],_8l=E(_8j[1]),_8m=_8l[2],_8n=E(_8h),_8o=E(_8l[1]),_8p=_8o[2],_8q=E(_8n[1])[1],_8r=E(_8o[1])[1];if(_8q>=_8r){if(_8q!=_8r){return [0,_8d,_5,_8f];}else{var _8s=E(_8p)[1];if(E(_8n[2])[1]>=_8s){return [0,_8d,_5,_8f];}else{var _8t=_82(_88>>1,_8r,_8s,_8m,_8k);return [0,new T(function(){return _7L(_8n,_8i,_8d,_8t[1]);}),_8t[2],_8t[3]];}}}else{var _8u=_8v(_88>>1,_8r,_8p,_8m,_8k);return [0,new T(function(){return _7L(_8n,_8i,_8d,_8u[1]);}),_8u[2],_8u[3]];}}}}},_8v=function(_8w,_8x,_8y,_8z,_8A){var _8B=E(_8w);if(_8B==1){var _8C=E(_8A);if(!_8C[0]){return [0,[0,1,E([0,[0,_8x],_8y]),_8z,E(_4W),E(_4W)],_5,_5];}else{var _8D=E(E(_8C[1])[1]),_8E=E(_8D[1])[1];if(_8x>=_8E){if(_8x!=_8E){return [0,[0,1,E([0,[0,_8x],_8y]),_8z,E(_4W),E(_4W)],_5,_8C];}else{var _8F=E(_8y);return _8F[1]>=E(_8D[2])[1]?[0,[0,1,E([0,[0,_8x],_8F]),_8z,E(_4W),E(_4W)],_5,_8C]:[0,[0,1,E([0,[0,_8x],_8F]),_8z,E(_4W),E(_4W)],_8C,_5];}}else{return [0,[0,1,E([0,[0,_8x],_8y]),_8z,E(_4W),E(_4W)],_8C,_5];}}}else{var _8G=_8v(_8B>>1,_8x,_8y,_8z,_8A),_8H=_8G[1],_8I=_8G[3],_8J=E(_8G[2]);if(!_8J[0]){return [0,_8H,_5,_8I];}else{var _8K=E(_8J[1]),_8L=_8K[1],_8M=_8K[2],_8N=E(_8J[2]);if(!_8N[0]){return [0,new T(function(){return _77(_8L,_8M,_8H);}),_5,_8I];}else{var _8O=_8N[2],_8P=E(_8N[1]),_8Q=_8P[2],_8R=E(_8L),_8S=E(_8P[1]),_8T=_8S[2],_8U=E(_8R[1])[1],_8V=E(_8S[1])[1];if(_8U>=_8V){if(_8U!=_8V){return [0,_8H,_5,_8J];}else{var _8W=E(_8T)[1];if(E(_8R[2])[1]>=_8W){return [0,_8H,_5,_8J];}else{var _8X=_82(_8B>>1,_8V,_8W,_8Q,_8O);return [0,new T(function(){return _7L(_8R,_8M,_8H,_8X[1]);}),_8X[2],_8X[3]];}}}else{var _8Y=_8v(_8B>>1,_8V,_8T,_8Q,_8O);return [0,new T(function(){return _7L(_8R,_8M,_8H,_8Y[1]);}),_8Y[2],_8Y[3]];}}}}},_8Z=function(_90,_91,_92){var _93=E(_92);if(!_93[0]){return E(_91);}else{var _94=E(_93[1]),_95=_94[1],_96=_94[2],_97=E(_93[2]);if(!_97[0]){return _77(_95,_96,_91);}else{var _98=E(_97[1]),_99=E(_95),_9a=E(_98[1]),_9b=_9a[2],_9c=E(_99[1])[1],_9d=E(_9a[1])[1],_9e=new T(function(){var _9f=_8v(_90,_9d,_9b,_98[2],_97[2]),_9g=_9f[1],_9h=E(_9f[3]);return _9h[0]==0?_8Z(_90<<1,_7L(_99,_96,_91,_9g),_9f[2]):_6Y(_7L(_99,_96,_91,_9g),_9h);});return _9c>=_9d?_9c!=_9d?_6Y(_91,_93):E(_99[2])[1]>=E(_9b)[1]?_6Y(_91,_93):E(_9e):E(_9e);}}},_9i=function(_9j){var _9k=E(_9j);if(!_9k[0]){return [1];}else{var _9l=E(_9k[1]),_9m=_9l[1],_9n=_9l[2],_9o=E(_9k[2]);if(!_9o[0]){return [0,1,E(E(_9m)),_9n,E(_4W),E(_4W)];}else{var _9p=E(_9m),_9q=E(E(_9o[1])[1]),_9r=E(_9p[1])[1],_9s=E(_9q[1])[1];return _9r>=_9s?_9r!=_9s?_6Y([0,1,E(_9p),_9n,E(_4W),E(_4W)],_9o):E(_9p[2])[1]>=E(_9q[2])[1]?_6Y([0,1,E(_9p),_9n,E(_4W),E(_4W)],_9o):_8Z(1,[0,1,E(_9p),_9n,E(_4W),E(_4W)],_9o):_8Z(1,[0,1,E(_9p),_9n,E(_4W),E(_4W)],_9o);}}},_9t=function(_9u,_9v){var _9w=E(_9u),_9x=E(_9v);return _9w[1]>_9x[1]?E(_9w):E(_9x);},_9y=function(_9z,_9A){var _9B=E(_9z),_9C=E(_9A);return _9B[1]>_9C[1]?E(_9C):E(_9B);},_9D=function(_9E,_9F){var _9G=E(_9E),_9H=E(_9F);return [0,new T(function(){return _9t(_9G[1],_9H[1]);}),new T(function(){return _9t(_9G[2],_9H[2]);})];},_9I=function(_9J,_9K){var _9L=E(_9J),_9M=E(_9K);return [0,new T(function(){return _9y(_9L[1],_9M[1]);}),new T(function(){return _9y(_9L[2],_9M[2]);})];},_9N=function(_9O,_9P,_9Q){while(1){var _9R=(function(_9S,_9T,_9U){var _9V=E(_9U);if(!_9V[0]){return [0,_9S,_9T];}else{var _9W=E(_9V[1])[1];_9O=new T(function(){return _9I(_9S,_9W);});_9P=new T(function(){return _9D(_9T,_9W);});_9Q=_9V[2];return null;}})(_9O,_9P,_9Q);if(_9R!=null){return _9R;}}},_9X=[0,0],_9Y=[0,_9X,_9X],_9Z=[0,_9X,_9X],_a0=function(_a1,_a2){while(1){var _a3=(function(_a4,_a5){var _a6=E(_a5);if(!_a6[0]){var _a7=E(_a6[2]),_a8=_a7[1],_a9=_a7[2],_aa=_a0(_a4,_a6[3]);_a1=[0,new T(function(){var _ab=E(_aa[1]);return [0,new T(function(){return _9y(_ab[1],_a8);}),new T(function(){return _9y(_ab[2],_a9);})];}),new T(function(){var _ac=E(_aa[2]);return [0,new T(function(){return _9t(_ac[1],_a8);}),new T(function(){return _9t(_ac[2],_a9);})];})];_a2=_a6[4];return null;}else{return E(_a4);}})(_a1,_a2);if(_a3!=null){return _a3;}}},_ad=[0,_9Z,_9Y],_ae=function(_af,_ag){var _ah=new T(function(){var _ai=_9N(_9Z,_9Y,_af);return [0,_ai[1],_ai[2]];}),_aj=new T(function(){return _a0(_ad,_ag);});return [0,new T(function(){var _ak=E(E(_aj)[1]),_al=E(E(_ah)[1]);return [0,new T(function(){return _9y(_ak[1],_al[1]);}),new T(function(){return _9y(_ak[2],_al[2]);})];}),new T(function(){var _am=E(E(_aj)[2]),_an=E(E(_ah)[2]);return [0,new T(function(){return _9t(_am[1],_an[1]);}),new T(function(){return _9t(_am[2],_an[2]);})];})];},_ao=function(_ap,_aq,_ar){while(1){var _as=E(_ar);if(!_as[0]){var _at=_as[3],_au=_as[4],_av=E(_as[2]),_aw=E(_av[1])[1];if(_ap>=_aw){if(_ap!=_aw){_ar=_au;continue;}else{var _ax=E(_av[2])[1];if(_aq>=_ax){if(_aq!=_ax){_ar=_au;continue;}else{return true;}}else{_ar=_at;continue;}}}else{_ar=_at;continue;}}else{return false;}}},_ay=function(_az,_aA,_aB){while(1){var _aC=E(_aB);if(!_aC[0]){var _aD=_aC[3],_aE=_aC[4],_aF=E(_aC[2]),_aG=E(_aF[1])[1];if(_az>=_aG){if(_az!=_aG){_aB=_aE;continue;}else{var _aH=E(_aA)[1],_aI=E(_aF[2])[1];return _aH>=_aI?_aH!=_aI?_ao(_az,_aH,_aE):true:_ao(_az,_aH,_aD);}}else{_aB=_aD;continue;}}else{return false;}}},_aJ=function(_aK,_aL,_aM){var _aN=E(_aM);if(!_aN[0]){var _aO=_aN[3],_aP=_aN[4],_aQ=E(_aN[2]),_aR=E(_aK)[1],_aS=E(_aQ[1])[1];if(_aR>=_aS){if(_aR!=_aS){return _ay(_aR,_aL,_aP);}else{var _aT=E(_aL)[1],_aU=E(_aQ[2])[1];return _aT>=_aU?_aT!=_aU?_ao(_aR,_aT,_aP):true:_ao(_aR,_aT,_aO);}}else{return _ay(_aR,_aL,_aO);}}else{return false;}},_aV=function(_aW,_aX,_aY){while(1){var _aZ=E(_aY);if(!_aZ[0]){var _b0=_aZ[4],_b1=_aZ[5],_b2=E(_aZ[2]),_b3=E(_b2[1])[1];if(_aW>=_b3){if(_aW!=_b3){_aY=_b1;continue;}else{var _b4=E(_b2[2])[1];if(_aX>=_b4){if(_aX!=_b4){_aY=_b1;continue;}else{return [1,_aZ[3]];}}else{_aY=_b0;continue;}}}else{_aY=_b0;continue;}}else{return [0];}}},_b5=function(_b6,_b7,_b8){while(1){var _b9=E(_b8);if(!_b9[0]){var _ba=_b9[4],_bb=_b9[5],_bc=E(_b9[2]),_bd=E(_bc[1])[1];if(_b6>=_bd){if(_b6!=_bd){_b8=_bb;continue;}else{var _be=E(_b7)[1],_bf=E(_bc[2])[1];return _be>=_bf?_be!=_bf?_aV(_b6,_be,_bb):[1,_b9[3]]:_aV(_b6,_be,_ba);}}else{_b8=_ba;continue;}}else{return [0];}}},_bg=function(_bh,_bi,_bj){var _bk=E(_bj);if(!_bk[0]){var _bl=_bk[4],_bm=_bk[5],_bn=E(_bk[2]),_bo=E(_bh)[1],_bp=E(_bn[1])[1];if(_bo>=_bp){if(_bo!=_bp){return _b5(_bo,_bi,_bm);}else{var _bq=E(_bi)[1],_br=E(_bn[2])[1];return _bq>=_br?_bq!=_br?_aV(_bo,_bq,_bm):[1,_bk[3]]:_aV(_bo,_bq,_bl);}}else{return _b5(_bo,_bi,_bl);}}else{return [0];}},_bs=function(_bt,_bu,_bv){while(1){var _bw=E(_bv);if(!_bw[0]){var _bx=_bw[4],_by=_bw[5],_bz=E(_bw[2]),_bA=E(_bz[1])[1];if(_bt>=_bA){if(_bt!=_bA){_bv=_by;continue;}else{var _bB=E(_bz[2])[1];if(_bu>=_bB){if(_bu!=_bB){_bv=_by;continue;}else{return true;}}else{_bv=_bx;continue;}}}else{_bv=_bx;continue;}}else{return false;}}},_bC=function(_bD,_bE,_bF){while(1){var _bG=E(_bF);if(!_bG[0]){var _bH=_bG[4],_bI=_bG[5],_bJ=E(_bG[2]),_bK=E(_bJ[1])[1];if(_bD>=_bK){if(_bD!=_bK){_bF=_bI;continue;}else{var _bL=E(_bE)[1],_bM=E(_bJ[2])[1];return _bL>=_bM?_bL!=_bM?_bs(_bD,_bL,_bI):true:_bs(_bD,_bL,_bH);}}else{_bF=_bH;continue;}}else{return false;}}},_bN=function(_bO,_bP,_bQ){var _bR=E(_bQ);if(!_bR[0]){var _bS=_bR[4],_bT=_bR[5],_bU=E(_bR[2]),_bV=E(_bO)[1],_bW=E(_bU[1])[1];if(_bV>=_bW){if(_bV!=_bW){return _bC(_bV,_bP,_bT);}else{var _bX=E(_bP)[1],_bY=E(_bU[2])[1];return _bX>=_bY?_bX!=_bY?_bs(_bV,_bX,_bT):true:_bs(_bV,_bX,_bS);}}else{return _bC(_bV,_bP,_bS);}}else{return false;}},_bZ=function(_c0,_c1){if(_c0<=_c1){var _c2=function(_c3){return [1,[0,_c3],new T(function(){return _c3!=_c1?_c2(_c3+1|0):[0];})];};return _c2(_c0);}else{return [0];}},_c4=unCStr("Maybe.fromJust: Nothing"),_c5=new T(function(){return err(_c4);}),_c6=[0,10],_c7=function(_c8){var _c9=E(_c8);return _c9[0]==0?[0]:_1(_c9[1],[1,_c6,new T(function(){return _c7(_c9[2]);})]);},_ca=new T(function(){return _c7(_5);}),_cb=[0,8592],_cc=[0,8595],_cd=[0,8594],_ce=[0,8593],_cf=[0,79],_cg=[0,32],_ch=function(_ci){var _cj=E(_ci);return [0,_cj[1],_cj[2]];},_ck=function(_cl,_cm){var _cn=E(_cm);return _cn[0]==0?[0]:[1,new T(function(){return A(_cl,[_cn[1]]);}),new T(function(){return _ck(_cl,_cn[2]);})];},_co=function(_cp,_cq){var _cr=_ae(_cp,_cq),_cs=E(_cr[1]),_ct=E(_cr[2]),_cu=E(_cs[2])[1],_cv=E(_ct[2])[1];if(_cu<=_cv){var _cw=new T(function(){return _9i(_ck(_ch,_cp));}),_cx=new T(function(){return _bZ(E(_cs[1])[1],E(_ct[1])[1]);}),_cy=function(_cz){return [1,new T(function(){var _cA=[0,_cz],_cB=function(_cC){var _cD=E(_cC);if(!_cD[0]){return [0];}else{var _cE=_cD[1];return [1,new T(function(){if(!_bN(_cE,_cA,_cw)){return !_aJ(_cE,_cA,_cq)?E(_cg):E(_cf);}else{var _cF=_bg(_cE,_cA,_cw);if(!_cF[0]){return E(_c5);}else{switch(E(_cF[1])){case 0:return E(_ce);case 1:return E(_cd);case 2:return E(_cc);default:return E(_cb);}}}}),new T(function(){return _cB(_cD[2]);})];}};return _cB(_cx);}),new T(function(){return _cz!=_cv?_cy(_cz+1|0):[0];})];};return _c7(_cy(_cu));}else{return E(_ca);}},_cG=unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"),_cH=new T(function(){return err(_cG);}),_cI=function(_cJ){var _cK=E(E(_cJ)[1]);return _cK==(-2147483648)?E(_cH):[0,_cK-1|0];},_cL=unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"),_cM=new T(function(){return err(_cL);}),_cN=function(_cO){var _cP=E(E(_cO)[1]);return _cP==2147483647?E(_cM):[0,_cP+1|0];},_cQ=1,_cR=function(_cS,_cT,_cU,_cV){var _cW=E(_cV);if(!_cW[0]){var _cX=new T(function(){var _cY=_cR(_cW[1],_cW[2],_cW[3],_cW[4]);return [0,_cY[1],_cY[2]];});return [0,new T(function(){return E(E(_cX)[1]);}),new T(function(){return _o(_cT,_cU,E(_cX)[2]);})];}else{return [0,_cT,_cU];}},_cZ=function(_d0,_d1,_d2,_d3){var _d4=E(_d2);if(!_d4[0]){var _d5=new T(function(){var _d6=_cZ(_d4[1],_d4[2],_d4[3],_d4[4]);return [0,_d6[1],_d6[2]];});return [0,new T(function(){return E(E(_d5)[1]);}),new T(function(){return _Y(_d1,E(_d5)[2],_d3);})];}else{return [0,_d1,_d3];}},_d7=function(_d8,_d9){var _da=E(_d8);if(!_da[0]){var _db=_da[1],_dc=E(_d9);if(!_dc[0]){var _dd=_dc[1];if(_db<=_dd){var _de=_cZ(_dd,_dc[2],_dc[3],_dc[4]);return _o(_de[1],_da,_de[2]);}else{var _df=_cR(_db,_da[2],_da[3],_da[4]);return _Y(_df[1],_df[2],_dc);}}else{return E(_da);}}else{return E(_d9);}},_dg=function(_dh,_di,_dj){var _dk=E(_dj);if(!_dk[0]){var _dl=_dk[3],_dm=_dk[4],_dn=E(_dk[2]),_do=E(_dn[1])[1];if(_dh>=_do){if(_dh!=_do){return _o(_dn,_dl,_dg(_dh,_di,_dm));}else{var _dp=E(_dn[2])[1];return _di>=_dp?_di!=_dp?_o(_dn,_dl,_dg(_dh,_di,_dm)):_d7(_dl,_dm):_Y(_dn,_dg(_dh,_di,_dl),_dm);}}else{return _Y(_dn,_dg(_dh,_di,_dl),_dm);}}else{return [1];}},_dq=function(_dr,_ds,_dt){var _du=E(_dt);if(!_du[0]){var _dv=_du[3],_dw=_du[4],_dx=E(_du[2]),_dy=E(_dx[1])[1];if(_dr>=_dy){if(_dr!=_dy){return _o(_dx,_dv,_dq(_dr,_ds,_dw));}else{var _dz=E(_ds)[1],_dA=E(_dx[2])[1];return _dz>=_dA?_dz!=_dA?_o(_dx,_dv,_dg(_dr,_dz,_dw)):_d7(_dv,_dw):_Y(_dx,_dg(_dr,_dz,_dv),_dw);}}else{return _Y(_dx,_dq(_dr,_ds,_dv),_dw);}}else{return [1];}},_dB=function(_dC,_dD,_dE){var _dF=E(_dE);if(!_dF[0]){var _dG=_dF[3],_dH=_dF[4],_dI=E(_dF[2]),_dJ=E(_dC)[1],_dK=E(_dI[1])[1];if(_dJ>=_dK){if(_dJ!=_dK){return _o(_dI,_dG,_dq(_dJ,_dD,_dH));}else{var _dL=E(_dD)[1],_dM=E(_dI[2])[1];return _dL>=_dM?_dL!=_dM?_o(_dI,_dG,_dg(_dJ,_dL,_dH)):_d7(_dG,_dH):_Y(_dI,_dg(_dJ,_dL,_dG),_dH);}}else{return _Y(_dI,_dq(_dJ,_dD,_dG),_dH);}}else{return [1];}},_dN=function(_dO,_dP,_dQ){return !_aJ(_dO,_dP,_dQ)?_1U([0,_dO,_dP],_dQ):_dB(_dO,_dP,_dQ);},_dR=function(_dS,_dT){var _dU=E(_dS);return _dN(_dU[1],_dU[2],_dT);},_dV=function(_dW,_dX){while(1){var _dY=E(_dX);if(!_dY[0]){return E(_dW);}else{var _dZ=_dR(E(_dY[1])[1],_dW);_dX=_dY[2];_dW=_dZ;continue;}}},_e0=function(_e1,_e2){return [0,new T(function(){return _ck(function(_e3){var _e4=E(_e3),_e5=_e4[2],_e6=E(_e4[1]),_e7=_e6[1],_e8=_e6[2];if(!_aJ(_e7,_e8,_e2)){switch(E(_e5)){case 0:return [0,[0,new T(function(){return _cN(_e7);}),_e8],_cQ];case 1:return [0,[0,_e7,new T(function(){return _cN(_e8);})],_4f];case 2:return [0,[0,new T(function(){return _cI(_e7);}),_e8],_4l];default:return [0,[0,_e7,new T(function(){return _cI(_e8);})],_4b];}}else{switch(E(_e5)){case 0:return [0,[0,new T(function(){return _cI(_e7);}),_e8],_4l];case 1:return [0,[0,_e7,new T(function(){return _cI(_e8);})],_4b];case 2:return [0,[0,new T(function(){return _cN(_e7);}),_e8],_cQ];default:return [0,[0,_e7,new T(function(){return _cN(_e8);})],_4f];}}},_e1);}),new T(function(){return _dV(_e2,_e1);})];},_e9=function(_ea){return E(_ea);},_eb=unCStr("world"),_ec=[0,10],_ed=unCStr("generations"),_ee=unCStr("innerHTML"),_ef=function(_eg,_eh){while(1){var _ei=E(_eg);if(!_ei[0]){var _ej=_ei[1],_ek=E(_eh);if(!_ek[0]){var _el=_ek[1],_em=addC(_ej,_el);if(!E(_em[2])){return [0,_em[1]];}else{_eg=[1,I_fromInt(_ej)];_eh=[1,I_fromInt(_el)];continue;}}else{_eg=[1,I_fromInt(_ej)];_eh=_ek;continue;}}else{var _en=E(_eh);if(!_en[0]){_eg=_ei;_eh=[1,I_fromInt(_en[1])];continue;}else{return [1,I_add(_ei[1],_en[1])];}}}},_eo=[2],_ep=function(_eq){return _c(_eq);},_er=function(_es,_et,_eu){return [0,function(_){var _ev=E(_es)[1],_ew=rMV(_ev),_ex=E(_ew);if(!_ex[0]){var _=wMV(_ev,[0,_ex[1],new T(function(){var _ey=new T(function(){return A(_eu,[_0]);});return _1(_ex[2],[1,[0,_et,function(_ez){return E(_ey);}],_5]);})]);return _eo;}else{var _eA=E(_ex[1]);if(!_eA[0]){var _=wMV(_ev,[0,_et,_5]);return new T(function(){return A(_eu,[_0]);});}else{var _=wMV(_ev,[1,_eA[2]]);return [1,[1,new T(function(){return A(_eu,[_0]);}),[1,new T(function(){return A(_eA[1],[_et,_ep]);}),_5]]];}}}];},_eB=[1,_5],_eC=function(_eD,_eE){return [0,function(_){var _eF=E(_eD)[1],_eG=rMV(_eF),_eH=E(_eG);if(!_eH[0]){var _eI=_eH[1],_eJ=E(_eH[2]);if(!_eJ[0]){var _=wMV(_eF,_eB);return new T(function(){return A(_eE,[_eI]);});}else{var _eK=E(_eJ[1]),_=wMV(_eF,[0,_eK[1],_eJ[2]]);return [1,[1,new T(function(){return A(_eE,[_eI]);}),[1,new T(function(){return A(_eK[2],[_ep]);}),_5]]];}}else{var _=wMV(_eF,[1,new T(function(){return _1(_eH[1],[1,function(_eL){var _eM=new T(function(){return A(_eE,[_eL]);});return function(_eN){return E(_eM);};},_5]);})]);return _eo;}}];},_eO=function(_eP,_eQ){return [0,function(_){var _eR=nMV(_eB),_eS=[0,_eR];return [0,function(_){var _eT=jsSetTimeout(E(_eP)[1],function(_){return _6([1,new T(function(){return _er(_eS,_0,_c);}),_5],_);});return new T(function(){return _eC(_eS,_eQ);});}];}];},_eU=unCStr(" could be found!"),_eV=function(_eW){return err(unAppCStr("No element with ID ",new T(function(){return _1(_eW,_eU);})));},_eX=function(_eY){var _eZ=E(_eY),_f0=_eZ[1],_f1=_eZ[2],_f2=new T(function(){return _ef(_f1,_4u);}),_f3=new T(function(){return _4M(0,_f1,_5);});return function(_f4){return [0,function(_){var _f5=E(_eb),_f6=jsFind(toJSStr(_f5)),_f7=E(_f6);if(!_f7[0]){return _eV(_f5);}else{var _f8=A(_4Q,[_e9,_f7[1],_ee,new T(function(){var _f9=E(_f0);return _co(_f9[1],_f9[2]);}),_]);return [0,function(_){var _fa=E(_ed),_fb=jsFind(toJSStr(_fa)),_fc=E(_fb);if(!_fc[0]){return _eV(_fa);}else{var _fd=A(_4Q,[_e9,_fc[1],_ee,_f3,_]);return new T(function(){var _fe=new T(function(){return A(_f4,[[0,new T(function(){var _ff=E(_f0),_fg=_e0(_ff[1],_ff[2]);return [0,_fg[1],_fg[2]];}),_f2]]);});return _eO(_ec,function(_fh){return E(_fe);});});}}];}}];};},_fi=new T(function(){return A(_e,[_eX,_4t,_c]);}),_fj=[1,_fi,_5],_fk=function(_){return _6(_fj,_);},_fl=function(_){return _fk(_);};
var hasteMain = function() {A(_fl, [0]);};window.onload = hasteMain;