/**
 * Created by jrootham on 09/12/15.
 */
if (!Array.prototype.includesXY) {
    Array.prototype.includesXY = function(searchElement /*, fromIndex*/ ) {
        'use strict';
        var O = Object(this);
        var len = parseInt(O.length) || 0;
        if (len === 0) {
            return false;
        }
        var n = parseInt(arguments[1]) || 0;
        var k;
        if (n >= 0) {
            k = n;
        } else {
            k = len + n;
            if (k < 0) {k = 0;}
        }
        var currentElement;
        while (k < len) {
            currentElement = O[k];
            if (searchElement.x === currentElement.x && searchElement.y === currentElement.y) {
                return true;
            }
            k++;
        }
        return false;
    };
}

var example = [[{x:0, y:0}, {x:1,y:0}, {x:0, y:1}, {x:1, y:1}],
[{x:1, y:0}, {x:2, y:0}, {x:1, y:2}, {x:2, y:2}],
[{x:0, y:1}, {x:1, y:1}, {x:0, y:3}, {x:1, y:3}],
[{x:1, y:2}, {x:2, y:2}, {x:1, y:3}, {x:2, y:3}]];

function join(a, b) {
    var left = a.filter (function(t) {
        return !b.includesXY(t)
    })
    var right = b.filter (function(t) {
        return !a.includesXY(t)
    })

    return left.concat(right);
}

var j = join(example[0], example[1]);

console.log(j);
