

var ctx, canvasl;
var vxs = [[ 500, 0 ], [ 0, 1000 ], [ 1000, 1000 ]];
function init() {
    canvasl = document.getElementById('triangles');
    ctx = canvasl.getContext("2d");

    ctx.clearRect(0, 0, 1000, 1000);
}

var WIDTH=20;
var HEIGHT=20;
function drawTriangle(x, y) {
    var path=new Path2D();
    path.moveTo(x, y);
    path.lineTo(x+WIDTH/2, y+HEIGHT);
    path.lineTo(x+WIDTH, y);
    ctx.fill(path);
}

var state=[[0,0],[20,0],[10,20]];


function treeCopy(tree, dx, dy) {
    return traverse(tree, function(x, y) {
        return [ x + dx, y + dy]
    });
}

function grow(tree, n) {
    var t1 = treeCopy(tree, n, 0);
    var t2 = treeCopy(tree, n/2, n);
    return [tree, t1, t2];
}

function traverse(tree, fn) {
    if( typeof tree[0]==="number" ) {
        return fn(tree[0], tree[1]);
    } else {
        var a2=[];
        for(var i=tree.length-1; i>=0; i--) {
            a2.push(traverse(tree[i], fn));
        }
        return a2;
    }
}

function render(tree) {
    traverse(tree, drawTriangle);
}
// @return [ width, height ]
function dimensions(tree) {
    var x1=Infinity, x2=-1, y1=Infinity, y2=-1;
    traverse(tree, function(x, y) {
        var dx=x+WIDTH, dy=y+HEIGHT;
        if( x<x1 ) x1=x;
        if( dx>x2 ) x2=dx;
        if( y<y1 ) y1=y;
        if( dy>y2 ) y2=dy;
    });
    return [ (x2-x1), (y2-y1) ];
}

var size = dimensions(state);
function step() {
    canvasl.width = size[0];
    canvasl.height = size[1];

    render(state);
    state = grow(state, size[0]);
    size[0]=size[0]*2;
    size[1]=size[1]*2;
}

window.onload = function() {
    init();
//    drawTriangle.apply(this, state);
};
