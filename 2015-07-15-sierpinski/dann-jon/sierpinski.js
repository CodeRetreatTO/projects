/**
 * Created by Jonathan Willis on 7/15/15.
 */

;(function() {
}());

var ctx;
var vxs = [[ 500, 0 ], [ 0, 1000 ], [ 1000, 1000 ]];
function init() {
    var canvasl = document.getElementById('triangles');
    ctx = canvasl.getContext("2d");

    ctx.clearRect(0, 0, 1000, 1000);
}

function randomElement(elements) {
    var chosen = Math.floor(Math.random() * elements.length);
    return elements[chosen];
}

function halfwayElement(point, elements) {
    var chosen = randomElement(elements);
    return [ (point[0] + chosen[0])/2, (point[1] + chosen[1])/2 ];
}



function step(point) {
    var newPoint = halfwayElement(point, vxs);
    render.apply(this, newPoint);
    return newPoint;
}

function render(x, y) {
    ctx.fillRect(x, y, 1, 1);
}

function main() {
    var point = [ 500, 500 ];
    var frame = function frame() {
        var FPS=100;
        window.requestAnimationFrame(frame);
        for(var i=0; i<FPS; i++) {
            point = step(point);
        }
    };
    window.requestAnimationFrame(frame);
}

window.onload = function() {
    init();
    main();   
};
