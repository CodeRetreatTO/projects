
// [ [[0,0], [2,1]],
//   [[2,0], [3,1]],
//   [[0,1], [1,3]],
//   [[1,1], [2,2]],
//   [[2,1], [3,3]]]

var quilt = [[1,1,2],
             [3,4,2],
             [3,5,5]];

var q1 = [[1]],
    q2 = [[1,2]],
    q3 = [[1],
          [2]]
;
function edges(q, r, c) {
    var self = q[r][c];
    return [ [r-1, c], [r, c-1], [r+1, c], [r, c+1] ].filter(function(coord) {
        return q[coord[0]] && q[coord[0]][coord[1]];
    }).map(function(coord) {
        return q[coord[0]][coord[1]];
    }).filter(function(p){
        return p!==self;
    });
}

function unique(arr) {
    
    var list = {};
    arr.filter(function(p) {
        if(list[p]===undefined) {
            list[p]=1;
        }
    });
    return Object.keys(list).map(function(n) { return parseInt(n, 10); });
}
function edgeMap(q) {
    var r, c, map = {};

    for(r=0; r<q.length; r++) {
        for(c=0; c<q[r].length; c++) {
            piece = q[r][c];
            if(map[piece]!==undefined) {
                map[piece] = map[piece].concat(edges(q, r, c));
                map[piece] = unique(map[piece]);
            } else {
                map[piece] = edges(q, r, c);
            }
        }
    }
    return map;
}

function mergeEdge(map, piece1, piece2) {
    var newPiece = map[piece1].concat(map[piece2]);
    newPiece = unique(newPiece);
    return newPiece.filter( function(i) {
        return i!==piece1 && i!== piece2; 
    });
}

function replaceEdge(map, piece1, piece2) {
    var list = map[piece2];
    list.forEach(function(piece) {
        var secondList = map[piece];
        secondList = replaceSecond(secondList, piece2, piece1);
        secondList = unique(secondList);
        map[piece] = secondList;
    });

}

function replaceSecond(list, search, replace) {
    return list.map(function (p) {
        return p==search?replace:p;
    });
}

em = edgeMap(quilt);

