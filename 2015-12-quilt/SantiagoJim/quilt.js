/**
 * Created by jrootham on 09/12/15.
 */

var small = [[1,1,2], [1,1,2], [3,3,4]]

function copy(old) {
    var result = []

    for (var i in old) {
        var row = []
        for (var j in old[i]) {
            row.push(old[i][j])
        }
        result.push(row)
    }

    return result
}

function main(quilt) {
    var working = copy(quilt)
}

function getNeighbors(quilt, n, i, j) {
    var res = [];
    var indices = [[-1, 0],[0,-1],[0,1],[1,0]];
    if(quilt[i - 1] && quilt[i - 1][j] && quilt[i - 1][j] != n) {
        res.push(quilt[i - 1][j]);
    }
    if(quilt[i][j - 1] && quilt[i][j - 1] != n) {
        res.push(quilt[i][j - 1]);
    }
    if(quilt[i][j + 1] && quilt[i][j + 1] != n) {
        res.push(quilt[i][j + 1]);
    }
    if(quilt[i + 1] && quilt[i + 1][j] && quilt[i + 1][j] != n) {
        res.push(quilt[i + 1][j]);
    }
    return res;
}

function addPairs(pairs, toAdd) {
    for(var i in toAdd) {
        if(pairs.indexOf(toAdd[i]) < 0) {
            pairs.push(toAdd[i]);
        }
    }
    return pairs;
}

function findPairs(quilt) {
    var n = 0;
    var pairs = [];
    while(n >= 0) {
        n = getNext(n);
        var tempPairs = [];
        for (var i in quilt) {
            for (var j in quilt[i]) {
                if (quilt[i][j] == n) {
                    tempPairs = addPairs(tempPairs, getNeighbors(quilt, n, i, j));
                }
            }
        }
    }
}

function getNext(quilt, n) {
    for (var i in quilt) {
        for (var j in quilt[i]) {
            if (quilt[i][j] > n) {
                return quilt[i][j]
            }
        }
    }

    return -1;
}

function join(quilt, a, b) {

}