var sample4x4 = [[1, 0, 3, 0],
		 [0, 4, 0, 2],
		 [0, 3, 4 ,0],
		 [4, 0, 2, 3]];

var sample9x9 = [[0, 7, 1, 4, 0, 0, 0, 0, 5],
		 [0, 0, 0, 0, 5, 0, 0, 8, 0],
		 [0, 0, 3, 9, 0, 7, 6, 0, 0],
		 [0, 0, 0, 0, 0, 1, 0, 0, 0],
		 [0, 9, 0, 8, 0, 6, 0, 0, 3],
		 [0, 0, 0, 0, 0, 0, 8, 2, 0],
		 [0, 6, 0, 0, 4, 0, 7, 0, 8],
		 [3, 0, 0, 0, 0, 0, 0, 9, 0],
		 [0, 0, 0, 0, 8, 5, 0, 0, 0]];

function possibilities (board, x, y) { 
    return _.difference(_.range(1, _.size(board[0]) + 1), 
			row(board, x, y),
			col(board, x, y),
			block(board, x, y))
}

function row(board, x, y) {
    return board[y];
}

function col(board, x, y) {
    return _.map(board, function (row) { row[x] })
}

function block(board, x, y) {
    var blockSize = Math.sqrt(_.size(board[0]))
    var origin = function (n) { 
	return blockSize * Math.floor(n / blockSize)
    }
    var relevantRows = _.take(_.drop(board, origin(y)), blockSize);
    return _.reduce(relevantRows, function (memo, row) { 
	return _.union(memo, _.take(_.drop(row, origin(y)), blockSize)) 
    }, []);
}

function isFilled (board) { 
    return _.every(board, function (row) { _.every(row, function (val) { val != 0; })})
}

function solve(board) {
    if (isFilled(board)) {
	return board;
    } else {
	return solve(_.map(board, function (row, y) { 
	    return _.map(row, function (val, x) { 
		var poss = possibilities(board, x, y);
		if (val != 0) {
		    return val;
		} else if ((val == 0) && (_.size(poss) == 1)) {
		    return poss[0];
		} else {
		    console.log("UNSOLVED");
		    return 0;
		}
	    })
	}));
    }
}