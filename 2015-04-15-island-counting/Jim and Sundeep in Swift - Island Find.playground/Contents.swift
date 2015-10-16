var grid = [[0,0,1,0,1],
            [1,1,0,0,0],
            [1,1,0,1,0],
            [0,0,0,1,1]]

func islands(inout grid: Array<Array<Int>>) -> Int {
    
    var counter = 0
    
    for i in 0..<grid.count {
        for j in 0..<grid[i].count {
            
            let cell = grid[i][j]
            if cell == 1 {
                let point = [i, j]
                sinkIsland(&grid, point)
                counter = counter + 1
            }
        }
    }
    
    return counter
}

func sinkIsland(inout grid: Array<Array<Int>>, point: Array<Int>) {
    var stack = [point]
    
    while stack.count > 0 {
        let p = stack.removeLast()
        let x = p[0]
        let y = p[1]
        let xMin = max(0, x-1)
        let yMin = max(0, y-1)
        let xMax = min(grid.count-1, x+1)
        let yMax = min(grid[0].count-1, y+1)
        
        for i in xMin...xMax {
            for j in yMin...yMax {
                if  grid[i][j] == 1 {
                    stack.append([i,j])
                    grid[i][j] = 0
                }
            }
        }
    }
}

islands(&grid)