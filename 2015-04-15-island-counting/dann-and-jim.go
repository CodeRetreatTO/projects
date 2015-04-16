package main

import (
	"fmt"
)

type Range struct {
	start int
	end int
}

func swim (line string) (string) {
	if line[0] == '.' {
		return swim(line[1:])
	} else {
		return line
	}
}

func walk (line string) (string) {
	if line[0] != '.' {
		return walk(line[1:])
	} else {
		return line
	}
}

func dispatch (line string, ranges []Range, fn func (string) (string)) []Range {
	next := fn(line)
	return ranges
	
}

func scanMap (grid []string, ranges [][]Range) [][]Range {
//	ix := -1
//	s := []Range{}
	for _,char := range grid[0] {
		if char != '.' {
			fmt.Println("Land!")
		}
	}
	return ranges
}

func CountIslands(grid []string) int {
	return len(scanMap(grid, [][]Range{}))
}

func main () {
	fmt.Println("===================")
	
	strs := []string{
		"...xx...........",
		"................",
		"................",
		"................",
		"................",
		"................" }

	for _,ln := range strs {
		fmt.Println(ln)
	}
	fmt.Println(CountIslands(strs))
}
