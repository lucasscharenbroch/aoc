package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func turn(direction byte) byte {
	switch direction {
	case '^':
		return '>'
	case '>':
		return 'v'
	case 'v':
		return '<'
	case '<':
		return '^'
	default:
		panic("Bad direction")
	}
}

func advance(i int, j int, direction byte) (ip int, jp int) {
	switch direction {
	case '^':
		return i - 1, j
	case '>':
		return i, j + 1
	case 'v':
		return i + 1, j
	case '<':
		return i, j - 1
	default:
		panic("Bad direction")
	}
}

func main() {
	data, err := ioutil.ReadFile("6.input")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}
	lines := strings.Split(string(data), "\n")
	// can't assign to lines[i][j] (strings not mutable), so make `grid`
	grid := make([][]byte, len(lines))
	for i := range grid {
		grid[i] = []byte(lines[i])
	}

	startI, startJ := -1, -1
outer:
	for i, line := range grid {
		for j, char := range line {
			if char == '^' {
				startI, startJ = i, j
				break outer
			}
		}
	}

	if startI == -1 && startJ == -1 {
		fmt.Println("No starting position")
		return
	}

	direction := grid[startI][startJ]
	grid[startI][startJ] = '.'
	i, j := startI, startJ
	res, res2 := solve(grid, i, j, direction, false, 0)

	fmt.Println(res, res2)
}

func solve(grid [][]byte, i int, j int, direction byte, hypothetical bool, sd int) (res int, res2 int) {
	prev_unvisited := false

	// Use grid as a primitive vis mask.
	// To be safe, we really should keep four of these, one for each direction
	// But I guess just keeping the first direction works for this problem
	if grid[i][j] == direction {
		// found a cycle
		return 0, 1
	} else if grid[i][j] == '.' {
		grid[i][j] = direction
		prev_unvisited = true
	}

	ip, jp := advance(i, j, direction)

	if ip < 0 || jp < 0 || ip >= len(grid) || jp >= len(grid[ip]) {
		if prev_unvisited {
			grid[i][j] = '.'
		}
		return 1, 0
	}

	for grid[ip][jp] == '#' {
		direction = turn(direction)
		ip, jp = advance(i, j, direction)
	}

	res, res2 = solve(grid, ip, jp, direction, hypothetical, sd + 1)
	if prev_unvisited {
		res += 1
	}

	if !hypothetical && grid[ip][jp] == '.' {
		// try placing
		grid[ip][jp] = '#'
		temp := grid[i][j]
		grid[i][j] = '.'
		_, res2H := solve(grid, i, j, direction, true, sd + 1)
		res2 += res2H
		grid[ip][jp] = '.'
		grid[i][j] = temp
	}

	if prev_unvisited {
		grid[i][j] = '.'
	}

	return res, res2
}
