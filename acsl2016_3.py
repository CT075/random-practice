#!/usr/bin/python3
"""
Cameron Wong
Phillips Academy Andover
Senior-5
"""

import itertools as it
import copy

def solve(grid, inits):
    # Initialize with given information
    for letter, plc in inits:
        if plc in (2,3,4,5):
            col = plc-2
            if grid[0][col] == 'D': grid[1][col] = letter
            else: grid[0][col] = letter
        if plc in (7,13,19,25):
            row = plc//6 - 1
            if grid[row][0] == 'D': grid[row][1] = letter
            else: grid[row][0] = letter
        if plc in (12, 18, 24, 30):
            row = plc//6 - 2
            if grid[row][-1] == 'D': grid[row][-2] = letter
            else: grid[row][-1] = letter
        if plc in (32, 33, 34, 35):
            col = (plc-2) % 6
            if grid[-1][col] == 'D': grid[-2][col] = letter
            else: grid[-1][col] = letter
    # Brute force the solve
    missing = [[c for c in 'ABCD' if c not in row] for row in grid]
    combos = [it.permutations(m) for m in missing]
    for soln in it.product(*combos):
        g = copy.deepcopy(grid)
        for i, row in enumerate(soln):
            j = 0
            for c in row:
                while g[i][j] is not None: j += 1
                g[i][j] = c
        if is_solved(g): return g
    # Unsolveable???
    print("Huh, this doesn't work?")
    return None

def is_solved(grid):
    # rows
    for row in grid:
        if ''.join(sorted(row)) != 'ABCD': return False
    # cols
    for i in range(4):
        if ''.join(sorted(grid[j][i] for j in range(4))) != 'ABCD': return False
    return True

def init_grid(data):
    grid = [[None for j in range(4)] for i in range(4)]
    filled = map(int, data[:4])
    for fill in filled: grid[(fill-1)//6-1][(fill-1)%6-1] = 'D'
    lets = []
    for i in range(int(data[4])):
        lets.append((data[5 + 2*i], int(data[6 + 2*i])))
    return grid, lets

for i in range(5):
    a = input()
    solved = solve(*init_grid(a.split(', ')))
    print(''.join(c for row in solved for c in row if c != 'D'))
