#!/usr/bin/python

CONSONANTS = 'bcdfghjklmnpqrstvwxyz'

def get_runs(s):
    s = s.lower()
    values = [c for c in s if c in CONSONANTS]
    max_run = 0
    prev_asc = ''
    prev_desc = ''
    prev = ''
    runs_asc = []
    runs_desc = []
    for c in values:
        if not prev:
            prev_asc = c
            prev_desc = c
            prev = c
            continue
        if c > prev:
            runs_desc.append(prev_desc)
            prev_desc = c
            prev_asc += c
        if c < prev:
            runs_asc.append(prev_asc)
            prev_asc = c
            prev_desc += c
        if c == prev:
            prev_asc += c
            prev_desc +=c
        prev = c
    runs_asc.append(prev_asc)
    runs_desc.append(prev_desc)
    return max(*map(len, runs_asc), *map(len, runs_desc))

if __name__ == '__main__':
    for i in range(5): print(get_runs(input()))
