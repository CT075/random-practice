#!/usr/bin/python
for _ in range(5):
    s,*gs=[bin(i)[2:].zfill(3)for i in tuple(map(int,input().split(', ')))]
    print('\t'.join(map(' '.join, (gs,[''.join((('rwsrwsrwt'[i*3:i*3+3])if int(s[::-1][i])else'rwx')[j]if int(g[j])else'-'for j in range(3))for i,g in enumerate(gs)]))))
