#!/usr/bin/python
import sys
from random import randint

def main():
    w = h = 10
    n = 50
    if len(sys.argv) >= 3:
        w, h = int(sys.argv[1]), int(sys.argv[2])
    if len(sys.argv) == 4:
        n = int(sys.argv[3])
    pts = set()
    while len(pts) < n:
        pts.add( (randint(1,w), randint(1,h)) )

    print("%d %d"%(w,h))
    print("%d"%n)
    for pt in pts:
        print("%d %d"%pt)

if __name__ == "__main__":
    main()
