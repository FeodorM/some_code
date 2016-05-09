#!/usr/bin/env python3

import sys

def main():
    secret_code = input()
    secret_code_length = len(secret_code)
    secret_code_set = set(secret_code)
    next(sys.stdin)
    # n = int(input())
    # for _ in range(n):
    for line in sys.stdin:
        not_matched = [x for x, y in zip(line[:-1], secret_code) if x != y]
        first       = secret_code_length - len(not_matched)
        second      = secret_code_length - len(secret_code_set - set(not_matched))
        print(first, second)

def change_input(filename):
    sys.stdin = open(filename)


if __name__ == '__main__':
    change_input('test')
    main()
