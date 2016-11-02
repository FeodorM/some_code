#!/usr/bin/env python3

from sys import argv
# from subprocess import call
from os import popen

d = {
    'q': 'й',
    'w': 'ц',
    'e': 'у',
    'r': 'к',
    't': 'е',
    'y': 'н',
    'u': 'г',
    'i': 'ш',
    'o': 'щ',
    'p': 'з',
    '[': 'х',
    ']': 'ъ',
    'a': 'ф',
    's': 'ы',
    'd': 'в',
    'f': 'а',
    'g': 'п',
    'h': 'р',
    'j': 'о',
    'k': 'л',
    'l': 'д',
    ';': 'ж',
    '\'': 'э',
    'z': 'я',
    'x': 'ч',
    'c': 'с',
    'v': 'м',
    'b': 'и',
    'n': 'т',
    'm': 'ь',
    ',': 'б',
    '.': 'ю',
    '/': '.',
    '@': '\"',
    '#': '№',
    '$': ';',
    '^': ':',
    '&': '?',
}


def eng_to_rus(c):
    if c.lower() not in d:
        return c
    if c in d:
        return d[c]
    return d[c.lower()].upper()


text = ' '.join(''.join(eng_to_rus(c) for c in word) for word in argv[1:])
popen(
    'echo {} | xclip -sel clip'.format(text)
)
