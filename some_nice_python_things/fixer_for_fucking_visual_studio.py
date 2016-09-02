from os import getcwd, walk
from os.path import sep
from shutil import move, rmtree


def take_up_headers_and_cpp(path, dir_name, names):
    for name in names:
        move(sep.join([path, name]), dir_name + '_' + name)
    rmtree(dir_name)


def rename_file(path, name):
    temp_name = 'temp'
    move(path, temp_name)
    rmtree(name)
    move(temp_name, name + '.cpp')


w = walk(getcwd())
next(w)
for path, _, files in w:
    files = [file for file in files if file.endswith(('.cpp', '.h'))]
    if not files:
        continue
    take_up_headers_and_cpp(path, path[path.rfind('\\') + 1:], files)
