"""
Calculation
===

can calculate expressions in infix or postfix form
you need to import just calculate
"""


class MyError(Exception):
    ...


def weight(char):
    if char == '(':
        return 0
    if char == ')':
        return 1
    if char in ['+', '-']:
        return 2
    if char in ['*', '/', '//', '%', '**']:
        return 3
    raise MyError


def make_polish(orig):
    orig = orig.strip().split()
    stack = []
    for_return = []
    operations = {'(', ')', '+', '-', '*', '/', '//', '%', '**'}
    for elem in orig:
        if elem not in operations:
            for_return.append(elem)
        elif elem in operations:
            w = weight(elem)
            if not stack or w == 0 or w > weight(stack[-1]):
                stack.append(elem)
            else:
                while stack and weight(stack[-1]) >= w:
                    if stack[-1] == ')' or stack[-1] == '(':
                        stack.pop()
                    else:
                        for_return.append(stack.pop())
                stack.append(elem)
        else:
            raise MyError
    while stack:
        if stack[-1] == ')' or stack[-1] == '(':
            stack.pop()
        else:
            for_return.append(stack.pop())
    return for_return


def calculate(expression):
    """

    :param expression: str with expression, where operators divided by spaces
    :return: int / float / complex - result of calculation expression
    """
    stack = []
    operations = {'+', '-', '*', '/', '//', '%', '**'}
    for elem in make_polish(expression):
        if elem not in operations:
            stack.append(int(elem))
        else:
            b, a = stack.pop(), stack.pop()
            stack.append({
                             '+': lambda x, y:  x + y,
                             '-': lambda x, y:  x - y,
                             '*': lambda x, y:  x * y,
                             '/': lambda x, y:  x / y,
                             '//': lambda x, y: x // y,
                             '%': lambda x, y:  x % y,
                             '**': lambda x, y: x ** y,
                         }[elem](a, b))
    if len(stack) == 1:
        return stack.pop()
    else:
        raise MyError
