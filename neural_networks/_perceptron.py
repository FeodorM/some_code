#!/usr/bin/env python3

import numpy as np


def activation(w, x):
    return sum(w * x for w, x in zip(w, x)) > 0


def predict(weight):
    return 1


def func(examples, start_weight=(0, 0, 0)):
    w = np.array(start_weight)
    perfect = False
    while not perfect:
        perfect = True
        for example in examples:
            predicted = predict(example)
            if predicted != examples[example]:
                perfect = False
                if predicted == 0:
                    w += example
                if predicted == 1:
                    w -= example

if __name__ == '__main__':
    examples = {
        (1, .3): True,
        (.4, .5): True,
        (.7, .8): False
    }
    # examples = {np.array((1,) + e): v for e, v in examples.items()}
