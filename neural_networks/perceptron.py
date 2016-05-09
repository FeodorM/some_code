#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np
import random

random.seed(42)


class Perceptron:

    def __init__(self, w, b):
        """
        Метод рассчитывает ответ перцептрона при предъявлении одного примера
        single_input - вектор примера размера (m, 1).
        Метод возвращает число (0 или 1) или boolean (True/False)
        """

        self.w = w
        self.b = b

    def forward_pass(self, single_input):
        """
        Метод рассчитывает ответ перцептрона при предъявлении одного примера
        single_input - вектор примера размера (m, 1).
        Метод возвращает число (0 или 1) или boolean (True/False)
        """

        return self.w.T.dot(single_input) + self.b > 0

    def vectorized_forward_pass(self, matrix):
        """
        Метод рассчитывает ответ перцептрона при предъявлении набора примеров
        input_matrix -
            матрица примеров размера (n, m), каждая строка - отдельный пример,
        n - количество примеров, m - количество переменных
        Возвращает вертикальный вектор размера (n, 1) с ответами перцептрона
        (элементы вектора - boolean или целые числа (0 или 1))
        """

        return (matrix.dot(self.w) + self.b > 0).reshape((-1, 1))

    def train_on_single_example(self, example, y):
        """
        принимает вектор активации входов example формы (m, 1)
        и правильный ответ для него (число 0 или 1 или boolean),
        обновляет значения весов перцептрона в соответствии с этим примером
        и возвращает размер ошибки,
        которая случилась на этом примере до изменения весов (0 или 1)
        (на её основании мы потом построим интересный график)
        """

        predicted = self.forward_pass(example)
        error = y - predicted
        self.w += error * example
        self.b += error
        return error

    def train_until_convergence(self, input_matrix, y, max_steps=1e8):
        """
        input_matrix - матрица входов размера (n, m),
        y - вектор правильных ответов размера (n, 1)
        (y[i] - правильный ответ на пример input_matrix[i]),
        max_steps - максимальное количество шагов.
        Применяем train_on_single_example,
        пока не перестанем ошибаться или до умопомрачения.
        Константа max_steps - наше понимание того, что считать умопомрачением.
        """
        i = 0
        errors = 1
        while errors and i < max_steps:
            i += 1
            errors = 0
            for example, answer in zip(input_matrix, y):
                example = example.reshape((example.size, 1))
                error = self.train_on_single_example(example, answer)
                errors += error  # Здесь было int(error)


def show_apples_and_pears(data, apples, pears):
    plt.scatter(data[apples][:, 0], data[apples][:, 1], color="red")
    plt.scatter(data[pears][:, 0], data[pears][:, 1], color="green")
    plt.xlabel("yellowness")
    plt.ylabel("symmetry")
    plt.show()


def main():
    data = np.loadtxt("data.csv", delimiter=",")
    pears = data[:, 2] == 1
    apples = np.logical_not(pears)
    show_apples_and_pears(data, apples, pears)


def test():
    pass


if __name__ == '__main__':
    main()
