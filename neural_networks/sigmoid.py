#!/usr/bin/env python3

import matplotlib
import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d as p3
import numpy as np
import random
import time

from functools import partial
from numpy.linalg import norm

random.seed(42)


def sigmoid(x):
    """
    сигмоидальная функция, работает и с числами, и с векторами (поэлементно)
    """
    return 1 / (1 + np.exp(-x))


def sigmoid_prime(x):
    """
    производная сигмоидальной функции,
    работает и с числами, и с векторами (поэлементно)
    """
    return sigmoid(x) * (1 - sigmoid(x))


class Neuron:
    def __init__(self, weights, activation_function=sigmoid,
                 activation_function_derivative=sigmoid_prime):
        """
        weights - вертикальный вектор весов нейрона формы (m, 1),
            weights[0][0] - смещение
        activation_function - активационная функция нейрона,
            сигмоидальная функция по умолчанию
        activation_function_derivative -
            производная активационной функции нейрона
        """

        assert weights.shape[1] == 1, "Incorrect weight shape"

        self.w = weights
        self.activation_function = activation_function
        self.activation_function_derivative = activation_function_derivative

    def forward_pass(self, single_input):
        """
        активационная функция логистического нейрона
        single_input - вектор входов формы (m, 1),
        первый элемент вектора single_input -
            единица (если вы хотите учитывать смещение)
        """

        result = self.w.T.dot(single_input)[0]
        return self.activation_function(result)

    def summatory(self, input_matrix):
        """
        Вычисляет результат сумматорной функции
            для каждого примера из input_matrix.
        input_matrix - матрица примеров размера (n, m),
            каждая строка - отдельный пример,
        n - количество примеров, m - количество переменных.
        Возвращает вектор значений сумматорной функции размера (n, 1).
        """
        # Этот метод необходимо реализовать

        pass

    def activation(self, summatory_activation):
        """
        Вычисляет для каждого примера результат активационной функции,
        получив на вход вектор значений сумматорной функций
        summatory_activation - вектор размера (n, 1),
        где summatory_activation[i] -
            значение суммматорной функции для i-го примера.
        Возвращает вектор размера (n, 1), содержащий в i-й строке
        значение активационной функции для i-го примера.
        """
        # Этот метод необходимо реализовать

        pass

    def vectorized_forward_pass(self, input_matrix):
        """
        Векторизованная активационная функция логистического нейрона.
        input_matrix - матрица примеров размера (n, m),
            каждая строка - отдельный пример,
        n - количество примеров, m - количество переменных.
        Возвращает вертикальный вектор размера (n, 1)
            с выходными активациями нейрона
        (элементы вектора - float)
        """
        return self.activation(self.summatory(input_matrix))

    def SGD(self, X, y, batch_size,
            learning_rate=0.1, eps=1e-6, max_steps=200):
        """
        Внешний цикл алгоритма градиентного спуска.
        X - матрица входных активаций (n, m)
        y - вектор правильных ответов (n, 1)

        learning_rate - константа скорости обучения
        batch_size - размер батча, на основании которого
        рассчитывается градиент и совершается один шаг алгоритма

        eps - критерий остановки номер один:
            если разница между значением целевой функции
        до и после обновления весов меньше eps - алгоритм останавливается.
        Вторым вариантом была бы проверка размера градиента,
            а не изменение функции, что будет работать лучше - неочевидно.
        В заданиях используйте первый подход.

        max_steps - критерий остановки номер два:
            если количество обновлений весов
        достигло max_steps, то алгоритм останавливается

        Метод возвращает 1,
            если отработал первый критерий остановки (спуск сошёлся)
        и 0, если второй (спуск не достиг минимума за отведённое время).
        """

        # Этот метод необходимо реализовать

        pass

    def update_mini_batch(self, X, y, learning_rate, eps):
        """
        X - матрица размера (batch_size, m)
        y - вектор правильных ответов размера (batch_size, 1)
        learning_rate - константа скорости обучения
        eps - критерий остановки номер один:
            если разница между значением целевой функции
        до и после обновления весов меньше eps - алгоритм останавливается.

        Рассчитывает градиент
        (не забывайте использовать подготовленные заранее внешние функции)
        и обновляет веса нейрона.
        Если ошибка изменилась меньше, чем на eps - возвращаем 1,
        иначе возвращаем 0.
        """
        # Этот метод необходимо реализовать

        pass


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
    # show_apples_and_pears(data, apples, pears)


def test():
    n = Neuron(np.array([1, 2, 3]).reshape((-1, 1)))
    x = np.array([1, 2, 3])
    print(n.forward_pass(x))


if __name__ == '__main__':
    test()
