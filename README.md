# FP
Лабораторные работы по функциональному программированию

## Варианты заданий:
[Lab1](lab1) - Вариант 1.17 (сложность 2)
Запрограммируйте на языке Коммон Лисп функцию-предикат с тремя параметрами - действительными положительными числами a, b, c.
Функция должна возвращать T (истину), если существует тупоугольный  треугольник с длинами сторон a, b и c.

[Lab2](lab2) - Вариант 2.41 (сложность 3)
Запрограммируйте рекурсивно на языке Коммон Лисп функцию-предикат tree-similar-p (x y), которая принимает два аргумента - дерева, представленных в виде списков атомов. Предикат должен вернуть истину, если одинаковые атомы расположены в списках х и у в одном и том же порядке при обходе дерева слева направо, т.е. независимо от внутренней структуры х и у.

[Lab3](lab3) - Вариант 3.44 (сложность 3)
Запрограммировать на языке Коммон Лисп функцию, принимающую в качестве единственного аргумента целое число n - порядок матрицы. Функция должна создавать и возвращать двумерный массив, представляющий целочисленную квадратную матрицу порядка n, элементами которой являются числа 1, 2, ... n2, расположенные по схеме, показанной на рисунке.

    (1   2  6  7)
    (3   5  8 13)
    (4   9 12 14)
    (10 11 15 16)

[Lab4](lab4) - Вариант № 4.38 (сложность 3)
Запрограммировать на языке Коммон Лисп функцию, принимающую один аргумент - натуральное число n, n<1000000.
Функция должна вернуть предложение, которое выражает это число русскими словами.

[Lab5](lab5) - Вариант № 5.24 (сложность 2)
Дан экземпляр класса triangle, причем все вершины треугольника могут быть заданы как декартовыми координатами (экземплярами класса cart), так и полярными (экземплярами класса polar).
Задание: Определить обычную функцию биссектриса, возвращающую объект-отрезок (экземпляр класса line), являющийся биссектрисой первого угла vertex1. Концы результирующего отрезка могут быть получены либо в декартовых, либо в полярных координатах.
