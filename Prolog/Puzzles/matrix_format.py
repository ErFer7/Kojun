# -*- coding: utf-8 -*-

'''
Converte as matrizes.
'''

file_name = input()

matrix = []

with open(file_name, 'r+', encoding="utf-8") as file:

    puzzle = list(map(int, file.read().split()))

    size = puzzle[0]

    for i in range(size):

        matrix.append([])

        for j in range(size):

            matrix[i].append([puzzle[i * size + j + size**2 + 1], puzzle[i * size + j + 1]])

with open(file_name[:-4] + "_new.txt", 'w+', encoding="utf-8") as file:

    str_matrix = ''

    for i, line in enumerate(matrix):

        if i < len(matrix) - 1:
            str_matrix += str(line) + '.\n'
        else:
            str_matrix += str(line) + '.'

    str_matrix = str_matrix.replace('[0', '[_')

    file.write(str_matrix)
