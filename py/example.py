#!/bin/env python2

# Remember to set LD_LIBRARY_PATH to folder containing libifport.so


import numpy as np
from sequential import sequential

s = int(input("Provide matrix size: "))

arr1 = np.random.rand(s,s)
arr2 = np.random.rand(s,s)
expected = np.matmul(arr1, arr2)
print(arr1)
print(arr2)

print("Multiplicating arrays")

result, status = sequential.mm_seq_square(arr1, arr2)
print("Result:")
print(result)
print("Expected:")
print(expected)
print("Result == Expected: {}".format(np.all(result == expected)))
print("")


print("Generating arrays for gaussian elimination")

a = np.array(np.random.rand(s,s), order='F')
x = np.array(np.random.rand(s), order='F')
expected = np.linalg.solve(a,x)

print("Input:")
print(a)
print(x)

print("Eliminating...")
result = sequential.gauss_seq(a, x, s - 1)

print("Result: ")
print(np.array(a))
print("----")
print(np.array(x))


print("Nonzero elements outside of diagonal (expected zero): {}".format(np.count_nonzero(a - np.diag(np.diagonal(a)))))