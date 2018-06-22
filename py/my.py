#!/bin/env python2

# Remember to set LD_LIBRARY_PATH to folder containing libifport.so


import numpy as np
from sequential import sequential


a,b,c,d = [[1]],[[2]],3,4
a = np.ones((3,3))
b = np.ones((3,3))
b[:,:] = 4

print(a,b,c,d)

print("Fortraning")

print(a.shape)
print(b.shape)

print(dir(sequential))
result = sequential.mm_seq_square(a,b)
print("Result: ", result)

print(a,b,c,d)