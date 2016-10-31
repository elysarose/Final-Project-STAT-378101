from math import *
import random
import numpy
import matplotlib.pyplot as plt
B = 5
T = 50000
M = numpy.zeros((T, 3))


def gibbs(T,thin=10):
    x=1
    y=1
    print "Iter  x  y"
    for i in range(T):
        for j in range(thin):
            p1= random.uniform(0,1)
            x=-1/y*log(1-p1+p1*exp(-y*B))
            p2= random.uniform(0,1)
            y=-1/x*log(1-p2+p2*exp(-x*B))
            M[i,0] = i
            M[i,1] = x
            M[i,2] = y
        #print i,x,y

gibbs(T)
#print M
print numpy.mean(M[:,1])
print numpy.mean(M[:,2])
plt.hist(M[:,1])
