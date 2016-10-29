import random,math
B = 5
def gibbs(T=500):
    x = 1
    y = 1
    print "Iter  x  y"
    for i in range(T):
        if 0 < x < B:
            if 0 < y < B:
                x=random.expovariate(y)
                y=random.expovariate(x)
                print i,x,y

gibbs()
