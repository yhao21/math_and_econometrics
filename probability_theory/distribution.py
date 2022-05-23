import numpy as np
import matplotlib.pyplot as plt






x = np.linspace(-5,5,100)

def cauchy(location, scale, x):
    a = ((x-location)/scale)**2
    fx = 1/(np.pi*scale*(1+a))
    return fx



results = []
for i in x:
    cauchy_fx = cauchy(0,1,i)
    results.append(cauchy_fx)

c = [3 for i in range(0,100)]
d = np.linspace(0,0.35, 100)
print(d)

plt.plot(x, results, c = 'black')
plt.plot(c,d, c = 'black')
#plt.savefig('figures/hahaa.png')
plt.clf()
