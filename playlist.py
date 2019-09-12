class Single(list):
    def length(self): return len(self)
    def index(self, anIndex):
        return self[anIndex % self.length()]

def applyLength(item): return item.length()

class Merge(list):
    def length(self):
        return sum(map(applyLength,self))
    def index(self,anIndex):
        i = anIndex % self.length()
        for item in self:
            if(i<item.length()):
                return item.index(i)
            i = i - item.length()

from collections import deque
from math import sqrt, floor

def listDifference(list1,list2):
    list1.sort()
    list2.sort()
    list1 = deque(list1)
    list2 = deque(list2)
    result = []
    while(len(list1)>0 and len(list2)>0):
        if(list1[0]==list2[0]):
            list1.popleft()
            list2.popleft()
        elif(list1[0]<list2[0]):
            result.append(list1.popleft())
        else:
            list2.popleft()
    return result + list(list1)

def product(x,y): return x * y

class Complex(list):
    def length(self):
        from primes import primes
        subLengths = map(applyLength,self)
        factors = []
        for item in subLengths:
            factors = factors + listDifference(primes[item],factors)
        return reduce(product,factors,1) * len(self)
    def index(self,anIndex):
        return self[anIndex % len(self)].index(anIndex / len(self))
    def getFrame(self,aFrame):
        collect = []
        for i in range(len(self)):
            collect = collect + [self.index(aFrame*len(self)+i)]
        return collect

def testLengthComplex():
    a = Single([8,9,10,11,12,13])
    x = Single([1,2,3,4])
    y = Single([5,6,7])
    z = Complex([Merge([x,y]),a])
    return z.length()
