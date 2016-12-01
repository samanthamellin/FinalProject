import pandas as pd
import numpy as np
import matplotlib as plt
import os
import time

fwTotals = list()
data = pd.read_csv('C:\\dev\labware\\EmittanceScanner\\EmittanceScannerRScripts\\CurrentSpeedTests\\resample.csv', index_col = None, header = 0, names = ['V1', 'V2', 'V3', 'V4', 'V5'])

#R script did not give reasonable header names. The names correspond as followed:
# V1 = FIBX
# V1 = FIBY
# V3 = X
# V4 = Y
# V5 = Current

#Definitions
FullWidthsTotal = list()
UpperLine = list()
LowerLine = list()
S1curr = 39.09e-12 #picoamps ( current through just the upper slit)
emx = 0

#Same functions from the EmittanceScanner Script
def pullData(dd):
    X = dd['V3']
    Y = dd['V4']
    dd['V1'] = round(dd['V1'] + 0.00353, 5)
    if X[0] > X[1]:
        R = np.sqrt(np.square(Y - Y[len(Y) - 1]) + np.square(X - X[len(X) - 1]))
        R_0 = - np.sqrt(np.square(-0.0016 - Y[len(Y) - 1 ]) + np.square(0.00307 - X[len(X) - 1]))
    else:
        R = np.sqrt(np.square(Y - Y[0]) + np.square(X - X[0]))
        R_0 = - np.sqrt(np.square(-0.0016 - Y[0]) + np.square(0.00307 - X[0]))
    S2_S1 = (R - R_0) - dd['V1']
    dd['xPrime'] = np.arctan(S2_S1 / 0.398731) # add a new column for Xprime
    Current = dd['V5']

#piecing out each point in scan
def IntegrateArrayYofX(a, b):
    cIntegral = list()
    cIntegral.append(0)
    n = len(b)
    for i in range(1,n):
        if(a[i-1] != a[i]):
           littleBitMore = (b[i-1]+b[i])/2
           littleBitMore *= (a[i] - a[i-1])
           cIntegral.append(cIntegral[i-1] + littleBitMore)
    return cIntegral

#function to sort the lists if they are not in increasing order
def sortVal(x,y):
    if(x[1] < x[0]):
        l = list(x)
        l.reverse()
        x = list(l)

        l = list(y)
        l.reverse()
        y = list(l)

def FW50(xx, cc):
    x = xx
    c = cc
    if(len(x) != len(c)):
        print("Lengths of arrays must match")
    if(len(x) < 2):
        print("Lengths of each array must be two or greater")
        return
    #Sort the arrays from smallest to largest
    n = len(c)
    if(x[1] < x[0]):
        l = list(x)
        l.reverse()
        x = list(l)
        l = list(c)
        l.reverse()
        c = list(l)
    for i in range(1,n):
        if(x[i] == x[i - 1]):
            x[i] = x[i - 1] + 0.00000001
        if(x[i] <= x[i - 1]):
            print("x array (xprime) must be monotonic increasing") 
            return -1
        if(c[i] < 0):   
            c[i] = 0
    cInt = IntegrateArrayYofX(x,c)
    fifyPercent = cInt[n-1]/2
    #Generate a list of the smallest FW50 x-intervals
    #note: we find the smallest x-interval for each yInt value in the lower half
    #looking up and for each yInt value in the upper half looking down and then
    #we pick the smallest one
    widths = list()
    for i in range(0,n):
        if (cInt[i] <= fifyPercent):
            #for points which come before halfway, we examine the view looking up
            for j in range(i+1, n):
                #take the first width which is greater than or equal to 50%
                if (cInt[j] - cInt[i] >= fifyPercent):
                    #make linear interpolation refinement
                    upperX = x[j] - (cInt[j] - cInt[i] - fifyPercent)/((cInt[j] - cInt[j-1])/(x[j] - x[j-1])) # UpperX is the actual position in xprime?
                    widths.append(upperX - x[i]) #width found by subtracting the current position from the UpperX
                    break #jump out of j loop
        else:
            #for points in the upper half, we take the smallest width looking down
            for j in range(-1, i-1, -1):
                 if (cInt[i] - cInt[j] >= fiftyPercent):
                     lowerX = x[j] + (cInt[i] - cInt[j] - fifyPercent)/((cInt[j+1] - cInt[j])/(x[j+1] - x[j]))
                     widths.append(x[i] - lowerX)
                     break
    if ( len(widths) > 0):
        widths.sort()
        FullWidthsTotal.append(widths[0])
        return widths[0]
    else:
        return 0

pullData(data)
positions = set(data['V1'])
pos = np.asarray(list(positions))
pos.sort()
#Calculate the FW50 for each individual positions of slit one.
for k in positions:
    g = data.groupby(['V1']).get_group(k).reset_index(drop = True)
    if FW50(g['xPrime'], g['V5']) == -1:
        xx = skipElement(g['xPrime'])
        cc = skipElement(g['V5'])
        a = FW50(xx,cc)
    else:
        print(FW50(g['xPrime'], g['V5'])) 

   
#print("original FW50 values: ", FullWidthsTotal)
#averageFW = np.average(FullWidthsTotal)
#FullWidthsTotal.clear();
#print("Average original FW50: ", averageFW)
