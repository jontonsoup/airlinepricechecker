import json
import cPickle as pickle
import sys
import random
import matplotlib.pyplot as plt
import matplotlib
import numpy as np
import scipy.stats as stats
from math import fabs
from datetime import datetime
from pprint import pprint

#Needs two arrays to put into stats.ttest_rel(a,b)
def pairedt(pairs, numSamples):
    results = dict()
    t,v = pairs.items()
    diffs = [t[1][x] - v[1][x] for x in range(len(t[1]))]
    plotDiffs(diffs)
    sampleSize = int(len(diffs)/numSamples)
    indices = range(len(diffs))
    random.shuffle(indices)
    mean_diffs = []
    i = 0
    for sample in range(numSamples):
        total_diff = 0
        for x in range(sampleSize):
            index = indices[i]
            total_diff += diffs[index]
            i+=1
        sample_avg = total_diff/float(sampleSize)
        mean_diffs.append(sample_avg)

    #normality check
    nt = stats.normaltest(mean_diffs)
    results['normal_p'] =  format(round(nt[1],4))

    #ttest
    t_prob = stats.ttest_1samp(mean_diffs, 0)
    results['ttest_t'] =  format(round(t_prob[0],4))
    results['ttest_p'] =  format(round(t_prob[1],4))

    #other stats
    results['avg_diff'] =  format(round(np.mean(diffs),4))
    results['numSamples'] = numSamples
    results['sampleSize'] = sampleSize
    results['num_pairs'] = len(pairs['tor'])

    return results
    

def getCostPairs(json_file):
    f = open(json_file, 'r')
    data = json.load(f)
    f.close()
    fArray = [ x for x in data if x['browser'] == 'Firefox']
    tArray = [ x for x in data if x['browser'] == 'Tor']
    pairCount = 0
    pairs = dict(tor=[], firefox=[])
    for tt in tArray:
        tt['paired'] = 0
    for ff in fArray:
        for tt in tArray:
            if (not tt['paired']):
                if isPaired(tt, ff):
                    tt['paired'] = 1
                    tPairs = pairs['tor']            
                    fPairs = pairs['firefox']
                    tPairs.append(tt['price'])
                    fPairs.append(ff['price'])
                    pairCount += 1
    return pairs

def isPaired(t, ff):
    if ff['date'] == t['date'] and withinMinutes(5, ff['time'], t['time'], '%H:%M:%S'):
        if ff['airlineCode'] == t['airlineCode'] and ff['flightNum'] == t['flightNum']:
            return True
    return False
    

def withinMinutes(minutes, tstr1, tstr2, tformat):
    seconds = minutes * 60
    t1 = datetime.strptime(tstr1, tformat)
    t2 = datetime.strptime(tstr2, tformat)
    diff = t1-t2
    diff_seconds = fabs(diff.total_seconds())
    if diff_seconds > seconds:
        return False
    else:
        return True

def dispStats(results):
    thresholds = [.1, .05, .01]
    print "RESULTS:"
    print "-------------------------------------------------"
    print "Number of pairs: ", results['num_pairs']
    print "Number of samples: ", results['numSamples']
    print "Sample size: ", results['sampleSize']
    print "Average difference: ", results['avg_diff']
    print "-------------------------------------------------"
    print "Normality test"
    print "\tp-value:", results['normal_p']
    for threshold in thresholds:
        if float(results['normal_p']) > threshold:
            pn = 'NORMAL'
        else:
            pn = 'NOT NORMAL'
        print '\tThreshold: ', threshold, '- ', pn
    print "-------------------------------------------------"
    print "Sample t-test"
    print "\tt-test p-value: ", results['ttest_p']
    print "\tt-test t-value: ", results['ttest_t']
    for threshold in thresholds:
        if float(results['ttest_p']) > float(threshold):
            pn = 'FAIL'
        else:
            pn = 'PASS'
        print '\tThreshold: ', threshold, '- ', pn

def plotPairs(pairs):
    count = 100
    firefox = pairs['firefox'][:count]
    tor = pairs['tor'][:count]
    numPairs = len(firefox)
    xs = range(1,numPairs+1)
    xs = range(1,count+1)
    plt.scatter(xs,firefox, c='b', marker='o', label = 'Without Tor')
    plt.scatter(xs,tor, c='r', marker='D', label = 'With Tor')
    plt.xlim(0,count)
    plt.ylim(310,350)
    plt.ylabel('Cost')
    plt.xlabel('Pair')
    plt.title('Paired Costs from Browsing  With and Without Tor')
    plt.legend('bottom right')
    plt.savefig('plots/pairs')

def plotDiffs(diffs):
    n = len(diffs)
    xs = range(1,n+1)
    plt.scatter(xs,diffs, c='r', marker='o')
    plt.xlim(0,n+1)
    plt.ylim(-30,30)
    plt.ylabel('Difference in Cost')
    plt.xlabel('Pair')
    plt.title('Obsered Cost Difference in Pairs (Tor-No Tor)')
    plt.savefig('plots/diffs')
    

if __name__ == '__main__':
    try:
        pairs = pickle.load(open("pairs.pkl", "r"))
    except:
        pairs = getCostPairs('cut-flight_output.json') 
        pickle.dump(pairs, open("pairs.pkl", "w"))
    results = pairedt(pairs,30)
    plotPairs(pairs)
    #dispStats(results)
    
    
     
