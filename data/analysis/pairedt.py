import json
import sys
import array
import numpy as np
import scipy.stats as stats
from math import fabs
from datetime import datetime
from pprint import pprint

#Needs two arrays to put into stats.ttest_rel(a,b)

def pairedt(pairs):
    #pairs = getCostPairs(json_file)
    fpairs = pairs['firefox']
    tpairs = pairs['tor']
    #results  holds various stats for both arrays
    results = dict(firefox = dict(), tor = dict())
    fstats = results['firefox']
    tstats = results['tor']

    #Normality check
    nt = stats.normaltest(fpairs)
    fstats['normal_p'] = nt[1]
    nt = stats.normaltest(tpairs)
    tstats['normal_p'] = nt[1]
    #Mean
    fstats['mean'] = np.mean(fpairs)
    tstats['mean'] = np.mean(tpairs)
    #ttest
    tt = stats.ttest_rel(fpairs, tpairs)
    results['ttest_p'] = tt[1]
    results['ttest_t'] = tt[0]
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
    print "Normality p-value:"
    print "\tFirefox- ", results['firefox']['normal_p']
    print "\tTor normality p-value- ", results['tor']['normal_p']
    print "-------------------------------------------------"
    print "Paired t-test"
    print "\tt-test p-value: ", results['ttest_p']
    print "\tt-test t-value: ", results['ttest_t']
    for threshold in thresholds:
        if results['ttest_p'] > threshold:
            pn = 'FAIL'
        else:
            pn = 'PASS'
        print '\tThreshold: ', threshold, '- ', pn
    print "-------------------------------------------------"
    print "Mean cost:"
    print "\tFirefox- ", results['firefox']['mean']
    print "\tTor normality p-value- ", results['tor']['mean']


if __name__ == '__main__':
    pairs = getCostPairs('cut-flight_output.json') 
    results = pairedt(pairs)
    dispStats(results)
    
    
     
