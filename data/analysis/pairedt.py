import json
import sys
from math import fabs
from datetime import datetime
from scipy import stats
from pprint import pprint

#Needs two arrays to put into stats.ttest_rel(a,b)

def pairedt(json_file):
    f = open(json_file, 'r')
    data = json.load(f)
    f.close()
    fArray = [ x for x in data if x['browser'] == 'Firefox']
    tArray = [ x for x in data if x['browser'] == 'Tor']
    pairCount = 0
    for tt in tArray:
        tt['paired'] = 0
    for ff in fArray:
        for tt in tArray:
            if (not tt['paired']):
                if ff['date'] == tt['date'] and withinMinutes(5, ff['time'], tt['time'], '%H:%M:%S'):
                    if ff['airlineCode'] == tt['airlineCode'] and ff['flightNum'] == tt['flightNum']:
                        tt['paired'] = 1
                        pairCount += 1

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
    

if __name__ == '__main__':
    pairedt('edited-flight_output.json') 
    
     
