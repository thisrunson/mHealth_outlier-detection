#!/usr/bin/env python
# encoding: utf-8
"""
example.py

Created by Ben Birnbaum on 2012-12-02.
benjamin.birnbaum@gmail.com

Example use of outlierdetect.py module.
"""

from matplotlib import mlab
import outlierdetect
import pandas as pd


DATA_FILE = 'CHW_toydata.csv'


def print_scores(scores):
    for interviewer in scores.keys():
        print "%s" % interviewer
        for column in scores[interviewer].keys():
            print "\t%s:\t%.2f" % (column, scores[interviewer][column])
    

if __name__ == '__main__':
    data = pd.read_csv(DATA_FILE)  # Uncomment to load as pandas.DataFrame.
    # data = mlab.csv2rec(DATA_FILE)  # Uncomment to load as numpy.recarray.

    # Compute SVA outlier scores    #Uncomment to run SVA in addition to MMA
    #sva_scores = outlierdetect.run_sva(data, 'interviewer_id', ['cough', 'fever'])
    #print "SVA outlier scores"
    #print_scores(sva_scores)

    # Compute MMA outlier scores.  Will work only if scipy is installed.
    mma_scores = outlierdetect.run_mma(data, 'interviewer_ID', ['blood_pressure', 'abdominal_exam', 'ifa_tablets_issued'])
    print "\nMMA outlier scores"
    print_scores(mma_scores)
