
import sys
import pdb
import numpy as np
import os.path
import glob
import argparse
import configparser

def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--binLDStimeSeriesINI", help="Bin LDS time series ini file", default="../../data/exampleMouse/binLDStimeSeries.ini")
    args = parser.parse_args()

    binLDStimeSeriesINI = args.binLDStimeSeriesINI

    binInitConfig = configparser.ConfigParser()
    binInitConfig.read(binLDStimeSeriesINI)
    boutTimesPath = binInitConfig["filenames"]["boutTimesPath"]
    globPathname = os.path.join(boutTimesPath, "*.npz")
    boutFilenames = glob.glob(globPathname)

    for boutFilename in boutFilenames:
        print(boutFilename)
        loadRes = np.load(boutFilename)
        for key in loadRes.keys():
            print(key)

if __name__=="__main__":
    main(sys.argv)
