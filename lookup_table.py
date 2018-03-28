import arcpy
import csv
import os
import numpy as np
from datetime import date


arcpy.env.workspace = "F:\\Parks_Paper\\QNBT\\Aggregation.gdb"

tbl = arcpy.ListTables("*")[0]
fieldList = arcpy.ListFields(tbl)

def unique_values(table, field):
    with arcpy.da.SearchCursor(table, [field]) as cursor:
        return sorted({row[0] for row in cursor})

print "getting unique parks..."
with open('parks.txt') as f:
    parks = f.read().splitlines()
print "unique parks received..."

years = [2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016]
months =[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]


filename = 'F:\\Parks_Paper\\QNBT\\Analysis\\lookup.csv'
try:
    os.remove(filename)
except OSError:
    pass

dataCSV = open(filename, 'wb')
writer = csv.writer(dataCSV, dialect="excel")
writer.writerow(['Park', 'Year', 'Month', 'parklat', 'parklon',
                 'qnum25', 'qnum50', 'qnum75',
                 'qldt25', 'qldt50', 'qldt75',
                 'qdur25', 'qdur50', 'qdur75',
                 'qdis25', 'qdis50', 'qdis75',
                 'numres', 'sumdur', 'persnight'])

for park in parks:
    for year in years:
        for month in months:
            print "Processing.... \n Park: {0} \n Year: {1} \n Month: {2}".format(park, year, month)
            where_clause = """"Park" = '{0}' AND "Year" = {1} AND "Month" = {2}""".format(park, year, month)

            parkLatList = []
            parkLongList = []
            numPplList = []
            qnumList = []
            qldtList = []
            qdurList = []
            qdisList = []
            numResList = []
            sumDurList = []
            persNightList = []
            rowCount = 0

            with arcpy.da.SearchCursor(tbl, ("*"), where_clause) as cursor:
                for row in cursor:

                    numPplList.append(int(row[12]))
                    parkLatList.append(float(row[15]))
                    parkLongList.append(float(row[16]))
                    qnumList.append(int(row[12]))
                    qldtList.append(int(row[13]))
                    qdurList.append(int(row[14]))
                    qdisList.append(float(row[19]))
                    numResList.append(int(len(row)))
                    sumDurList.append(int(row[14]))
                    rowCount += 1

                if not parkLatList:
                    print "park {0} year {1} month {2} is empty".format(park, year, month)
                else:
                    parkLat = parkLatList[0]
                    parkLong = parkLongList[0]
                    qnum25 = float(np.percentile(qnumList, 25))
                    qnum50 = float(np.percentile(qnumList, 50))
                    qnum75 = float(np.percentile(qnumList, 75))
                    qldt25 = float(np.percentile(qldtList, 25))
                    qldt50 = float(np.percentile(qldtList, 50))
                    qldt75 = float(np.percentile(qldtList, 75))
                    qdur25 = float(np.percentile(qdurList, 25))
                    qdur50 = float(np.percentile(qdurList, 50))
                    qdur75 = float(np.percentile(qdurList, 75))
                    qdis25 = float(np.percentile(qdisList, 25))
                    qdis50 = float(np.percentile(qdisList, 50))
                    qdis75 = float(np.percentile(qdisList, 75))
                    numRes = rowCount
                    sumDur = int(np.sum(sumDurList))
                    persNight = int(np.sum(sumDurList)) * int(np.sum(numPplList))

                    writer.writerow([park, year, month,
                                     parkLat, parkLong,
                                     qnum25, qnum50, qnum75,
                                     qldt25, qldt50, qldt75,
                                     qdur25, qdur50, qdur75,
                                     qdis25, qdis50, qdis75,
                                     numRes, sumDur, persNight])

                    del parkLatList
                    del parkLongList
                    del numPplList
                    del qnumList
                    del qldtList
                    del qdurList
                    del qdisList
                    del numResList
                    del sumDurList
                    del persNightList

f.close()
