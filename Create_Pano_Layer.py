# Name: Buffer.py
# Description: Find areas of suitable vegetation which exclude areas heavily impacted by major roads

# import system modules
import arcpy
arcpy.env.overwriteOutput = True

# Set environment settings
arcpy.env.workspace = "F:\\Parks_Paper\\QNBT\\Chapter_2.gdb"
arcpy.MakeFeatureLayer_management("Pano_PUD","Pano_layer")

years=[u'2006', u'2007', u'2008', u'2009', u'2010', u'2011', u'2012', u'2013', u'2014']
months=[u'1', u'2',u'3', u'4', u'5', u'6', u'7', u'8', u'9', u'10', u'11', u'12']


for year in years:
    for month in months:
        query = "upload_month = {0}".format("'{0}'".format(month)) + " AND upload_year = {0}".format("'{0}'".format(year))
        filename = "Pano_MonthYear/Pano_" + month + '_' + year + '_PUD'
        print(query)
        print(filename)
        arcpy.SelectLayerByAttribute_management("Pano_layer", "NEW_SELECTION", query)
        arcpy.CopyFeatures_management("Pano_layer", filename)
