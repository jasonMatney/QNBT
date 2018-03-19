import arcpy
import os

arcpy.env.overwriteOutput = True
arcpy.env.workspace = "F:/Parks_Paper/QNBT/Tidy.gdb"

def distinct_values(table, field):
    """ Return distinct value content in field to list """
    with arcpy.da.SearchCursor(table, [field]) as cursor:
        return sorted({row[0] for row in cursor if row[0]})  # No blank data

# Set environment settings
# Set local variables
workspace = arcpy.env.workspace

NPS_fc = os.path.join(workspace, "QueryBuilder")
arcpy.MakeFeatureLayer_management("NPS_Contiguous", "NPS_Boundary")
arcpy.MakeFeatureLayer_management(NPS_fc, "NPS_lyr")

month = [1,2,3,4,5,6,7,8,9,10,11,12]
year = [2006,2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014]

for y in year:
    for m in month:
        print "Processing - Year: ", y, " Month: ", m
        Panoramio = os.path.join(workspace, "Panoramio")
        Pano_where = '"Pano_Year" = '+ "'%s'" %y + ' And "Pano_Month" = ' + "'%s'" %m
        arcpy.Select_analysis(Panoramio, "Pano_selection", Pano_where)
        print "Selection complete using: ", Pano_where

        Pano_selection = os.path.join(workspace, "Pano_selection")
        Spatial_Join = os.path.join(workspace, "Spatial_Join")

        arcpy.SpatialJoin_analysis("NPS_Boundary", Pano_selection, Spatial_Join,
                                   "JOIN_ONE_TO_ONE", "KEEP_ALL",
                                   "UNIT_CODE \"UNIT_CODE\" "
                                   "true true false 16 Text 0 0 ,First,"
                                   "#,NPS_Boundary,UNIT_CODE,-1,"
                                   "-1;UNIT_NAME \"UNIT_NAME\" true true false "
                                   "254 Text 0 0 ,First,#,NPS_Boundary,UNIT_NAME,-1,-1;"
                                   "STATE \"STATE\" true true false 2 Text 0 0 ,First,#,NPS_Boundary,STATE,"
                                   "-1,-1;REGION \"REGION\" true true false 2 Text 0 0 ,First,#,NPS_Boundary,"
                                   "REGION,-1,-1;UNIT_TYPE \"UNIT_TYPE\" true true false 50 Text 0 0 ,First,#,"
                                   "NPS_Boundary,UNIT_TYPE,-1,-1;SHAPE_Length \"SHAPE_Length\" false true true "
                                   "8 Double 0 0 ,First,#,NPS_Boundary,SHAPE_Length,-1,-1;SHAPE_Area \"SHAPE_Area\" "
                                   "false true true 8 Double 0 0 ,First,#,NPS_Boundary,SHAPE_Area,-1,-1;Count \"Count\" "
                                   "true true false 4 Long 0 0 ,Sum,#,Panoramio,Count,-1,-1;upload_date \"date\" "
                                   "true true false 8 Date 0 0 ,First,#,Panoramio,upload_date,-1,-1;Pano_Month \"Pano_Month\" "
                                   "true true false 50 Text 0 0 ,First,#,Panoramio,Pano_Month,-1,-1;Pano_Year \"Pano_Year\" "
                                   "true true false 50 Text 0 0 ,First,#,Panoramio,Pano_Year,-1,-1",
                                   "INTERSECT", "", "")
        print Pano_where, " Spatial Join complete"

        arcpy.MakeFeatureLayer_management(Spatial_Join, "SJ_lyr")
        unit_code = distinct_values(Spatial_Join, "UNIT_CODE")

        for u in unit_code:
            NPS_where = """ "UnitCode" = '%s'"""%u + ' And "NPS_Year" = ' + str(y) + ' And "NPS_Month" = ' + str(m)
            SJ_where = """ "UNIT_CODE" = '%s'""" % u

            arcpy.SelectLayerByAttribute_management("NPS_lyr", "NEW_SELECTION", NPS_where)
            arcpy.Select_analysis("SJ_lyr", "SJ_selection", SJ_where)

            print "Selection complete using: ", SJ_where

            #SJ_selection = arcpy.SelectLayerByAttribute_management("SJ_lyr", "NEW_SELECTION", SJ_where)
            SJ_field = "Count"

            SJ_cursor = arcpy.SearchCursor("SJ_selection")
            for row in SJ_cursor:
                a = row.getValue(SJ_field)


            NPS_field = "Panoramio_Count"
            NPS_cursor = arcpy.da.UpdateCursor(NPS_fc, NPS_field, where_clause=NPS_where)
            for row in NPS_cursor:
                print row
                if row[0] == None:
                    row[0] = a
                    NPS_cursor.updateRow(row)
                    print "Data updated: ", NPS_where, " now has value: ", row
                else:
                    print "!Error! with: ", NPS_where

del SJ_cursor, NPS_cursor, row
