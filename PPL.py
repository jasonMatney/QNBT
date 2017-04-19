# MakeXYLayer.py
# Description: Creates an XY layer and exports it to a layer file

# import system modules
import arcpy
import os.path
import os
from arcpy import env

# Set environment settings
env.workspace = "C:\\Users\\jamatney\\Desktop\\PhD\\PPL_reservationdata_csv\\PPL_binned_years.gdb"
arcpy.env.overwriteOutput = True

try:
    # Set the local variables
    tbls = arcpy.ListTables()
    for tbl in tbls:

         x_coords = "parklon"
         y_coords = "parklat"
         out_Layer = os.path.splitext(tbl)[0] + "_layer"
         saved_Layer = os.path.splitext(tbl)[0]
         out_Shapefile = os.path.splitext(tbl)[0] + ".shp"
         outWorkspace = env.workspace
         shpWorkspace = "C:\\Users\\jamatney\\Desktop\\PhD\\PPL_reservationdata_csv"
         outFeatureClass = os.path.splitext(tbl)[0]

         # Set the spatial reference
         spRef = arcpy.SpatialReference(4326)

         # Make the XY event layer...
         arcpy.MakeXYEventLayer_management(tbl, x_coords, y_coords, out_Layer, spRef)

         # Save to a layer file
         arcpy.SaveToLayerFile_management(out_Layer, saved_Layer)

         saved_Layer = saved_Layer + ".lyr"

         # Copy features to shapefile
         arcpy.CopyFeatures_management(saved_Layer, out_Shapefile)


except:
    # If an error occurred print the message to the screen
    print arcpy.GetMessages()
