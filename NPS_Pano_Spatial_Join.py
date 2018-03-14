import arcpy
import os
arcpy.env.overwriteOutput = True
arcpy.env.workspace = "F:\\Parks_Paper\\QNBT\\Chapter_2.gdb"
# Set environment settings
# Set local variables
workspace = r"F:\\Parks_Paper\\QNBT\\Chapter_2.gdb"
outWorkspace = r"F:\\Parks_Paper\\QNBT\\Chapter_2.gdb\\SpatialJoin"

pano_fcs = arcpy.ListFeatureClasses(feature_dataset="Pano_MonthYear")
nps_lyr = arcpy.MakeFeatureLayer_management("NPS_Boundaries_With_Estimates_within_US", "NPS_Estimates_Layer")
nps_fields = arcpy.ListFields(nps_lyr, "F*")

targetFeatures = os.path.join(workspace, "NPS_Boundaries_With_Estimates_within_US")

# Differentiate between index and value
for idx, val in enumerate(pano_fcs):
     joinFeatures = os.path.join(workspace, val)
     print("Processing file: " + joinFeatures.lower())

     filename = "NPS_Join_" + val
     outfc = os.path.join(outWorkspace, filename)

     # # Create a new fieldmappings and add the two input feature classes.
     fieldmappings = arcpy.FieldMappings()
     fieldmappings.addTable(targetFeatures)
     fieldmappings.addTable(joinFeatures)

     CountFieldIndex = fieldmappings.findFieldMapIndex("Count")
     fieldmap = fieldmappings.getFieldMap(CountFieldIndex)

     # Get the output field's properties as a field object
     field = fieldmap.outputField

     # Rename the field and pass the updated field object back into the field map
     field.name = "sum_pano_count"
     field.aliasName = "panoramio sum"
     fieldmap.outputField = field

     # Set the merge rule to mean and then replace the old fieldmap in the mappings object
     # with the updated one
     fieldmap.mergeRule = "sum"
     fieldmappings.replaceFieldMap(CountFieldIndex, fieldmap)


     keepers = ["UNIT_CODE", "UNIT_NAME", nps_fields[idx].name, "sum_pano_count", "SHAPE_Length", "SHAPE_Area"]  # etc.
     for field in fieldmappings.fields:
              if field.name not in keepers:
                 fieldmappings.removeFieldMap(fieldmappings.findFieldMapIndex(field.name))
     #
     # Run the Spatial Join tool, using the defaults for the join operation and join type
     arcpy.SpatialJoin_analysis(targetFeatures, joinFeatures, outfc, "#", "#", fieldmappings)
     print("Join Complete")
