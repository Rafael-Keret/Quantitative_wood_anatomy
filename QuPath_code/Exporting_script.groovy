// SCRIPT FOR EXPORTING AND APPENDING QUPATH DATA
// Keret et al. - An open-source machine-learning approach for obtaining high-quality quantitative wood anatomy data from E. grandis and P. radiata xylem.  
// Composer: Mr. Rafael Keret.  
// Code was modified from "petebankhead" and is available on GitHub (https://github.com/qupath/qupath-docs/blob/0.3/docs/scripting/overview.rst). 
// QuPath software and code was originally developed by Bankhead et al., 2017.  

// (1) Importing relevant tools

import qupath.lib.objects.PathCellObject
import qupath.lib.gui.tools.MeasurementExporter

// (2) Define all images within the project

def QP_project = getProject()
def Project_images = project.getImageList()

// (3) Define the objects to export

def Object_type = PathCellObject.class

// (4) Define the columns to export

def Columns = new String[]{} // to specify columns: def columnsToInclude = new String[]{"Name", "Class", "Nucleus: Area"}

// (5) Specify the comma delimiter in the output csv file

def Delimiter = ","

// (6) Specify the location for the output csv file

def Path_data = "Destination of output .csv file"
def File_output = new File(Path_data)

// (7) Make the exporter by combining the functions and run

def exporter  = new MeasurementExporter()
          .imageList(Project_images)                 // Select images
            .exportType(Object_type)                 // Object types     
              .includeOnlyColumns(Columns)           // Selected columns
                .separator(Delimiter)                // Delimiter or separator
                  .exportMeasurements(File_output)   // Start

print "Done!"
