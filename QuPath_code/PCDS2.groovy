// POSITIVE CELL DETECTION SCRIPT 2 (PCDS2): P. RADIATA
// Keret et al. - An open-source machine-learning approach for obtaining high-quality quantitative wood anatomy data from E. grandis and P. radiata xylem.
// Composer: Mr. Rafael Keret.  
// Code was modified from "petebankhead" and is available on GitHub (https://github.com/qupath/qupath-docs/blob/0.3/docs/scripting/overview.rst). 
// QuPath software and code was originally developed by Bankhead et al., 2017.  

// (1) Set up the image type and pixel size

setImageType('OTHER');
setPixelSizeMicrons(0.48, 0.48)

// (2) Create a rectangular region of interest (ROI)

import qupath. lib. roi. ROIs
import qupath. lib. regions. ImagePlane
import qupath. lib. objects. PathObjects

int z_slice = 0
int time_point = 0
def Image_P = ImagePlane.getPlane(z_slice, time_point)
def ROI = ROIs.createRectangleROI(0.0, 0.0, 600, 3500, Image_P)
def ROI_object = PathObjects.createAnnotationObject(ROI)
addObject(ROI_object)

// (3) Selecting annotation or ROI, no need to specify as one was created per image

selectAnnotations()

// To analyse entire image use; 
// **createSelectAllObject(true)**

// (4) Positive cell detection parameters

runPlugin('qupath.imagej.detect.cells.PositiveCellDetection', '''{"detectionImage": "Green",  
"requestedPixelSizeMicrons": 0.48,  "backgroundRadiusMicrons": 40.0,  "medianRadiusMicrons": 3.0,  
"sigmaMicrons": 1.0,  "minAreaMicrons": 30.0,  "maxAreaMicrons": 6500.0,  "threshold": 75.0,  
"watershedPostProcess": false,  "cellExpansionMicrons": 10.0,  "includeNuclei": true,  
"smoothBoundaries": true,  "makeMeasurements": true,  "thresholdCompartment": 
"Cytoplasm: Green mean",  "thresholdPositive1": 10.0,  "thresholdPositive2": 20.0,  
"thresholdPositive3": 30.0,  "singleThreshold": true}''');

// (5) Removing the detections that intersect or come within 5 pixels of the defined ROI

import qupath.lib.regions.ImageRegion
import qupath.lib.objects.PathDetectionObject
import qupath.lib.objects.PathObject
import org.locationtech.jts.geom.util.LinearComponentExtracter
import java.util.stream.Collectors
import static qupath.lib.gui.scripting.QPEx.*

double distancePixels = 5.0  // Define distance from ROI, in pixels
boolean useHierarchyRule = false // Hierarchy is not required as only one ROI or object is used

// (6) Retrieve ROI or parent annotations

def hierarchy = getCurrentHierarchy()
def annotations = hierarchy.getAnnotationObjects()

// (7) Loop through images in project, and collect the plant cell detections

def toRemove = new HashSet<PathObject>()
for (def annotation in annotations) {
    def roi = annotation.getROI()
    if (roi == null)
        continue
    Collection<? extends PathObject> detections
    if (useHierarchyRule)
        detections = hierarchy.getObjectsForRegion(PathDetectionObject.class, ImageRegion.createInstance(roi), null)
    else
        detections = hierarchy.getObjectsForROI(PathDetectionObject.class, roi)
    def geometry = roi.getGeometry()
    for (def line in LinearComponentExtracter.getLines(geometry)) {
        toRemove.addAll(
                detections.parallelStream()
                        .filter(d -> line.isWithinDistance(d.getROI().getGeometry(), distancePixels))
                        .collect(Collectors.toList())
        )
    }
}
println '''Removing ${toRemove.size()} detections without 
${distancePixels} pixels of an annotation boundary'''

hierarchy.removeObjects(toRemove, false) // Not necessary to apply, as only one object per image

// (8) Apply the trained random trees classifier to the project to subset cells into groups
// Specify absolute path to the ".json" classifier 

runObjectClassifier("Path to Classifier2.json ")
