// POSITIVE CELL DETECTION SCRIPT 1 (PCDS1): E. GRANDIS
// Keret et al. - An open-source machine-learning approach for obtaining high-quality quantitative wood anatomy data from E. grandis and P. radiata xylem.
// Composer: Mr. Rafael Keret.  
// Code was modified from "petebankhead" and is available on GitHub (https://github.com/qupath/qupath-docs/blob/0.3/docs/scripting/overview.rst). 
// QuPath software and code was originally developed by Bankhead et al., 2017. 

// (1) Set up the image type and pixel size

setImageType('OTHER');
setPixelSizeMicrons(0.24, 0.24)

// (2) Create a circular or ellipse region of interest (ROI)

import qupath. lib. roi. ROIs
import qupath. lib. regions. ImagePlane
import qupath. lib. objects. PathObjects

int z_slice = 0
int time_point = 0
def Image_P = ImagePlane.getPlane(z_slice, time_point)
def ROI = ROIs.createEllipseROI(240, 0.0, 1500, 1500, Image_P)
def ROI_object = PathObjects.createAnnotationObject(ROI)
addObject(ROI_object)

// (3) Selecting annotation or ROI, no need to specify as one was created per image

selectAnnotations()

// To analyse entire image use; 
// **createSelectAllObject(true)**

// (4) Positive cell detection parameters

runPlugin('qupath.imagej.detect.cells.PositiveCellDetection', '''{"detectionImage": "Green",  
"requestedPixelSizeMicrons": 0.24,  "backgroundRadiusMicrons": 26.5,  "medianRadiusMicrons": 0.0,  
"sigmaMicrons": 1.60,  "minAreaMicrons": 5.0,  "maxAreaMicrons": 6500.0,  "threshold": 0.0,  
"watershedPostProcess": false,  "cellExpansionMicrons": 2.2,  "includeNuclei": true,  
"smoothBoundaries": true,  "makeMeasurements": true,  "thresholdCompartment": 
"Cytoplasm: Green mean",  "thresholdPositive1": 10.0,  "thresholdPositive2": 20.0,  
"thresholdPositive3": 30.0,  "singleThreshold": true}''');

// (5) Removing the detections that intersect or come within 100 pixels of the defined ROI

import qupath.lib.regions.ImageRegion
import qupath.lib.objects.PathDetectionObject
import qupath.lib.objects.PathObject
import org.locationtech.jts.geom.util.LinearComponentExtracter
import java.util.stream.Collectors
import static qupath.lib.gui.scripting.QPEx.*

double distancePixels = 100.0  // Define distance from ROI, in pixels
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

runObjectClassifier("Path to Classifier1.json ")
