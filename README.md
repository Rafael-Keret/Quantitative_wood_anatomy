# An open-source machine-learning approach for obtaining high-quality quantitative wood anatomy data from _E. grandis_ and _P. radiata_ xylem.

## Purpose of repository 

This repository represents a method to analyse wood microsections of _E. grandis_ and _P. radiata_ using the bioimage analysis software QuPath (v0.4.4). Consequently, the relevant code for generating and exporting cell detections in QuPath are included. The automated quantitative wood anatomy (QWA) data produced in QuPath can be assessed for accuracy/reproducibility via the code available in the R scripts. 

## Contents (code)

1. PCDS1.groovy
2. PCDS2.groovy
3. Classifier1.json
4. Classifier2.json
5. Exporting_script.groovy
6. Eucalyptus_QuPath.R
7. Pine_QuPath.R

## Access to microsection scans for QuPath methods development

### Training images

_Eucalyptus grandis_ - https://doi.org/10.5281/zenodo.8006449
_Pinus radiata_ - https://doi.org/10.5281/zenodo.8006687

### Testing images

_Eucalyptus grandis_ - https://doi.org/10.5281/zenodo.8006637
_Pinus radiata_ - https://doi.org/10.5281/zenodo.8006705

## Application of the QuPath groovy scripts and classifiers to generate QWA data

### Positive cell detection in QuPath

1. Download QuPath (v0.4.4) quantitative pathology & bioimage analysis software (Bankhead et al., 2017) from https://qupath.github.io. 
2.	Select the “Create project” icon and select the relevant directory for which a QuPath project will be created.
   NOTE: QuPath projects can only be created in an empty directory. 
4.	Select the “Add images” icon and change the “Set image type” to “Other”. Leave all the other setting as default. 
5.	Press “Choose files” and “Import” the relevant images. 
6.	Select “Automate” from the toolbar, and from the drop-down menu choose “Show script editor”. 
7.	In “Script Editor” navigate to “File” and select “Open” from the drop-down menu. Navigate to and select either PCDS1.groovy (_E. grandis_) or PCDS2.groovy (_P. radiata_). 
    NOTE: This script is the positive cell detection and subsetting script. Make sure that the file path for the classifier (_E. grandis_ - Classifier1.json or _P. radiata_ - Classifier2.json) is set in the code at line 88.  
8.	Navigate to “Run”, select “Run for project” and choose all the relevant images to be analyzed.
    NOTE: The cell detection and subsetting parameters have been optimized for _Eucalyptus grandis_ and _Pinus radiata_ stem sections.  
9.	Repeat steps 6 and 7 with the file titled Exporting_script.groovy, and select “Run” to export the datafile as a .csv. 
    NOTE: Change the file directory (line 31) and filename to be saved. 
    
#### Notice

The code titled PCDS1.groovy and PCDS2.groovy has been specifically tailored to meet the analytical requirements of the _E. grandis_ and _P. radiata_ wood anatomy images. Hence the parameters such as image type (line 9), pixel size (line 10) and region of interest (lines 12-23) will require modification by the end user for correct operations (i.e. accurate detection measurements). Additionally, the Classifier1.json and Classifier2.json used for cell subsetting will need to be downloaded, and the directory in which the file is saved on the local pc must be specified in the PCDS1.groovy or PCDS2.groovy code (line 88). In the exporting script, the output destination of the .csv file requires specification (line 31). 
