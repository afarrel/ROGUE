# ROGUE
RNAseq &amp; Ontology Graphic User Environment

ROGUE (RNA-seq Ontology Graphic User Environment), is an R Shiny application that allows biologists to perform differentially expressed gene analysis, gene ontology and pathway enrichment analysis, potential biomarker identification, and advanced statistical analyses. 


Click on download Code button.
Click Download ZIP
Unzip ROGUE-main.zip


tar -zxf [PATH_TO_MAIN_ROGUE_DIRECTORY]/Data/Homo_Sapien.tar.gz --directory  [PATH_TO_MAIN_ROGUE_DIRECTORY]/ROGUE-main/Data
**#Untar Gene Ontology Data**
Example: (If the folder is in your Home directory's download folder)
tar -zxf ~/Downloads/ROGUE-main/Data/Homo_Sapien.tar.gz --directory  ~/Downloads/ROGUE-main/Data

#Install Necessary Packages
Run the file 'Install_Packages.R'
Rscript [PATH_TO_MAIN_ROGUE_DIRECTORY]/Install_Packages.R
(Alternatively you can load the 'Install_Packages.R' in RStudio and install packages one by one).

**#Run ROGUE**
Rscript -e "shiny::runApp('[PATH_TO_MAIN_ROGUE_DIRECTORY]',launch.browser=TRUE)"

Example: (If the folder is in your Home directory's download folder)
Rscript -e "shiny::runApp('~/Downloads/ROGUE-main',launch.browser=TRUE)"
