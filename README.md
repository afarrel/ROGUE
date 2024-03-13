# ROGUE
RNAseq &amp; Ontology Graphic User Environment

ROGUE (RNA-seq Ontology Graphic User Environment), is an R Shiny application that allows biologists to perform differentially expressed gene analysis, gene ontology and pathway enrichment analysis, potential biomarker identification, and advance  statistical analyses. 

**Notes:**

This app was written and tested in **R version 3.6**.

The below packages are required:

|          |              |            |       |        |
|----------|--------------|------------|-------|--------|
|shiny     |shinydashboard|shinyWidgets|shinyjs|markdown|
| edgeR |MASS|pheatmap|plotly|heatmaply|
| RColorBrewer|ggplot2|dplyr|Rtsne|scatterplot3d|
|gridExtra|gtools|colourpicker|grid|cowplot|
|fgsea  |reshape2|rintrojs|DESeq2|uwot|
|shinyalert|shinycssloaders||||





# 1. Download and Install R
https://www.r-project.org/

Note: This Rshiny app is written to be executed on UNIX and Linux. The application will need to be modified to be executed on Windows.

# 2. Download Code
1. Click on the Code button (top right of file list).

2. Click 'Download ZIP'.

3. Unzip ROGUE-main.zip.

# 3. Untar Gene Ontology Data
1. tar -zxf [PATH_TO_MAIN_ROGUE_DIRECTORY]/Data/Homo_Sapien.tar.gz --directory [PATH_TO_MAIN_ROGUE_DIRECTORY]/ROGUE-main/Data

	a) Example: (If the folder is in your Home directory's download folder)

		tar -zxf ~/Downloads/ROGUE-main/Data/Homo_Sapien.tar.gz --directory  ~/Downloads/ROGUE-main/Data


# 4. Install Necessary Packages

1. Run the file 'Install_Packages.R'

	a) Rscript [PATH_TO_MAIN_ROGUE_DIRECTORY]/Install_Packages.R

	b) (Alternatively you can load the 'Install_Packages.R' in RStudio and install packages one by one).
	
		Rscript ~/Downloads/ROGUE-main/Install_Packages.R

# 5. Run ROGUE
1.	Rscript -e "shiny::runApp('[PATH_TO_MAIN_ROGUE_DIRECTORY]',launch.browser=TRUE)"

	a)	Example: (If the folder is in your Home directory's download folder)
	
		Rscript -e "shiny::runApp('~/Downloads/ROGUE-main',launch.browser=TRUE)"
