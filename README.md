# ROGUE
RNAseq &amp; Ontology Graphic User Environment

ROGUE (RNA-seq Ontology Graphic User Environment), is an R Shiny application that allows biologists to perform differentially expressed gene analysis, gene ontology and pathway enrichment analysis, potential biomarker identification, and advanced statistical analyses. 


# 1. Download and Install R
https://www.r-project.org/

Note: This Rshiny app is written to be executed on UNIX and Linux. It will need to be modified to be executed on Windows.

# 2. Download Code
1. Click on download Code button.

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

# 5. Run ROGUE
1.	Rscript -e "shiny::runApp('[PATH_TO_MAIN_ROGUE_DIRECTORY]',launch.browser=TRUE)"

	a)	Example: (If the folder is in your Home directory's download folder)
	
		Rscript -e "shiny::runApp('~/Downloads/ROGUE-main',launch.browser=TRUE)"
