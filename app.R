
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(markdown))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(pheatmap))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(edgeR))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(heatmaply))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(require(gtools))
suppressPackageStartupMessages(library(colourpicker))
suppressPackageStartupMessages(library(Rtsne))
suppressPackageStartupMessages(library(scatterplot3d))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(cowplot))
#suppressPackageStartupMessages(library(fgsea))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(rintrojs))
#suppressWarnings(suppressPackageStartupMessages(library(DESeq2)))
suppressPackageStartupMessages(library(uwot))
suppressPackageStartupMessages(library(shinyalert))


options(shiny.maxRequestSize=500*1024^2)  # Limits file upload size to 500 MB

READS_FILE_LIST = c()
READ_TABLE_DATA_added = c()

RPKM_FILE_LIST = c()
RPKM_TABLE_DATA_added = c()

Data.folder = "Data/FPKM/"
Reads.Data.folder = "Data/READS/"
Ontology.folder = "Data/Homo_Sapien/"

Backup.folder = "Backup/"

load("Data/ROGUE_Base_Workspace.RData")

#GENE_ID_Match = read.delim("Data/Gene_ID_Matches.txt")
#GENE_LENGTHS = read.delim("Data/GENE_Lengths.txt")



Data.folder.files = list.files(Data.folder)
Data_File_path = ""
Group_Count=0
Reads.Data.folder.files = list.files(Reads.Data.folder)
Reads.Data_File_path = ""

Color.Select.List = c()
ALL.COLORS = colors()

Report.List.Reads = list()



#########################################
### ########### ROGUE GUI ###########  ###
#########################################

  load_data <- function() {
    Sys.sleep(0.2)
    hide("loading_page")
    show("main_content")
  }
  

sidebar <- dashboardSidebar(width = 250,

fluidRow(column(width=8,shiny::actionButton(inputId='ab1', label="View ROGUE Manual",
                       icon = icon("th"),style="color: #B2C8DB; background-color: #576570; border-color: #B2C8DB; padding:1px;",
                          onclick ="window.open('Instructions.pdf', '_blank')")),column(width=2,actionButton("ROGUE_help", "", icon = icon("question"),class = "btn-xs", title = "Get Started"))),
				hr(),
				sidebarMenu(id="TABS",width=5,
					menuItem("ROGUE", tabName="rogue",icon=icon("line-chart"),selected=TRUE),
					menuItem("Load Count Data:(EdgeR/DEseq2)", tabName="Count_EdgeR_Data", icon=icon("line-chart")),
					menuItem("Load Expression Data", tabName="Expression_Data",  icon=icon("line-chart")),				
					menuItem("Create Groups", tabName="Create_Groups",  icon=icon("line-chart")),
					menuItem("EdgeR/DEseq2 Group Comparison", tabName="Groups_CountData", icon=icon("line-chart")),
					menuItem("Gene Comparison (Samples)", tabName="Sample_Comparison", icon=icon("line-chart")),
					menuItem("Gene Comparison (Groups)", tabName="Group_Comparison",  icon=icon("line-chart")),
					menuItem("Advanced Analysis",   icon=icon("line-chart"),
						menuSubItem("Gene Set Entrichment Analysis", tabName="GSEA", icon=icon("line-chart")),
						menuSubItem("Gene Ontology",tabName="Gene_Ontology", icon=icon("line-chart")),
						menuSubItem("Group Statistical Comparison",tabName="Group_Stats_Comparison", icon=icon("line-chart")),
						menuSubItem("Group Expr. Ontologies",tabName="Expressed_Ontologies", icon=icon("line-chart")),
						menuSubItem("Differentially Expressed Ontologies",tabName="Diff_Expressed_Ontologies", icon=icon("line-chart"))
					),#menuItem("Advanced Analysis",   icon=icon("line-chart"),
				   hr(),	
					menuItem("Restore/Load Session", tabName="Session_Info", icon=icon("refresh")),
				   downloadButton(outputId = "Download_Summary_Plots",label = "Download Report",class = "Download_Report"),
				  tags$head(tags$style(".Download_Report{background-color:#1A3333;} .Download_Report{color: #1A3333;} .Download_Report{border-color: #1A3333;} .Download_Report{float: center;} ")),
					p(),
					textOutput(outputId = "Current_Session_ID_Main", inline=T),
					tags$head(tags$style("#Current_Session_ID_Main{color: grey;font-size: 11px;font-style: italic;}"))
				  
				
												
				)#sidebarMenu(id="TABS",
)#sidebar <- dashboardSidebar(

#.left-side, .main-sidebar {padding-top: 100px}
body <- dashboardBody(introjsUI(),useShinyjs(),
			tags$head(tags$style(HTML('
			
				left-side, .main-sidebar {padding-top: 75px}

			
				/* logo */
              .skin-blue .main-header .logo {
              background-color: #ffffff;
              }
			
			
				/* body */
				.content-wrapper, .right-side {
					background-color: #ffffff;
					}
			
			'))),


	
		    ######################################
			    ######################################
			    ############# ROGUE ##################
			    ######################################
			    ######################################
	
	
			tabItems(
			    tabItem(tabName = "rogue",
			      
			      fluidRow(column(width=1),(column(width=6,
			      img(src='ROGUE_Map.png', align = "center", width = "1010", height="500" )
			      )))
			    		
			    ),#tabItem(tabName = "rogue",
			    
			    ######################################
			    ######################################
			    #### Load Count data:(EdgeR) #########
			    ######################################
			    ######################################
			    
			    
			  
			    
				tabItem(tabName = "Count_EdgeR_Data",
				
				  fluidRow(

    box(
      title = p(actionButton("Counts_EdgeR_help", "", icon = icon("question"),
                  class = "btn-xs", title = "Help"),"Load Raw Reads and Perform Differentially Expressed Gene Analysis" 
                
      ), 
      width = 10, solidHeader = TRUE, 
      uiOutput("boxContentUI2"),
				
				
				sidebarLayout(
                   sidebarPanel(
                   
                     h5( radioButtons(inputId = "Reads.Select_input",label = "Select Input Source",choices = c("Upload File"="upload","Database"="dbase"),selected = "upload"),id = "h4ReadsSelectInput"),
                     
                     hidden(
                      	div(id = "file1_Box_wrapper",
                      		box(id = "file1_Box", title = "Select a counts/reads file", width = '200px',                        		                   
    		                  		h5(
                        				fileInput("file1", "Choose Reads File",
                               					accept = c(
                                 				"text/csv",
                                 				"text/comma-separated-values,text/plain",
                                 				".csv")#accept = c(
                      						),  #fileInput("file1", "Choose Reads File",
                      					id = "h4file1")
                      ))),#hidden(div(id = "file1_Box_wrapper"
                      
                      hidden(
                      	div(id = "Sample_Select_Box_wrapper",
                      		box(id = "Sample_Select_Box", title = "Sample Selection", width = '200px',                      
    		                  		h5(id="h4Sample_Select_Box", selectizeInput(inputId = "Reads.Library_select",label = "Select Samples",choices = c(""), selected = NULL, multiple = T))
                      ))),#hidden(div(id = "Sample_Select_Box_wrapper"
                      
                      hidden(
                      	div(id = "Reads_Database_Box_wrapper",
                      		box(id = "Sample_Reads_Database_Box", title = "Select a counts/reads file", width = '200px',   
                   
                    	 		h5(id = "h4ReadsDataUser",selectInput(inputId="Reads.Data_User", label="Select Data Library", c(Reads.Data.folder.files), selected = NULL, multiple = F)),                     
                    	 		h5(id = "h4ReadsDataset",selectInput(inputId="Reads.Data_set", label="Select Dataset", c(""), selected = NULL, multiple = F))
                     
                     ))),#hidden(div(id = "Reads_Database_Box_wrapper"
                     
                     h5(id = "h4ReadsRawNorm",radioButtons(inputId = "Reads.Raw_Norm",label = "Select Reads Status",choices = c("Raw"="Raw","Normalized"="Normalized"),selected = "Raw",inline = T)),         
                     h5(id = "h4ReadsLoadData",actionButton(inputId="Reads.Load_Data", label="Load Data")),
                     #h5(id = "h4AddData",fluidRow(
                     #  column(width=3,checkboxInput(inputId="Reads.Add_Data_Check", label="Add data", FALSE)),column(width=2,actionButton(inputId="Reads.Add_Data", label="Add File")),column(width=2,actionButton(inputId="Reads.Reset_Add_Data", label="Reset")))),
                     textOutput("Reads.User_Data_File"),
                     textOutput("Reads.User_Data_Info"),
                     #tags$hr(),
                     #checkboxInput("header", "Header", TRUE),
                     
                     hidden(
                      	div(id = "Comparison_Box_wrapper",
                      		box(id = "Comparison_Box", title = "Sample Selection (Max 2)", width = '200px',    
                     
                     			h5(id = "h4Comparison",selectInput(inputId="Comparison", label="Select 2 Samples", c("No Samples Loaded"), multiple = T)),
                     			h5(id = "h4EdgeR_DEseq",radioButtons(inputId="Reads.EdgeR_DEseq",label = "Select Method",choices = c("EdgeR"="EdgeR","DESeq2"="DESeq2"),selected = "EdgeR",inline = T)),
                     			h5(id = "h4CompareButton",actionButton(inputId="CompareButton", label="Compare Samples")),
                     			h5(id = "h4Log2FCThreshold",textInput(inputId="Log2FCThreshold", label="Log2FC", value = "1", width = '60px', placeholder = NULL)),
                     			h5(id = "h4FDRPvalThreshold",textInput(inputId="FDR_PvalThreshold", label="P-value", value = "0.05", width = '60px', placeholder = NULL)),
                     			h5(id = "h4PvalCorrection",selectInput(inputId="Pval_Correction", label="P-value correction method", c("none","BH", "fdr","BY","holm"), selected = "none", multiple = F)),                                       
		     					h5(id = "h4ReadsPlotWidths",sliderInput(inputId = "Reads.Plot_Widths",label = "Plot Widths",min =1 ,max = 10,value = 10)),
		    				 		h5(id = "h4ReadsVolcanoGenes",sliderInput(inputId = "Reads.Volcano_Genes",label = "Show Volcano Plot Genes",min =0 ,max = 200,value = 20)),
		     					h5(id = "h4ReadsVolcanoFontSize",sliderInput(inputId = "Reads.Volcano_Font_Size",label = "Volcano Plot Font Size",min = 0,max = 10,value = 2)),
		     					h5(id = "h4ReadsHeatmapFontSize",sliderInput(inputId = "Reads.Heatmap_Font_Size",label = "Heatmap Font Size",min = 0,max = 15,value = 5)),
		     					h5(id = "h4ReadsHeatmapSamples",radioButtons(inputId = "Reads.Heatmap_Samples",label = "Heatmap Samples",choices = c("Compared Samples","All Samples"),inline = T,selected = "Compared Samples"))
	 				)))#hidden(div(id = "Comparison_Box_wrapper"
                  ),#sidebarPanel(
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Summary_plots", 
                                hidden(div(id="MDSPlot_div",shinycssloaders::withSpinner(plotlyOutput("MDSPlot",height = "600px",width = "600px")))),
                                hidden(div(id="ComparePlot1_div",shinycssloaders::withSpinner(plotlyOutput("ComparePlot1",height = "500px",width = "500px")))),
                                hidden(div(id="ComparePlot2_div",shinycssloaders::withSpinner(plotOutput("ComparePlot2")))),
                                hidden(div(id="ComparePlot3_div",(plotOutput("ComparePlot3")))), #removed shinycssloaders
                                hidden(div(id="ComparePlot4_div",(plotOutput("ComparePlot4")))), #removed shinycssloaders
                                hidden(div(id="ComparePlot5_div",(plotOutput("ComparePlot5")))), #removed shinycssloaders
                                hidden(div(id="ComparePlot6_div",(plotOutput("ComparePlot6")))), #removed shinycssloaders
                                plotOutput("ComparePlot7"),
                                plotOutput("ComparePlot8")),
                       tabPanel("GeneLists",
                                textAreaInput(inputId="GeneList_Result", label = "Gene List", value = NULL,height = '400px'),
				downloadButton(outputId = "FC_List_Download",label = "Download DE Genes"),
                                downloadButton(outputId = "FC_Download",label = "Download FC Table"),
                                downloadButton(outputId = "FC_GeneList",label = "Download RPKM Table"),
                                textAreaInput(inputId="GeneList_Result_UP", label = "Upregulated Gene List", value = NULL,height = '400px'),
				downloadButton(outputId = "FC_Up_List_Download",label = "Download Upregulated Gene List"),
                                downloadButton(outputId = "FC_Download_UP",label = "Download Upregulated FC Data"),
                                downloadButton(outputId = "FC_GeneList_UP",label = "Download Upregulated RPKM Data"),
                                textAreaInput(inputId="GeneList_Result_DOWN", label = "Down Regulated Gene List", value = NULL,height = '400px'),
				downloadButton(outputId = "FC_Down_List_Download",label = "Download Downregulated Gene List"),
                                downloadButton(outputId = "FC_Download_DOWN",label = "Download Downregulated Genes"),
                                downloadButton(outputId = "FC_GeneList_DOWN",label = "Download Downregulated RPKM Data")
                       ),#tabPanel("GeneLists",
                       tabPanel("GeneTable",textAreaInput(inputId="Gene_Table", label = "Gene Table", value = NULL,height = '400px',width = '800px'),
                                downloadButton("downloadData", "Download CSV"),
                                textAreaInput(inputId="FPKM_Table", label = "Reads Table", value = NULL,height = '400px',width = '800px'),
				downloadButton("downloadFPKM", "Download TXT")
                       )#tabPanel("GeneTable",
                     )#tabsetPanel
                     
                     #tableOutput("contents")
                   )#mainPanel
                 )#sidebarLayout(
                 
                 
                  )#box(
  			)#fluidRow(

				
				),#tabItem(tabName = "Count_EdgeR_Data",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    ####### Load Expression Data #########
			    ######################################
			    ######################################
			    
				tabItem(tabName = "Expression_Data",
				
				
		  fluidRow(

    		box(
     	 title = p(actionButton("Expression_load_help", "", icon = icon("question"),
                  class = "btn-xs", title = "Help"),"Load Expression data" 
                
     	 ), 
     	 width = 10, solidHeader = TRUE, 
      	uiOutput("boxContentExpression"),

			 sidebarLayout(
               sidebarPanel(
                 	  
                 	  
                 h5(id="h4SelectInput",radioButtons(inputId = "Select_input",label = "Select Input Source",choices = c("Upload File"="upload","Database"="dbase"),selected = "upload")), 
                 	  
                 hidden(
                   div(id = "file_RPKM_Box_wrapper",
                     box(id = "file_RPKM_Box", title = "Select an Exppression file", width = '200px',   
                 	  
                 	  
                  	  h5(id="h4FileInput",fileInput("file_RPKM", "Choose Expression File (FPKM/RPKM/TPM)",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv",".rda","RData")
                    		))#h5(id="h4FileInput",fileInput("file_RPKM", "Choose Expression File",
                    		
                    ))),#hidden(div(id = "file1_Box_wrapper"
  	                      
  	              hidden(
                   div(id = "FPKM_Database_Box_wrapper",
                     box(id = "FPKM_Database_Box", title = "Select a FPKM/TPM file", width = '200px',    
  	                       	                      
                    		h5(id="h4DataUser",selectInput(inputId="Data_User", label="Select Data Library", c(Data.folder.files), selected = NULL, multiple = F)),
                    		h5(id="h4DataSet",selectInput(inputId="Data_set", label="Select Dataset", c(""), selected = NULL, multiple = F))
                    	))),#hidden(div(id = "FPKM_Database_Box_wrapper"	
                    		
                    h5(id="h4LoadFile",actionButton(inputId = "Load_File",label = "Load File")),
                    
                  hidden(
                  	 div(id = "FPKM_ADD_FILE_Box_wrapper",
                    		 box(id = "FPKM_ADD_FILE_Box", title = "Select a counts/reads file", width = '200px'   
                    			#h5(id="h4RPKMAddDataCheck",fluidRow(
                       	#column(width=3,checkboxInput(inputId="RPKM.Add_Data_Check", label="Add data", FALSE)),column(width=2,actionButton(inputId="RPKM.Add_Data", label="Add File")),column(width=2,actionButton(inputId="RPKM.Reset_Add_Data", label="Reset"))))
                    	))),#hidden(div(id = "FPKM_ADD_FILE_Box_wrapper"	   	
                   
                    ),#sidebarPanel
                 
                   # Show a plot of the generated distribution
                    mainPanel(
                      hidden(div(id="distPlot_div",shinycssloaders::withSpinner(plotOutput("distPlot")))),
                      tableOutput("User_Data_File")
                   )#mainPanel
                  )#sidebarLayout
                 
                 
                 )#box(
  		    )#fluidRow(
			    ),#tabItem(tabName = "Expression_Data",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    ########### Create Groups ############
			    ######################################
			    ######################################			    
				tabItem(tabName = "Create_Groups",
		          fluidRow(
    			  box(
     	 		title = p(actionButton("Create_Groups_load_help", "", icon = icon("question"),
                  	class = "btn-xs", title = "Help"),"Create Groups"                
     	 		),#title = p 
     	 			width = 10, solidHeader = TRUE, 
      				uiOutput("boxContent_Create_Groups"),

					 fluidRow(
                 		column(width=3,                 		
                 		h5(id = "h4GroupName",textInput(inputId="Group_Name",label="Group Name",placeholder="Enter Group Name"))),
                 		column(width=3,
                 		h5(id = "h4SelectGroupMembers",selectInput(inputId="All_Conditions", label="Select Group Members", c(""), selected = NULL, multiple = T)))
               		  ),#fluidRow
               		fluidRow(column(width=2,h5(id="h4CreateGroup",actionButton(inputId = "Create_Group",label = "Create Group")))),
               		fluidRow(column(width=2,h5(id="h4UploadGroupsFile",fileInput("Upload_Groups_File", "Choose 'Groups' File")))),
               		fluidRow(column(width=2,h5(id="h4UploadGroup",actionButton(inputId = "Upload_Group",label = "Upload Groups"))),
               				 column(width=2,h5(id="h4ConfirmGroup",actionButton(inputId = "Confirm_Group",label = "Confirm Groups")))),
               		fluidRow(column(width=8,h5(id="h4Groups",textAreaInput(inputId = "Groups",label = "Groups",width = 800, height = 200)))),
               		fluidRow(column(width=2,h5(id="h4DownloadGroups",downloadButton(outputId = "downloadGroups", label = "Download Groups"))))
				
				

				    )#box
  		    	)#fluidRow
				),#tabItem(tabName = "Create_Groups",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    ###### EdgeR Group Comparison ########
			    ######################################
			    ######################################
				tabItem(tabName = "Groups_CountData",
				fluidRow(
    			  box(
     	 		title = p(actionButton("Groups_CountData_help", "", icon = icon("question"),
                  	class = "btn-xs", title = "Help"),"Groups' Differentially Expressed Genes Analysis"                
     	 		),#title = p( 
     	 			width = 10, solidHeader = TRUE, 
      				uiOutput("boxContent_Groups_CountData"),
					  sidebarLayout(
           				sidebarPanel(
             				h5(id="h4GroupsControl",selectInput(inputId="Groups.Control", label="Select Group 1", c(""), selected = NULL, multiple = F)),
             				h5(id="h4GroupsTreatment",selectInput(inputId="Groups.Treatment", label="Select Group 2", c(""), selected = NULL, multiple = F)),
            				h5(id="h4GroupsCompareButton",actionButton(inputId="Groups.CompareButton", label="Compare Groups")),
             				h5(id="h4GroupsLog2FCThreshold",textInput(inputId="Groups.Log2FCThreshold", label="Log2FC", value = "1", width = '60px', placeholder = NULL)),
             				h5(id="h4GroupsFDRPvalThreshold",textInput(inputId="Groups.FDR_PvalThreshold", label="P-value", value = "0.05", width = '60px', placeholder = NULL)),
             				h5(id="h4GroupsPvalCorrection",selectInput(inputId="Groups.Pval_Correction", label="P.val adj", c("none","BH", "fdr","BY","holm"), selected = "fdr", multiple = F)),
	     					h5(id="h4GroupsReadsPlotWidths",sliderInput(inputId = "Groups.Reads.Plot_Widths",label = "Plot Widths",min =1 ,max = 10,value = 10)), 
	     					h5(id="h4GroupsReadsVolcanoGenes",sliderInput(inputId = "Groups.Reads.Volcano_Genes",label = "Show Volcano Plot Genes",min =0 ,max = 200,value = 20)),
             				h5(id="h4GroupsReadsVolcanoFontSize",sliderInput(inputId = "Groups.Reads.Volcano_Font_Size",label = "Volcano Plot Font Size",min = 0,max = 10,value = 2)),
             				h5(id="h4GroupsReadsHeatmapFontSize",sliderInput(inputId = "Groups.Reads.Heatmap_Font_Size",label = "Heatmap Font Size",min = 0,max = 15,value = 5)),
	     					h5(id="h4GroupsReadsHeatmapSamples",radioButtons(inputId = "Groups.Reads.Heatmap_Groups",label = "Heatmap Groups",choices = c("Compared Groups","All Groups"),inline = T,selected = "Compared Groups"))

           				),#sidebarPanel
           
           			mainPanel(
             		 tabsetPanel(
               			tabPanel("Summary_plots", 
               			         hidden(div(id="Groups.ComparePlot1_div",shinycssloaders::withSpinner(plotlyOutput("Groups.ComparePlot1",height = "500px",width = "500px")))),
               			         hidden(div(id="Groups.ComparePlot2_div",shinycssloaders::withSpinner(plotOutput("Groups.ComparePlot2")))),
               			         hidden(div(id="Groups.ComparePlot3_div",shinycssloaders::withSpinner(plotOutput("Groups.ComparePlot3")))), 
               			         hidden(div(id="Groups.ComparePlot4_div",(plotOutput("Groups.ComparePlot4")))), #removed shinycssloaders
               			         hidden(div(id="Groups.ComparePlot5_div",(plotOutput("Groups.ComparePlot5")))), #removed shinycssloaders
               			         hidden(div(id="Groups.ComparePlot6_div",(plotOutput("Groups.ComparePlot6")))), #removed shinycssloaders
                        	plotOutput("Groups.ComparePlot7"),
                        	plotOutput("Groups.ComparePlot8")),
               			tabPanel("Groups.GeneLists",
                    	    textAreaInput(inputId="Groups.GeneList_Result", label = "Gene List", value = NULL,height = '400px'),
                    	    downloadButton(outputId = "FC_Download.Groups",label = "Download FC Data"),
                    	    downloadButton(outputId = "FC_GeneList.Groups",label = "Download RPKM Data"),
                    	    textAreaInput(inputId="Groups.GeneList_Result_UP", label = "Upregulated Gene List", value = NULL,height = '400px'),
                    	    downloadButton(outputId = "FC_Download_UP.Groups",label = "Download Upregulated FC Data"),
                    	    downloadButton(outputId = "FC_GeneList_UP.Groups",label = "Download Upregulated RPKM Data"),
                    	    textAreaInput(inputId="Groups.GeneList_Result_DOWN", label = "Down Regulated Gene List", value = NULL,height = '400px'),
                    	    downloadButton(outputId = "FC_Download_DOWN.Groups",label = "Download Downregulated FC Data"),
                    	    downloadButton(outputId = "FC_GeneList_DOWN.Groups",label = "Download Downregulated RPKM Data")
               			)#tabPanel("Groups.GeneLists",
             		   )#tabsetPanel(
             
           			 )# mainPanel(
        		   )#sidebarLayout(
				  )#box(
  		    	)#fluidRow(
				),#tabItem(tabName = "Groups_CountData",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    ###### Gene Comparison (Samples) #####
			    ######################################
			    ######################################
				tabItem(tabName = "Sample_Comparison",
				 fluidRow(
    			  box(
     	 		title = p(actionButton("Sample_Comparison_help", "", icon = icon("question"),
                  	class = "btn-xs", title = "Help"),"Compare Gene Expressions Between Samples"                
     	 		),#title = p( 
     	 			width = 10, solidHeader = TRUE, 
      				uiOutput("boxContent_Sample_Comparison"),
				    fluidRow(
                 	 column(width=4,h5(id="h4SingleCompareGeneList",selectInput(inputId="Single.Compare.GeneList",label="Select Genes (max:12)",c(""),selected=NULL,multiple=T)),
                 				h5(id = "h4CompareSampleExp" ,actionButton(inputId = "Compare.Sample.Exp",label = "Compare Gene Expressions"))),
				 	 column(width=3, h5(id = "h4SinglePasteGeneList",textAreaInput(inputId="Single.Paste.GeneList",label="Paste Gene List (max:12)",width = 200, height = 100))),
                 	 column(width=4, h5(id = "h4SingleCompareConditions", selectInput(inputId="Single.Compare.Conditions", label="Select Samples", c(""), selected = NULL, multiple = T)))
               	    ),#fluidRow(
             
	      		    fluidRow(
               		 column(width=4,h5(id = "h4SampleGraphWidth",sliderInput(inputId = "Sample.Graph_Width",label = "Graph Width",min = 1,max = 5,value = 4))),
 					 column(width=4,h5(id = "h4SampleFontSize",sliderInput(inputId = "Sample.Font_Size",label = "Font Size",min = 1,max = 4,value = 2)))
               	    ),#fluidRow(
	       
	       		    fluidRow(
					 column(width=4,
					 h5(id = "h4IncludeColor" ,actionButton(inputId = "Include.Color",label = "Include Selected Color")),
					 h5(id = "h4Colorpicker" ,colourInput(inputId="Color_picker", "Select Color", "#000000")),
					 h5(id = "h4SingleColorList" ,selectInput(inputId="Single.Color.List",label="Select Colors:", ALL.COLORS,selected=NULL,multiple=T))),
					 column(width=4,h5(id = "h4ingleThemeList" ,selectInput(inputId="Single.Theme.List",label="Select Theme:",c("default","Black and White","Classic","Dark","Test","Void"),selected="default",multiple=F)),
					 h5(id = "h4SingleXaxisangle" ,sliderInput(inputId = "Single.Xaxis.angle",label = "X-axis Orientation",min = 0,max = 180,value = 45)))
 	       		   ),#fluidRow(
 
             	   
					 textOutput("Stacked_Plot.label"),
					 hidden(div(id="Stacked_Plot_div",shinycssloaders::withSpinner(plotOutput("Stacked_Plot")))),
					 textOutput("Single.Sample.barplot.label"),
					 hidden(div(id="Single.Sample.barplot_div",shinycssloaders::withSpinner(plotOutput("Single.Sample.barplot")))),
					 textOutput("Single.Sample.heatmap.label"),
					 hidden(div(id="Single.Sample.heatmap_div",(plotOutput("Single.Sample.heatmap")))),  #removed shinycssloaders
					 textOutput("MDS_PLOT.Samples.label"),
					 hidden(div(id="MDS_PLOT.Samples_div",(plotOutput("MDS_PLOT.Samples"))))  #removed shinycssloaders
                  
                  )#box(
  		    	)#fluidRow(  
				),#tabItem(tabName = "Sample_Comparison",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    ###### Gene Comparison (Groups) ######
			    ######################################
			    ######################################
				tabItem(tabName = "Group_Comparison",
				 fluidRow(
    			  box(
     	 		title = p(actionButton("Group_Comparison_help", "", icon = icon("question"),
                  	class = "btn-xs", title = "Help"),"Compare Gene Expressions Between Groups"                
     	 		),#title = p( 
     	 			width = 10, solidHeader = TRUE, 
      				uiOutput("boxContent_Group_Comparison"),
      				
               		fluidRow(
                 		column(width=4,h5(id="h4GroupCompareGeneList",selectInput(inputId="Group.Compare.GeneList",label="Select Genes (max:12)",c(""),selected=NULL,multiple=T)),
                                h5(id="h4CompareGroupExp",actionButton(inputId = "Compare.Group.Exp",label = "Compare Groups' Expressions"))),
                 		column(width=3, h5(id="h4GroupPasteGeneList",textAreaInput(inputId="Group.Paste.GeneList",label="Paste Gene List (max:12)",width = 200, height = 100))),
                 		column(width=4,h5(id="h4GroupCompareCondition",selectInput(inputId="Group.Compare.Condition", label="Select Groups", c(""), selected = "NULL", multiple = T)))
               		),#fluidRow(
			   		fluidRow(
               			column(width=4,h5(id="h4GroupGraphWidth", sliderInput(inputId = "Group.Graph_Width",label = "Graph Width",min = 1,max = 4,value = 4))),
              			column(width=4,h5(id="h4GroupFontSize", sliderInput(inputId = "Group.Font_Size",label = "Font Size",min = 1,max = 4,value = 2)))
                	),#fluidRow(
               	    fluidRow(
						column(width=4,
						h5(id="h4GroupIncludeColor", actionButton(inputId = "Group.Include.Color",label = "Include Selected Color")),
						h5(id="h4GroupColorpicker", colourInput(inputId="Group.Color_picker", "Select Color", "#000000")),
						h5(id="h4GroupColorList", selectInput(inputId="Group.Color.List",label="Select Colors:", ALL.COLORS,selected=NULL,multiple=T))),
						column(width=4,h5(id="h4GroupThemeList", selectInput(inputId="Group.Theme.List",label="Select Theme:",c("default","Black and White","Classic","Dark","Test","Void"),selected="default",multiple=F)),
						h5(id="h4GroupXaxisangle", sliderInput(inputId = "Group.Xaxis.angle",label = "X-axis Orientation",min = 0,max = 180,value = 45)))
	       		  	),#fluidRow(
               
						textOutput("Group.boxplot.label"),
						hidden(div(id="Group.boxplot_div",shinycssloaders::withSpinner(plotOutput("Group.boxplot",height = "800px")))),
						textOutput("Group.barplot.label"),
						hidden(div(id="Group.barplot_div",shinycssloaders::withSpinner(plotOutput("Group.barplot")))),
						textOutput("Group.barplot.sem.label"),
						hidden(div(id="Group.barplot.sem_div",(plotOutput("Group.barplot.sem")))),  #removed shinycssloaders
						textOutput("Group.heatmap.label"),
						hidden(div(id="Group.heatmap_div",(plotOutput("Group.heatmap")))),  #removed shinycssloaders
						textOutput("Group.members.heatmap.label"),
						hidden(div(id="Group.members.heatmap_div",(plotOutput("Group.members.heatmap")))),  #removed shinycssloaders
						textOutput("Group.MDS_PLOT.Samples.label"),
						hidden(div(id="Group.MDS_PLOT.Samples_div",(plotOutput("Group.MDS_PLOT.Samples"))))  #removed shinycssloaders
				  )#box(
  		    	)#fluidRow(
				),#tabItem(tabName = "Group_Comparison",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    ##### Gene Set Entrichment Analysis ##
			    ######################################
			    ######################################
				tabItem(tabName = "GSEA",
				 fluidRow(
    			  box(
     	 		title = p(actionButton("GSEA_help", "", icon = icon("question"),
                  	class = "btn-xs", title = "Help"),"Perform Gene Set Enrichment Analysis"                
     	 		),#title = p( 
     	 			width = 10, solidHeader = TRUE, 
      				uiOutput("boxContent_GSEA"),

                    sidebarLayout(
                     sidebarPanel(
                                   
                       	h5(id="h4GSEA_GroupvSample",radioButtons(inputId = "GSEA_Group_v_Sample",label = "Analyze Groups or Samples?",choices = c("Groups","Sample"),inline = T,selected = "Sample")),
                        h5(id="h4ControlGSEATest",selectInput(inputId = "Control_GSEA_Test",label = "Select Control",choices = c(""), selected = NULL, multiple = F)),
                        h5(id="h4QueryGSEATest",selectInput(inputId = "Query_GSEA_Test",label = "Select Subjects",choices = c(""), selected = NULL, multiple = F)),
                                     
                        fluidRow(
				      		column(width=4,h5(id="h4GseaTopHits",textInput(inputId="GSEA_TOP_HITS", label="Upregulated Enriched GeneSets", value = "10", width = '70px', placeholder = NULL))),	
 		                    column(width=4,h5(id="h4GseaBottomHits",textInput(inputId="GSEA_BOTTOM_HITS", label="Downregulated Enriched GeneSets", value = "10", width = '70px', placeholder = NULL)))
				     	),#fluidRow(
				     				                               
                        h5(id="h4GSEADatasetList",selectInput(inputId = "GSEA_Dataset_List",label = "Select GSEA Collection",choices = GSEA_LIST, selected = c("hallmark gene sets","GO gene sets"), multiple = T)),
                        h5(id="h4RunEnrichedSignatures",actionButton(inputId = "Run_Enriched_Signatures",label = "Find Enriched Gene Signatures")),
                                 
                        h5(id="h4GSEAGeneSet",selectInput(inputId = "GSEA_GeneSet",label = "Select Gene Set",choices = c(), selected = NULL, multiple = F)),
                        h5(id="h4GSEAGenelimitslider",sliderInput(inputId = "GSEA_Gene_limit_slider",label = "Select Gene Limit",min = 0,max = 1,value = 0,step = 1))
                                    
                         #actionButton(inputId = "Run_GSEA",label = "Run GSEA Analysis")

                      ),#sidebarPanel
                      mainPanel(
                         hidden(div(id="GSEA_PLOT_1_div",shinycssloaders::withSpinner(plotOutput(outputId = "GSEA_PLOT_1")))),
                         hidden(div(id="GSEA_PLOT_2_div",(plotOutput(outputId = "GSEA_PLOT_2")))),  #removed shinycssloaders
                          fluidRow(
                               column(width=5,hidden(div(id="GSEA_Heatmap_div",shinycssloaders::withSpinner(plotlyOutput(outputId = "GSEA_Heatmap"))))),
                               column(width=5,hidden(div(id="GSEA_Heatmap2_div",(plotlyOutput(outputId = "GSEA_Heatmap2"))))) # #removed shinycssloaders
                           ),#fluidRow(
                        hidden(div(id="GSEA_Barplot_div",(plotlyOutput(outputId = "GSEA_Barplot")))) # #removed shinycssloaders
                                     
                        )#mainPanel
						  )#sidebarLayout

				  )#box(
  		      	)#fluidRow(				
				),#tabItem(tabName = "GSEA",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    ############ Gene Ontology ###########
			    ######################################
			    ######################################
				tabItem(tabName = "Gene_Ontology",
				fluidRow(
    			  box(
     	 		title = p(actionButton("Gene_Ontology_help", "", icon = icon("question"),
                  	class = "btn-xs", title = "Help"),"Gene Ontology Analysis"                
     	 		),#title = p( 
     	 			width = 12, solidHeader = TRUE, 
      				uiOutput("boxContent_Gene_Ontology"),
				
				      sidebarLayout(
                        sidebarPanel(
                                     
                         h5(id = "h4OrganismCheckbox", checkboxGroupInput(inputId = "Organism_Checkbox",label=h5("Select Organism"),choices = c("Human","Mouse"),selected = c("Human","Mouse"),inline = T)),
                         h5(id = "h4OGeneList", textAreaInput(inputId = "GO_GeneList",label = "Enter Gene List",placeholder = "Enter GeneList here")),
				         h5(id = "h4SelectInitialGO", radioButtons(inputId = "Select_Initial_GO",label="Select Ontology Type",choices = c("molecular_function","biological_process","cellular_component"),selected = "biological_process")),
				         fluidRow(
				            column(width=4,h5(id = "h4GOPvalThreshold", textInput(inputId="GO_PvalThreshold", label="P-value", value = "0.05", width = '70px', placeholder = NULL))),	
 		                    column(width=4,h5(id = "h4GOPvalCorrection", selectInput(inputId="GO_Pval_Correction", label="Correction", c("none","BH", "fdr","BY","holm"), selected = "none", multiple = F,width = '70px')))
				         ), 
				         h5(id = "h4GOLabelsx", radioButtons(inputId = "GO_Labels.x",label = "Select X-axis Labels",choices = c("GO ID","GO Name"),inline = T,selected = "GO ID")),
                         h5(id = "h4GOGeneListButton", actionButton("GO_GeneList_Button", label = "Get Ontologies")),
                         h5(id = "h4OntologyMethod", checkboxGroupInput(inputId = "Ontology_Method",label=h5("Select Ontology Confidence Code"),
                                                        choices = c("EXP - Inferred from Experiment","IDA - inferred from direct assay","IPI - inferred from physical interaction","IMP - inferred from mutant phenotype",
                                                                    "IGI - inferred from genetic interaction","IEP - inferred from expression pattern","TAS - traceable author statement",
                                                                    "ISS - inferred from sequence similarity","IEA - inferred from electronic annotation","NAS - non-traceable author statement",
                                                                    "ND - no biological data available","IC - inferred by curator","RCA - inferred from reviewed computational analysis",
                                                                    "IBA - Inferred from Biological aspect of Ancestor","IBD - Inferred from Biological aspect of Descendant",
                                                                    "IKR - Inferred from Key Residues","IRD - Inferred from Rapid Divergence"),
                                                        selected = c("EXP - Inferred from Experiment","IDA - inferred from direct assay","IPI - inferred from physical interaction","IMP - inferred from mutant phenotype",
                                                                     "IGI - inferred from genetic interaction","IEP - inferred from expression pattern","TAS - traceable author statement")
                           ))#checkboxGroupInput
                                     
                           ),#sidebarPanel
                           mainPanel(fluidRow(
                                     
                                column(width=8,
                                       hidden(div(id="Ontology_Barplot_div",shinycssloaders::withSpinner(plotOutput(outputId = "Ontology_Barplot")))),
                                       hidden(div(id="Ontology_Barplot_Pvalues_div",(plotlyOutput(outputId = "Ontology_Barplot_Pvalues"))))  #removed shinycssloaders
                                ),#column(width=8,
                                column(width=4,
                                          h5(id = "h4PreviousGOButton",actionLink(inputId = "Previous_GO_Button", label = "Go Back")),
                                          h5(id = "h4SelectGO",radioButtons(inputId = "Select_GO",label="Select Ontologies",choices = c("Gene Ontology"))),
                                          h5(id = "h4SelectGOButton" ,actionButton(inputId = "Select_GO_Button", label = "Select GO")),
                                          h5(id = "h4RepresentedGenes",textAreaInput(inputId = "Represented_Genes",label="Represented Genes"))
                                            
                                 )#column(width=4,
                               )#fluidRow
                              )#mainPanel
                             )#sidebarLayout
                                 
    
				  )#box(
  		    	)#fluidRow(
				),#tabItem(tabName = "Gene_Ontology",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    #### Group Statistical Comparison ####
			    ######################################
			    ######################################
				tabItem(tabName = "Group_Stats_Comparison",
				
				            sidebarLayout(
                                   sidebarPanel(
                                     h5(id = "h4Group1StatCompare", selectInput(inputId = "Group1_Stat_compare",label = "Select Group1",choices = c(""), selected = NULL, multiple = F)),
                                     h5(id = "h4Group2StatCompare",selectInput(inputId = "Group2_Stat_compare",label = "Select Group2",choices = c(""), selected = NULL, multiple = F)),
                                     h5(id = "h4StatTestSelect",checkboxGroupInput(inputId = "Stat_Test_Select",label=h5("Select Tests"),choices = c("Fold Change","Mean/SD","Wilcox","Ttest"),
                                                        selected = c("Fold Change","Mead/SD"))),
                                     h5(id = "h4GroupStatAnalyisButton",actionButton(inputId = "Group_Stat_analysis_button",label = "Find Genes")),
                                     h5(id = "h4GroupStatPValThresh",sliderInput(inputId = "Group_Stat_PVal_Thresh",label = "Select Pvalue",min = 0,max = 1,value = 0.05,step = 0.001)),
                                     h5(id = "h4GroupStatRPKMThresh",textInput(inputId = "Group_Stat_RPKM_Thresh",label = "Min Expression", value = 5,width = '60px')),
                                     h5(id = "h4GroupStatFCThresh",textInput(inputId = "Group_Stat_FC_Thresh",label = "Min log2FC", value = 2,width = '60px')),
                                     h5(id = "h4GroupStatDisplayTopGraphs",textInput(inputId = "Group_Stat_Display_Top_Graphs",label = "Display Top Genes", value = 10,width = '60px')),
                                     h5(id = "h4GroupStatListFC",checkboxInput(inputId = "Group_Stat_List_FC",label = "Show log2FC in gene list?",value = TRUE))
                                   ),#sidebarPanel(
                                   mainPanel(
                                     tabsetPanel(id = "Group_Stats_Comparison_Panels",
							##########################################
							##########################################
                                       tabPanel(	"Group_Stats_Summary_plots", 
                                                fluidRow(
    			 								 box(
     	 												title = p(actionButton("Group_Stats_Summary_plots_help", "", icon = icon("question"),
                  										class = "btn-xs", title = "Help"),"Statistical Biomarker Prediction Tool"                
     	 												),#title = p( 
     	 											 width = 12, solidHeader = TRUE, 
      												uiOutput("boxContent_Group_Stats_Comparison"),
      												
      												hidden(div(id="Group_Stats_log2RPKM_div",shinycssloaders::withSpinner(plotlyOutput("Group_Stats_log2RPKM",height = "600px",width = "600px")))),
      												hidden(div(id="Group_Stats_Scatter_Boxplot_div",shinycssloaders::withSpinner(plotOutput("Group_Stats_Scatter_Boxplot",height = "1000px")))),
      												hidden(div(id="Group_Stat_Gene_FC_div",(plotOutput("Group_Stat_Gene_FC")))),  #removed shinycssloaders
										
      												downloadButton(outputId = "Group_Stat_Download_Graphs", label = "Download All Data")
                           
      												)#box
    			 								 )#fluidRow
    			 								 ),#tabPanel("Summary_plots"
							##########################################
							##########################################
                                       tabPanel("Group_Stats_GeneList",
                                                textAreaInput(inputId = "Group_Stats_Gene_FC_List",label = "Top scoring genes with corresponding fold change",value="",width = '300px', height='300px')
                                       ),#tabPanel("Group_Stats_GeneList"
							##########################################
							##########################################
                                       tabPanel("PCA_tSNE_UMAP",
                                           fluidRow(
    			 					          box(
     	 												title = p(actionButton("t_SNE_help", "", icon = icon("question"),
                  										class = "btn-xs", title = "Help"),"Dimension Reduction Tool"                
     	 												),#title = p( 
     	 											 width = 12, solidHeader = TRUE, 
      												uiOutput("boxContent_T_SNE"),

												h5(id = "h4GroupStatsSelectMethod", radioButtons(inputId = "Group_Stats_Select_Method",label = "Select Dimension Reduction Method",choices = c("tSNE"="tSNE","PCA"="PCA","UMAP"="UMAP"),inline = T,selected = "tSNE")),
                                                h5(id = "h4GroupStatsRunTSNE", actionButton(inputId = "Group_Stats_Run_tSNE",label = "Run")),
												h5(id = "h4SelectTSNEObject", radioButtons(inputId = "Select_tSNE_Object",label = "Select t-SNE Analysis",choices = c("All Samples"="SAMPLES","Genes"="GENES"),selected = "SAMPLES")),
                                                fluidRow(column(width=4,h5(id = "h4GroupStatsTSNEMaxIter", sliderInput(inputId = "Group_Stats_tSNE_max_iter",label="Choose t-SNE max iterations", min = 100, max = 1000, value = 1000, step = 100)))),
                                                fluidRow(
                                                column(width=2,h5(id = "h4GroupStatsTSNEPointSize", sliderInput(inputId = "Group_Stats_tSNE_pointSize",label="Choose point size", min = 1, max = 50, value = 3, step = 1))),
												column(width=2,h5(id = "h4GroupStatsTSNESeed", textInput(inputId = "Group_Stats_tSNE_Seed",label = "Set Seed", value = 4000,width = '60px')))),
												h5(id = "h4GenerateTSNEList", checkboxInput(inputId = "Generate_tSNE_List",label = "Generate distance p-value lists? (May take some time)",value = FALSE)),
												h5(id = "h4GroupStatsTSNEGeneList", textAreaInput(inputId = "Group_Stats_tSNE_Gene_List",label = "(Optional) Enter Gene List",value="",width = '300px', height='200px')),
												fluidRow(column(width=4,h5(id = "h4GroupStatsTSNEGene", selectizeInput(inputId = "Group_Stats_tSNE_Gene",label = "Highlight Gene (Optional)",choices = c(""), selected = NULL, multiple = T)))),
												fluidRow(column(width=4,h5(id = "h4GroupStatsTSNEGroup", selectizeInput(inputId = "Group_Stats_tSNE_Group",label = "Highlight Group (Optional)",choices = c(""), selected = NULL, multiple = T)))),
												hidden(div(id="Group_Stats_t_SNE_3Dplot_div",shinycssloaders::withSpinner(plotOutput(outputId = "Group_Stats_t_SNE_3Dplot")))),
												hidden(div(id="Group_Stats_t_SNE_3Dplotly_div",(plotlyOutput(outputId = "Group_Stats_t_SNE_3Dplotly",height = "600px",width = "600px")))),  #removed shinycssloaders
												hidden(div(id="Group_Stats_t_SNE_plotly_div",(plotlyOutput(outputId = "Group_Stats_t_SNE_plotly",height = "600px",width = "600px")))),  #removed shinycssloaders
                                                
												fluidRow(
												  column(width=6,textAreaInput(inputId = "Group_Stats_t_SNE_3DGeneList",label = "Similarly Expressed Genes (3D t-SNE)",value="",width = '300px', height='300px')),
												  column(width=6,textAreaInput(inputId = "Group_Stats_t_SNE_2DGeneList",label = "Similarly Expressed Genes (2D t-SNE)",value="",width = '300px', height='300px'))
												  )#fluidRow(
                                                
                                                
                                       )#tabPanel("t-SNE"
                                     )#stabsetPanel(
                                   )#mainPanel(
                               )#sidebarLayout(
				  )#box(
  		    	 )#fluidRow(
				),#tabItem(tabName = "Group_Stats_Comparison",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    ### Group Expressed Gene Ontologies ##
			    ######################################
			    ######################################
				tabItem(tabName = "Expressed_Ontologies",
				fluidRow(
    			 	 box(
     	 				title = p(actionButton("Expressed_Ontologies_help", "", icon = icon("question"),
                  			class = "btn-xs", title = "Help"),"Expressed Ontologies Tool"                
     	 					),#title = p( 
     	 					 width = 12, solidHeader = TRUE, 
      					uiOutput("boxContent_Expressed_Ontologies"),

				
                    sidebarLayout(
                      sidebarPanel(
                      h5(id = "h4GOGroupVSample", radioButtons(inputId = "GO_Group_v_Sample",label = "Analyze Groups or Samples?",choices = c("Groups","Samples"),inline = T,selected = "Samples")),
                      h5(id = "h4ControlGOTest", selectInput(inputId = "Control_GO_Test",label = "Select Control",choices = c(""), selected = NULL, multiple = F)),
                      h5(id = "h4SubjectsGOTest", selectInput(inputId = "Subjects_GO_Test",label = "Select Subjects",choices = c(""), selected = NULL, multiple = T)),
                      h5(id = "h4SelectOntologies",fluidRow(
                        #column(width=4,h5(id = "h4SelectBPs",actionButton(inputId = "Select_BPs",label = "Biological Process",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:9px"))),
                        #column(width=4,h5(id = "h4SelectMFs", actionButton(inputId = "Select_MFs",label = "Molecular Function",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:9px"))),
                        #column(width=4,h5(id = "h4GOGroupVSample", actionButton(inputId = "Select_CCs",label = "Cellular Components",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:9px")))
			selectInput(inputId = "Ontology_Select_Parent",label = "Select GO Class",choices = Ontology_CLASS_SELECT, selected = NULL, multiple = F)
                       )),                                  
                       h5(id = "h4OntologyList", selectInput(inputId = "Ontology_List",label = "Select GO (Select GO Class Above)",choices = c(), selected = NULL, multiple = T)),
                       h5(id = "h4SelectOntologiesCombie",fluidRow(
                       	#column(width=4,h5(id = "h4SelectBPsCombine", actionButton(inputId = "Select_BPs_Combine",label = "Biological Process",style="color: #fff; background-color: #537ab7; border-color: #4e6da4; font-size:9px"))),
                        #column(width=4,h5(id = "h4SelectMFsCombine", actionButton(inputId = "Select_MFs_Combine",label = "Molecular Function",style="color: #fff; background-color: #537ab7; border-color: #4e6da4; font-size:9px"))),
                        #column(width=4,h5(id = "h4SelectCCsCombine", actionButton(inputId = "Select_CCs_Combine",label = "Cellular Components",style="color: #fff; background-color: #537ab7; border-color: #4e6da4; font-size:9px")))
			selectInput(inputId = "Ontology_Select_Parent_Combine",label = "Select GO Class",choices = Ontology_CLASS_SELECT, selected = NULL, multiple = F)
                       )),
                       h5(id = "h4OntologyCombine", selectInput(inputId = "Ontology_Combine",label = "Combine GOs with - Optional:(Select GO Class Above) ",choices = c(), selected = NULL, multiple = T)),
                       h5(id = "h4GOAnalysis", actionButton(inputId = "GO_Analysis",label = "Get GO Gene's Fold Change")),
                       h5(id = "h4GOOverlayVSide", radioButtons(inputId = "GO_Overlay_v_Side",label = "Select Display Style",choices = c("Overlay","Beside"),inline = T,selected = "Overlay")),
                       h5(id = "h4LabelGOGene", checkboxInput(inputId = "Label_GO_Gene",label = "Label points?",value = FALSE)),
                       h5(id = "h4GOTextFClimit", sliderInput(inputId = "GO_Text_FC_limit",label = "Label absFC Threshold",min = 0,max = 0,value = 0)),
                       h5(id = "h4GOMinRPKM", textInput(inputId = "GO_Min_RPKM",label = "Min Expression Value", value = 5,width = '60px')),
                       h5(id = "h4GODistaxislimit", sliderInput(inputId = "GO_Dist_axis_limit",label = "Expression Distribution axis range",min = 0,max = 0,value = c(0,0))),
                       h5(id = "h4GOntologyMethod2Compare", checkboxGroupInput(inputId = "Ontology_Method_2Compare",label=h5("Select Ontology Confidence Code"),
                                                        choices = c("EXP - Inferred from Experiment","IDA - inferred from direct assay","IPI - inferred from physical interaction","IMP - inferred from mutant phenotype",
                                                                    "IGI - inferred from genetic interaction","IEP - inferred from expression pattern","TAS - traceable author statement",
                                                                    "ISS - inferred from sequence similarity","IEA - inferred from electronic annotation","NAS - non-traceable author statement",
                                                                    "ND - no biological data available","IC - inferred by curator","RCA - inferred from reviewed computational analysis",
                                                                    "IBA - Inferred from Biological aspect of Ancestor","IBD - Inferred from Biological aspect of Descendant",
                                                                    "IKR - Inferred from Key Residues","IRD - Inferred from Rapid Divergence"),
                                                        selected = c("EXP - Inferred from Experiment","IDA - inferred from direct assay","IPI - inferred from physical interaction","IMP - inferred from mutant phenotype",
                                                                     "IGI - inferred from genetic interaction","IEP - inferred from expression pattern","TAS - traceable author statement")
                        ))#checkboxGroupInput
                      ),#sidebarPanel(
                      mainPanel(
                       h5(id = "h4GraphWidthFont",fluidRow(
                       	column(width=3,sliderInput(inputId = "GO.Graph_Width",label = "Graph Width",min = 1,max = 4,value = 4)),
                        column(width=3,sliderInput(inputId = "GO.Font_Size",label = "Font Size",min = 1,max = 4,value = 2)))),
                       hidden(div(id="GO_FC_Expression_div",shinycssloaders::withSpinner(plotOutput("GO_FC_Expression")))),
                       hidden(div(id="GO_FC_Expression_Sep_div",(plotOutput("GO_FC_Expression_Sep")))),  #removed shinycssloaders
                       hidden(div(id="GO_Distribution_div",(plotOutput("GO_Distribution")))),  #removed shinycssloaders
                       hidden(div(id="GO_Heatmap_rowScale_div",(plotlyOutput("GO_Heatmap_rowScale",height='600px',width = "1200px")))),  #removed shinycssloaders
                       hidden(div(id="Space_hold_div",(plotOutput("Space_hold",height='200px',width = "200px")))),  #removed shinycssloaders
                       hidden(div(id="GO_Heatmap_div",(plotlyOutput("GO_Heatmap",height='600px',width = "1200px")))),  #removed shinycssloaders                                                            
                        textAreaInput(inputId = "GENE_GO_FC_RPKM",label = "GO RPKM Table",value="",width = '800px', height='300px')  
                       )#mainPanel
                     )#sidebarLayout
				
				  )#box(
  		    	 )#fluidRow(
				),#tabItem(tabName = "Expressed_Ontologies",
				#######################################
				#######################################
				
				######################################
			    ######################################
			    ## Differentially Expressed Gene Ontologies ##
			    ######################################
			    ######################################
				tabItem(tabName = "Diff_Expressed_Ontologies",
				fluidRow(
    			 	 box(
     	 				title = p(actionButton("Diff_Expressed_Ontologies_help", "", icon = icon("question"),
                  			class = "btn-xs", title = "Help"),"Differentially Expressed Ontologies Tool"                
     	 					),#title = p( 
     	 					 width = 12, solidHeader = TRUE, 
      					uiOutput("boxContent_Diff_Expressed_Ontologies"),
      					
                      sidebarLayout(
                         sidebarPanel(
                          h5(id="h4DEOGOGroupVSample",radioButtons(inputId = "DEO_GO_Group_v_Sample",label = "Analyze Groups or Samples?",choices = c("Groups","Samples"),inline = T,selected = "Samples")),
                          h5(id = "h4DEOControlGOTest",selectInput(inputId = "DEO_Control_GO_Test",label = "Select Control",choices = c(""), selected = NULL, multiple = F)),
                          h5(id = "h4DEOSubjectsGOTest",selectInput(inputId = "DEO_Subjects_GO_Test",label = "Select Subjects",choices = c(""), selected = NULL, multiple = T)),
			  h5(id = "h4DEOGOClass", selectInput(inputId = "DEO_GO_CLASS",label = "Select GO Class",choices = Ontology_CLASS_SELECT, selected = NULL, multiple = F)),
                          h5(id = "h4DEOOntologyKeywordSelect",selectInput(inputId = "DEO_Ontology_Keyword_Select",label = "Choose Kewords",choices = DEO_Keywords, selected = NULL, multiple = T)),
                          #h5(id = "h4DEOKeywordText",textInput(inputId = "DEO_Keyword_Text",label = "Type in keyword (optional)", value = "",width = '300px',placeholder = "Sepearate keywords with ';'")),

                          h5(id = "h4DEOGOAnalysis",actionButton(inputId = "DEO_GO_Analysis",label = "Find Differentially Expressed GOs")),
                          h5(id = "h4DEOGOTextFClimit",sliderInput(inputId = "DEO_GO_Text_FC_limit",label = "Label absFC Threshold",min = 0,max = 0,value = 0)),
                          fluidRow(
                               column(width=4,h5(id = "h4DEOBaseThresh",textInput(inputId = "DEO_BaseThresh",label = "Min Expression base", value = 3,width = '60px'))),
                               column(width=4,h5(id = "h4DEOabsFCThresh",textInput(inputId = "DEO_absFCThresh",label = "Min log2 Fold Change", value = 0.7,width = '60px'))),
                               column(width=4,h5(id = "h4DEOPvalThresh",textInput(inputId = "DEO_PvalThresh",label = "Max p-value", value = 0.1,width = '60px')))
                           ),#fluidRow(
                           h5(id = "h4DEOGODistAxislimit",sliderInput(inputId = "DEO_GO_Dist_axis_limit",label = "Expression Distribution axis range",min = 0,max = 0,value = c(0,0))),
                           h5(id = "h4DEOOntologyMethod2Compare",checkboxGroupInput(inputId = "DEO_Ontology_Method_2Compare",label=h5("Select Ontology Confidence Code"),
                                                        choices = c("EXP - Inferred from Experiment","IDA - inferred from direct assay","IPI - inferred from physical interaction","IMP - inferred from mutant phenotype",
                                                                    "IGI - inferred from genetic interaction","IEP - inferred from expression pattern","TAS - traceable author statement",
                                                                    "ISS - inferred from sequence similarity","IEA - inferred from electronic annotation","NAS - non-traceable author statement",
                                                                    "ND - no biological data available","IC - inferred by curator","RCA - inferred from reviewed computational analysis",
                                                                    "IBA - Inferred from Biological aspect of Ancestor","IBD - Inferred from Biological aspect of Descendant",
                                                                    "IKR - Inferred from Key Residues","IRD - Inferred from Rapid Divergence"),
                                                        selected = c("EXP - Inferred from Experiment","IDA - inferred from direct assay","IPI - inferred from physical interaction","IMP - inferred from mutant phenotype",
                                                                     "IGI - inferred from genetic interaction","IEP - inferred from expression pattern","TAS - traceable author statement")
                             ))#checkboxGroupInput
                          ),#sidebarPanel(
                          mainPanel(
                         	 h5(id = "h4DEOGraphWidthFont",fluidRow(
                       		  column(width=3,sliderInput(inputId = "DEO_GO.Graph_Width",label = "Graph Width",min = 1,max = 4,value = 3)),
                            column(width=3,sliderInput(inputId = "DEO_GO.Font_Size",label = "Font Size",min = 1,max = 25,value = 10)))),
                         	 hidden(div(id="DEO_GO_Boxplot_div",shinycssloaders::withSpinner(plotlyOutput("DEO_GO_Boxplot")))),
                         	 hidden(div(id="DEO_GO_Distribution_div",(plotOutput("DEO_GO_Distribution")))),  #removed shinycssloaders
                         	 hidden(div(id="DEO_GO_Heatmap_rowScale_div",(plotlyOutput("DEO_GO_Heatmap_rowScale",height='600px',width = "1200px")))),  #removed shinycssloaders
                               #plotOutput("DEO_GO_Heatmap",height='600px',width = "1600px"),                                                              
                               textAreaInput(inputId = "DEO_GENE_GO_FC_RPKM",label = "GO Expression Table",value="",width = '800px', height='300px')
                           )#mainPanel
                        )#sidebarLayout		
				  )#box(
  		    	 )#fluidRow(                        		
				),#tabItem(tabName = "Diff_Expressed_Ontologies",
				#######################################
				#######################################

                                ######################################
                            ######################################
                            ########### Session Info #############
                            ######################################
                            ######################################
                                
    tabItem(tabName = "Session_Info",
            fluidRow(
              box(
                title = p(actionButton("Session_Info_help", "", icon = icon("question"),
                                       class = "btn-xs", title = "Help"),"Session Info (Restore/Load)"
                          ),#title = p
                width = 10, solidHeader = TRUE,
                uiOutput("boxContent_Session_Info"),
                p("Your current session ID is", textOutput(outputId = "Current_Session_ID", inline=T)),
                fluidRow(
                  column(width=5, h5(id = "h4Session_ID", textInput(inputId="Restore_Session_ID", label="Enter Session ID", placeholder="Enter Session ID"))),
                  column(width=3, h5(id = "h4", actionButton(inputId = "Restore_Session", label = "Restore Session")))
                  ),#fluidRow
                p(textOutput(outputId = "Restore_Session_Progress", inline=T)),
                fluidRow(column(width=5,h5(id="h4UploadSessionFile",fileInput("Upload_Session_File", "Choose 'Session' File (Rdata)"))),
                         column(width=3,h5(id="h4ConfirmUpload",actionButton(inputId = "Upload_Session",label = "Upload Session")))),
                fluidRow(column(width=2,h5(id="h4DownloadSession",downloadButton(outputId = "download_Session", label = "Download Session")))),
                hidden(div(id="session_Wait_plot_div",shinycssloaders::withSpinner(plotOutput("session_Wait_plot"))))
                )#box
              )#fluidRow
            
              )#tabItem(tabName = "Session_Info",
    #######################################
    #######################################
    ) ## tags$head
			) ## body <- dashboardBody

                      



#DATA.Values = c()
#DATA.Values.5min = c()
#Groups =c()
#Group.Members=c()
#GeneList=c()
# Define server logic required to draw a histogram
server <- function(input,output,session,DATA.Values=c(),DATA.Values.5min=c(),Groups=c(),Group.Members=c(),GeneList,Reads.Data_File_path,readData=c(),pengRPKMTable=c(), Group.GTable,Gene.Choices=c(),PRE_GROUPS="",Reads_Reset=F,DATA.Values_Flag=F) {


output$Current_Session_ID_Main <- renderText({
    
	IP_addr <- reactive(input$getIP)
	IP_addr_Text <- capture.output(IP_addr(),split=F)
	IP_addr_Text_Clean <- gsub("\"|\\[1\\]| ","",IP_addr_Text)
	DATE = gsub("-",".",Sys.Date())


	if(is.null(IP_addr_Text_Clean) | IP_addr_Text_Clean =="NULL" | length(IP_addr_Text_Clean)==0)
	{
	  PART2 = round(runif(1,10,99))
	  PART3 = round(runif(1,100,999))
	  PART4 = round(runif(1,100,999))
	  IP_addr_Text_Clean = paste("CUS",PART2,PART3,PART4,sep=".")
	}
	  
	
	Session_ID_base = paste(as.character(IP_addr_Text_Clean),DATE,sep="_")

	BackedUp_Sessions = system(paste("ls ",Backup.folder,sep=""),intern=T)

	Session_iteration = 1
	if( any(grepl(Session_ID_base,BackedUp_Sessions)))
	{
		Last_Session_iteration = max(sapply(BackedUp_Sessions[grep(Session_ID_base,BackedUp_Sessions)], function(X) as.numeric(tail(strsplit(X,split="_")[[1]],n=1))))
		Session_iteration = Last_Session_iteration + 1

	}#if( any(grepl(Session_ID_base,BackedUp_Sessions)))

	Session_ID = paste(Session_ID_base,Session_iteration,sep="_")

	Backup_Session_ID <<- Session_ID
	Backup_Session_Folder <<- paste(Backup.folder,Session_ID,"/",sep="")
	
	if(!grepl("NULL",Backup_Session_Folder)) system(paste("mkdir",Backup_Session_Folder))
	#system(paste("mkdir",Backup_Session_Folder))	

	  output$Current_Session_ID <- renderText({Session_ID})
	  
      return(paste("\t Session ID:",Session_ID))
  })#output$Current_Session_ID <- renderText


observeEvent(input$Restore_Session,ignoreInit = TRUE,{

	disable("Restore_Session")
  shinyjs::show(id = "session_Wait_plot_div") 

  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  progress$set(message = "Restoring Session", value = 0)
  progress$inc(0.1, detail =  paste("Progress: ",10,"% Please Wait",sep=""))
  

	Session_ID_Query = gsub(" ","",input$Restore_Session_ID)
	BackedUp_Sessions = system(paste("ls ",Backup.folder,sep=""),intern=T)


	output$Restore_Session_Progress <- renderText("Searching for Session ID...")	
	output$Restore_Session_Progress <- renderText("Session Not Found")
	if( any( BackedUp_Sessions == Session_ID_Query ))
    {

	  progress$inc(0.3, detail =  paste("Progress: ",20,"% Session ID Found",sep=""))
	  
		Session_Data_Files = system(paste("ls ", Backup.folder, "/", Session_ID_Query, "/*rdata", sep=""), intern=T)

      if(length(Session_Data_Files) >= 1)
      {

        progress$inc(0.4, detail =  paste("Progress: ",40,"% Transferring Data",sep=""))
        
			  for(I in 1:length(Session_Data_Files))
			  {

				  output$Restore_Session_Progress <- renderText("Loading Data...")
                    
          load(Session_Data_Files[I])
          
          system(paste("cp",Session_Data_Files[I],Backup_Session_Folder))
          
			  }#for(I in 1:length(Session_Data_Files))

        
        
        progress$inc(0.5, detail =  paste("Progress: ",50,"% Loading Data",sep=""))
        
        All_Objects = ls()

        All_Objects = All_Objects[which(!All_Objects %in% c("I","Session_ID_Query","BackedUp_Sessions"))]

        Make_Global_Object_Command = paste(All_Objects,"<<-", All_Objects)

        for(Command in Make_Global_Object_Command)
        {
          eval(expr = parse(text = Command))

        }#for(Command in Make_Global_Object_Command)
        
        progress$inc(0.7, detail =  paste("Progress: ",75,"% Generating Plots",sep=""))
        
        source("Scripts/Restore_Session_Script.R", local=T)
        
        progress$inc(0.99, detail =  paste("Progress: ",100,"% Session Restored",sep=""))
        output$Restore_Session_Progress <- renderText("Session Restored.")

     }#if(length(Session_Data_Files) >= 1)

	} ## if( any( BackedUp_Sessions == Session_ID_Query ))


	
	enable("Restore_Session")	
	shinyjs::hide(id = "session_Wait_plot_div") 
})  ## observeEvent(input$Restore_Session



observeEvent(input$Upload_Session,ignoreInit = TRUE,{
  
  disable("Upload_Session")
  #Upload_Session_File
  shinyjs::show(id = "session_Wait_plot_div") 
  
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  progress$set(message = "Uploading Session", value = 0)
  progress$inc(0.3, detail =  paste("Progress: ",10,"% Please Wait",sep=""))
  
  
    Session_Data_Files = input$Upload_Session_File
    
    if(!is.null(Session_Data_Files$datapath))
    {
    
      load(Session_Data_Files$datapath)
  
      All_Objects = ls()
        
      #All_Objects = All_Objects[which(!All_Objects %in% c("I","Session_ID_Query","BackedUp_Sessions"))]
        
      Make_Global_Object_Command = paste(All_Objects,"<<-", All_Objects)
          
      progress$set(message = "Uploading Session", value = 0)
      progress$inc(0.5, detail =  paste("Progress: ",50,"% Loading Data",sep=""))
      
      for(Command in Make_Global_Object_Command)
      {
        eval(expr = parse(text = Command))
          
      }#for(Command in Make_Global_Object_Command)
  
      progress$set(message = "Uploading Session", value = 0)
      
      progress$inc(0.7, detail = paste("Progress: ",70,"% Loading Data",sep=""))
      source("Scripts/Upload_Session_Script.R", local=T)
      
      progress$inc(0.8, detail = paste("Progress: ",85,"% Generating Plots",sep=""))
      source("Scripts/Restore_Session_Script.R", local=T)
      
      
    output$Restore_Session_Progress <- renderText("Session Uploaded")
    }#if(!is.null(Session_Data_Files$datapath))
  enable("Upload_Session")	
  shinyjs::hide(id = "session_Wait_plot_div") 
  progress$inc(1, detail = paste("Progress: ",100,"%",sep=""))
})  ## observeEvent(input$Upload_Session,



 output$download_Session <- downloadHandler(
   filename = function() {
     paste('Session_',Backup_Session_ID, '.rdata', sep='')
   }, #filename = function() 
   content = function(FILE) {
     
     disable("download_Session")
     shinyjs::show(id = "session_Wait_plot_div") 
     

      Session_Data_Files = system(paste("ls ",Backup_Session_Folder, "/*rdata", sep=""), intern=T)
      
      if(length(Session_Data_Files) >= 1)
      {
        for(I in 1:length(Session_Data_Files))
        {
          load(Session_Data_Files[I])
        }#for(I in 1:length(Session_Data_Files))
        source("Scripts/Download_Session_Script.R", local=T)
        
       
      
        save(list= Selected_DATA, file =  FILE)
        
        
        
      }#if(length(Session_Data_Files) >= 1)
      enable("download_Session")
      shinyjs::hide(id = "session_Wait_plot_div") 
   }#content = function(FILE) 
 )#output$download_Session <- downloadHandler



observeEvent(input$Reads.Select_input,{
	
		if(input$Reads.Select_input=="upload")
		{
			shinyjs::hide(id = "Reads_Database_Box_wrapper")
			shinyjs::show(id = "file1_Box_wrapper")	
			if(length(input$Reads.Library_select) >= 1)
			{
				shinyjs::show(id = "Sample_Select_Box_wrapper")	
			}#if(length(input$Reads.Library_select) >= 1)
					
		}#if(input$Reads.Select_input=="upload")
		
		if(input$Reads.Select_input=="dbase")
		{
			shinyjs::hide(id = "file1_Box_wrapper")
			shinyjs::hide(id = "Sample_Select_Box_wrapper")
			shinyjs::show(id = "Reads_Database_Box_wrapper")
		}#if(input$Reads.Select_input=="dbase")
	
	})#observeEvent(input$Reads.Select_input,{


#Data_set_RPKM_Box_wrapper
#h5(id="h4SelectInput",radioButtons(inputId = "Select_input",label = "Select Input Source",choices = c("Database"="dbase","Upload File"="upload"),selected = "dbase")), 

observeEvent(input$Select_input,{
	
		if(input$Select_input =="upload")
		{
			shinyjs::hide(id = "FPKM_Database_Box_wrapper")
			shinyjs::show(id = "file_RPKM_Box_wrapper")	
		
		}#if(input$Select_input =="upload")
		
		if(input$Select_input =="dbase")
		{
			shinyjs::hide(id = "file_RPKM_Box_wrapper")
			shinyjs::show(id = "FPKM_Database_Box_wrapper")
		}#if(input$Reads.Select_input=="dbase")
	
	})#observeEvent(input$Select_input,{


observeEvent(input$file1,{
	
		inFile <- input$file1
		if(is.null(inFile))
		{
			shinyjs::hide(id = "Sample_Select_Box_wrapper")
		}
		if(!is.null(inFile))
		{
		 	file.datapath <<- inFile$datapath
     		if(file.exists(file.datapath) && !dir.exists(file.datapath)) 
     		{
     			File.Check = readLines(file.datapath)
     			if(length(File.Check)==0)
     			{
     				shinyjs::hide(id = "Sample_Select_Box_wrapper")
     	  			return(NULL)
     	  		}
     	
     		    Data_5 = read.delim(file.datapath,nrow=5)[,-1]       		    
     		    Sample_Names = colnames(Data_5)      
     		    
     		    N = ifelse(8<length(Sample_Names),8,length(Sample_Names))
     		    
     		    if(N >0) shinyjs::show(id = "Sample_Select_Box_wrapper")
     		    
     		    #selectInput(inputId="Reads.Library_select", label="Select Dataset", c(""), selected = NULL, multiple = F),
  	     		updateSelectizeInput(session, inputId = "Reads.Library_select", label = "Select Samples", choices = Sample_Names,selected = Sample_Names[1:N], server = TRUE)

			}#if(file.exists(file.datapath) && !dir.exists(file.datapath)) 
		}#if(!is.null(inFile))
	
	})#observeEvent(input$file1,{





  
  
 observeEvent(input$Counts_EdgeR_help, {
    if (input$TABS == "Count_EdgeR_Data") {
    			shinyjs::delay(ms = 100,shinyjs::show(id = "file1_Box_wrapper") )
    			#shinyjs::show(id = "file1_Box_wrapper")
    		shinyjs::delay(ms = 100,shinyjs::show(id = "Sample_Select_Box_wrapper"))
			#shinyjs::show(id = "Sample_Select_Box_wrapper")
			shinyjs::delay(ms = 100,shinyjs::show(id = "Reads_Database_Box_wrapper"))
			#shinyjs::show(id = "Reads_Database_Box_wrapper")
			shinyjs::delay(ms = 100,shinyjs::show(id = "Comparison_Box_wrapper"))
			

      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4ReadsSelectInput","#h4file1","#h4Sample_Select_Box","#h4ReadsDataUser","#h4ReadsDataset","#h4ReadsRawNorm","#h4ReadsLoadData","#h4Comparison","#h4CompareButton","#h4Log2FCThreshold","#h4FDRPvalThreshold","#h4PvalCorrection","#h4ReadsPlotWidths","#h4ReadsVolcanoGenes","#h4ReadsVolcanoFontSize","#h4ReadsHeatmapFontSize","#h4ReadsHeatmapSamples"),
                   intro = c("Select whether you will upload a Reads/Counts file or select one from the database.",
                             "If you selected 'Upload File' use this button to browse for a file.",
                             "If you upload a file, select the samples you would like to include in the analysis. The maximum number of samples for this function is limited to maintain a manageable load for the server.",
                             "If you selected 'Database' Choose a 'Data Library'.",
                             "If you selected 'Database' Choose a dataset from the above 'Data Library'.",
                             "Select whether the files Reads/Counts are raw or normalized.",                             
					    "Click 'Load Data' to load the selected Reads/Counts data.",
					    "Select two samples for differential expression analysis.",
					    "Click 'Compare Samples' to perform differential expression analysis.",
					    "Change Log2 foldchange threshold.",
					    "Change p-value threshold for statistically signficant differentially expressed genes.",
					    "Change p-value correction method.",
					    "Adjust plot widths then click 'Compare Samples' button again.",
					    "Select number of top differentially expressed genes to show on volcano plot.",
					    "Adjust font size on volcano plot.",
					    "Adjust font size on heatmaps then click 'Compare Samples' button again.",
					    "Select if heatmaps comparing differentially expressed genes should display the two compared samples only or all the samples in the dataset."	
                             
                             ))#intro = c(
      ))#rintrojs::introjs(session, options = list(

    }#if (input$TABS == "Count_EdgeR_Data") {
  })#observeEvent(input$intro, {

  
  observeEvent(input$Expression_load_help,{
  if (input$TABS == "Expression_Data") {    
  			
  			shinyjs::show(id = "FPKM_Database_Box_wrapper")
			shinyjs::show(id = "file_RPKM_Box_wrapper")	

  	
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4SelectInput", "#h4FileInput","#h4DataUser","#h4DataSet","#h4LoadFile"),
                           intro = c("Select whether you will upload an expression file or select one from the database.",
                                     "If you selected 'Upload File' use this button to browse for a file.",
                                     "If you selected 'Database' Choose a 'Data Library'.",
                                     "If you selected 'Database' Choose a Dataset from the above 'Data Library'.",
                                     "Click 'Load File' to load the selected expression data."
                                     
                                     ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "Expression_Data") { 	
  })#observeEvent(input$Expression_load_help,{

  observeEvent(input$Create_Groups_load_help,{
  if (input$TABS == "Create_Groups") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4SelectGroupMembers", "#h4GroupName","#h4CreateGroup","#h4Groups","#h4DownloadGroups","#h4UploadGroupsFile","#h4UploadGroup","#h4ConfirmGroup"),
                           intro = c("First, Select multiple samples to group together.",
                                     "Type the Name of your group (no spaces or special charactes except '_').",
                                     "Click 'Create Group' to create the group and load it into the application.",
                                     "The group will appear here in the format 'Group_Name:Group_member1;Group_member2;'.",
                                     "You can download the groups into a txt file on your local computer so you won't have to manually create the groups next time.",
                                     "To upload a file with saved groups, use the 'Browse' button to select the file.",
                                     "Click 'Upload Groups' to upload the file into the text box.",
                                     "Click 'Confirm Groups' to create the uploaded groups and load them into the various tools of the application. You can also manually type groups or edit groups in the format 'Group_Name:Group_member1;Group_member2' with each group being on a new line. Then, click 'Confirm Groups' to create the groups and load them into the application."
                                    
                                     
                                     ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "Create_Groups") { 	
  })#observeEvent(input$Create_Groups_load_help,{

 observeEvent(input$Groups_CountData_help,{
  if (input$TABS == "Groups_CountData") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4GroupsControl", "#h4GroupsTreatment", "#h4GroupsCompareButton", "#h4GroupsLog2FCThreshold", "#h4GroupsFDRPvalThreshold", "#h4GroupsPvalCorrection", "#h4GroupsReadsPlotWidths", "#h4GroupsReadsVolcanoGenes", "#h4GroupsReadsVolcanoFontSize","#h4GroupsReadsHeatmapFontSize","#h4GroupsReadsHeatmapSamples"),
                           intro = c("Select a group for differential gene expression analysis (control).",
                                     "Select another group for differential gene expression analysis.",
                                     "Click 'Compare Groups' to perform differential gene expression analysis.",
                                     "Change Log2 foldchange threshold.",
					    		  "Change p-value threshold for statistically signficant differentially expressed genes.",
					    		  "Change p-value correction method.",
					    		  "Adjust plot widths then click 'Compare Samples' button again.",
					    		  "Select number of top differentially expressed genes to show on volcano plot.",
					    		  "Adjust font size on volcano plot.",
					    		  "Adjust font size on heatmaps then click 'Compare Samples' button again.",
					    		  "Select if heatmaps comparing differentially expressed genes should display the two compared groups only or all of the groups."				                                       
                                     ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "Groups_CountData") { 	
  })#observeEvent(input$Groups_CountData_help,{

 observeEvent(input$Sample_Comparison_help,{
  if (input$TABS == "Sample_Comparison") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4SingleCompareGeneList", "#h4SinglePasteGeneList","#h4SingleCompareConditions",  "#h4CompareSampleExp", "#h4SampleGraphWidth", "#h4SampleFontSize", "#h4SingleColorList", "#h4Colorpicker", "#h4IncludeColor","#h4ingleThemeList","#h4SingleXaxisangle"),
                           intro = c("Select genes from this list to view their expression in one or more samples. If a gene is not in the loaded dataset or there is no expression in all samples then it is excluded from this list.",
                                     "Users can also type or paste gene names/lists here.",
                                     "Select samples from this list to evaluate",
                                     "Click 'Compare Gene Expressions' to generate plots below displaying gene expressions in the selected samples.",
					    		  "Adjust the widths of the plots with this control.",
					    		  "Adjust the font size of the plots with this control.",
					    		  "Customize the colors in the plots by selecting colors from this list.",
					    		  "Users can also use this 'color picker' to select a color.",
					    		  "Add the color you selected with the 'color picker' to the list by clicking 'Include Selected Color'.",
					    		  "Adjust the theme or background of the plots by selecting a different theme from this list.",
					    		  "Adjust the angle of the X-axis text of the plots with this control."				                                       
                                     ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "Sample_Comparison") { 	
  })#observeEvent(input$Sample_Comparison_help,{

 observeEvent(input$Group_Comparison_help,{
  if (input$TABS == "Group_Comparison") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4GroupCompareGeneList", "#h4GroupPasteGeneList","#h4GroupCompareCondition",  "#h4CompareGroupExp", "#h4GroupGraphWidth", "#h4GroupFontSize", "#h4GroupColorList", "#h4GroupColorpicker", "#h4GroupIncludeColor","#h4GroupThemeList","#h4GroupXaxisangle"),
                           intro = c("Select genes from this list to view their expression in one or more Groups. If a gene is not in the loaded dataset or there is no expression in all samples then it is excluded from this list.",
                                     "Users can also type or paste gene names/lists here.",
                                     "Select groups from this list to evaluate",
                                     "Click 'Compare Gene Expressions' to generate plots below displaying gene expressions in the selected Groups.",
					    		  "Adjust the widths of the plots with this control.",
					    		  "Adjust the font size of the plots with this control.",
					    		  "Customize the colors in the plots by selecting colors from this list.",
					    		  "Users can also use this 'color picker' to select a color.",
					    		  "Add the color you selected with the 'color picker' to the list by clicking 'Include Selected Color'.",
					    		  "Adjust the theme or background of the plots by selecting a different theme from this list.",
					    		  "Adjust the angle of the X-axis text of the plots with this control."				                                       
                                     ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "Group_Comparison") { 	
  })#observeEvent(input$Group_Comparison_help,{

 observeEvent(input$GSEA_help,{
  if (input$TABS == "GSEA") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4GSEA_GroupvSample", "#h4ControlGSEATest","#h4QueryGSEATest", "#h4GseaTopHits", "#h4GseaBottomHits", "#h4GSEADatasetList", "#h4RunEnrichedSignatures", "#h4GSEAGeneSet", "#h4GSEAGenelimitslider"),
                           intro = c("Select whether you are comparing samples or groups. Groups must be created in the 'Create Groups' tool to use groups.",
                                     "Select the first sample/group to include in the anaylsis",
                                     "Select the second sample/group to include in the anaylsis",
					    		  "Select the number of top enriched upregualted gene sets to display in the output",
					    		  "Select the number of most downregulated (enriched in other sample/group) gene sets to display in the output. ",
					    		  "Select the Collections to search for encriched gene sets from this list.",
					    		  "Click 'Find Enriched Gene Signatures' to perform GSEA analysis and find top hits.",
					    		  "After running 'Find Enriched Gene Signatures' you can select a different gene set to view GSEA enrichment plots of the various hits.",
					    		  "Adjust the number of genes from the selected genes set above represented in the heatmaps and barplots with this control."			                                       
                                     ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "GSEA") { 	
  })#observeEvent(input$GSEA_help,{

 observeEvent(input$Gene_Ontology_help,{
  if (input$TABS == "Gene_Ontology") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4OrganismCheckbox", "#h4OGeneList","#h4SelectInitialGO", "#h4GOPvalThreshold", "#h4GOPvalCorrection", "#h4GOLabelsx", "#h4GOGeneListButton", "#h4OntologyMethod","#h4RepresentedGenes","#h4SelectGO","#h4SelectGOButton","#h4PreviousGOButton"),
                           intro = c("Select whether searech gene ontologies of genes related to humans, mice, or both.",
                                     "Type or paste gene list.",
                                     "Select whether to evaluate curated ontologies that are related to molecular functions, biological processes, or cellular components.",
					    		  "Change p-value threshold.",
					    		  "Adjust p-value correction method.",
					    		  "to display gene ontologies by ID or name in barplot.",
					    		  "Click 'Get Ontologies' to perform GO analysis.",
					    		  "Include gene ontologies annotated/currated to genes by these methods.",
					    		  "The Genes represented in the results barplot and above subcategories would be listed here.",
					    		  "Select a GO to explore the related genes and nested gene ontologies then click 'Select GO'.",
					    		  "Click 'Select GO' as stated in previous step.",
					    		  "When searching through nested ontology bar plots, use this 'back button' to return to the previous plot."			                                       
                                   ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "GSEA") { 	
  })#observeEvent(input$GSEA_help,{							    		   

 observeEvent(input$Group_Stats_Summary_plots_help,{
  if (input$TABS == "Group_Stats_Comparison" & input$Group_Stats_Comparison_Panels == "Group_Stats_Summary_plots") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4Group1StatCompare", "#h4Group2StatCompare","#h4StatTestSelect", "#h4GroupStatAnalyisButton", "#h4GroupStatPValThresh", "#h4GroupStatRPKMThresh", "#h4GroupStatFCThresh", "#h4GroupStatDisplayTopGraphs","#h4GroupStatListFC"),
                           intro = c("Select the first group to evaluate for distinctly expressed genes",
                                     "Select the second group to evaluate for distinctly expressed genes.",
                                     "Select statistical tests to apply to the groups. Only genes with statistically signficiant p-values on all selected tests will be listed in results.",
					    		  	 "Click 'Find Genes' to start analysis.",
					    		  	 "Adjust p-value threshold for all selected statistical tests selected above.",
					    		     "Change minimum expression value of gene by at least group.",
					    		     "Change the minimum fold change threshold between groups.",
					    		     "Change the number of displayed top hits in results.",
					    		     "Select to print log fold change with genes on result list."					    		     		                                       
                                   ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "Group_Stats_Comparison" & input$Group_Stats_Comparison_Panels == "Group_Stats_Summary_plots") {  
  })#observeEvent(input$Group_Stats_Summary_plots_help,{	

 observeEvent(input$t_SNE_help,{
  if (input$TABS == "Group_Stats_Comparison" & input$Group_Stats_Comparison_Panels == "PCA_tSNE_UMAP") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4SelectTSNEObject", "#h4GroupStatsTSNEMaxIter","#h4GroupStatsRunTSNE", "#h4GroupStatsTSNESeed", "#h4GroupStatsTSNEPointSize", "#h4GenerateTSNEList", "#h4GroupStatsTSNEGeneList", "#h4GroupStatsTSNEGene","#h4GroupStatsTSNEGroup"),
                           intro = c("Select whether to perform t-SNE on samples based on gene expression or genes based on expression pattern among samples.",
                                     "Select the maximum iterations for the t-SNE function.",
                                     "Click 'Run' to perform the t-SNE analysis. t-SNE plots will be generated below.",
					    		  	 "Change 'seed' to change t-SNE plot.",
					    		  	 "Adjust the point size on the t-SNE plot.",
					    		     "Check to generate p-values representing relative distance of points from each other. This functions takes a long time to perform depending on the dataset.",
					    		     "Generate t-SNE plot of samples calculated using expression of genes on this list, or generate a t-SNE plot of a limited list of genes.",
					    		     "If running a t-SNE plot of genes, the user can enter a gene to highlight it on the plot.",
					    		     "This option allows the highlighting of all samples that belong to a selected defined group."					    		     		                                       
                                   ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "Group_Stats_Comparison" & input$Group_Stats_Comparison_Panels == "PCA_tSNE_UMAP") {  
  })#observeEvent(input$t_SNE_help,{	

  
 observeEvent(input$Expressed_Ontologies_help,{
  if (input$TABS == "Expressed_Ontologies") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4GOGroupVSample", "#h4ControlGOTest","#h4SubjectsGOTest", "#h4SelectOntologies", "#h4OntologyList", "#h4SelectOntologiesCombie", "#h4OntologyCombine", "#h4GOAnalysis","#h4GOOverlayVSide","#h4LabelGOGene","#h4GOTextFClimit","#h4GOMinRPKM","#h4GODistaxislimit","#h4GOntologyMethod2Compare","#h4GraphWidthFont"),
                           intro = c("Select whether to compare expression of genes related to specific curated ontologies between samples or groups.",
                                     "Select the control sample or group to compare.",
                                     "Select other samples or groups to compare to the control.",
					    		  	 "Select a GO class (Molecular Functions or Cellular Components) or a biological process class to populate the 'Select GO' list.",
					    		  	 "Select gene ontologies to compare between samples/groups.",
					    		     "Select a GO class (Molecular Functions or Cellular Components) or a biological process class to populate the 'Combine GO with' list. If gene ontologies are selected from the 'Combine GO with' list, only genes that occur both in the primary GO selected from the 'select GO list' that are also represented in the secondary GO from the 'Combine GOs with' list will be compared.",
					    		     "If gene ontologies are selected from the 'Combine GO with' list, only genes that occur both in the primary GO selected from the 'select GO list' that are also represented in the secondary GO from the 'Combine GOs with' list will be compared.",
					    		     "Click 'Get GO Gene's Fold Change' to start the analysis.",
					    		     "Choose whether data points representing different groups should be overlayed or side by side.",
					    		     "Option to label points on plots",
					    		     "Choose fold change threshold of the points you want to label (for example, if you only want to label points with an absolute fold change greater than 2, adjust the slider to 2)."	,
					    		     "Choose the minimum Expression value of genes to be considered in this analysis",
					    		     "Choose the range of Expression values to view on the distribution plots",
					    		     "Include gene ontologies annotated/currated to genes by these methods.",
					    		     "Adjust the widths and font sizes of the plots."
					    		     	    		     		                                       
                                   ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "Expressed_Ontologies")  {  
  })#observeEvent(input$Expressed_Ontologies_help,{	

 observeEvent(input$Diff_Expressed_Ontologies_help,{
  if (input$TABS == "Diff_Expressed_Ontologies") {   
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4DEOGOGroupVSample", "#h4DEOControlGOTest","#h4DEOSubjectsGOTest","#h4DEOGOClass","#h4DEOOntologyKeywordSelect", "#h4DEOGOAnalysis", "#h4DEOGOTextFClimit", "#h4DEOBaseThresh","#h4DEOabsFCThresh","#h4DEOPvalThresh","#h4DEOGODistAxislimit","#h4DEOOntologyMethod2Compare","#h4DEOGraphWidthFont"),
                           intro = c("Select whether to find differentially expressed gene ontologies between samples or groups.",
                                     "Select the first sample or group to compare.",
                                     "Select other sample or group to compare to the control.",
								"Select a GO class to evaulate if any GO related to that class is differentially expressed",
					    		  	 "Choose keywords for ontologies to search and compare. This allows the function to compare gene ontologies related to selected key words to reduce the search space and save time.",
					    		  	 #"In addition to selecting a keyword from the list above, the user can type in keywords seperated by ';'.",
					    		     "Click 'Find Differentially Expressed GOs' to start the analysis.",
					    		     "Choose fold change threshold of the points you want to label (for example, if you only want to label points with an absolute fold change greater than 2, adjust the slider to 2).",
					    		     "Choose the minimum Expression value of genes to be considered in this analysis.",
					    		     "Select the minimum fold change to report in the results.",
					    		     "Select the maximum p-value threshold.",	
					    		     "Choose the range of Expression values to view on the distribution plots.",
					    		     "Include gene ontologies annotated/currated to genes by these methods.",
					    		     "Adjust the widths and font sizes of the plots."			    		     		                                       
                                   ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
    }#else if (input$tabs == "Diff_Expressed_Ontologies") {  
  })#observeEvent(input$Diff_Expressed_Ontologies_help,{	
	
		
 observeEvent(input$Session_Info_help,{
   rintrojs::introjs(session, options = list(
     steps = data.frame(element = c("#h4Session_ID","#h4Session_ID","#h4Session_ID", "#h4UploadSessionFile","#h4ConfirmUpload","#h4DownloadSession"),
                        intro = c("Enter the Session ID that you would like to restore.",
                                  "Each unique session ID consists of the user's IP address, the date, and an iterative number (1,2,3, …).    XXX.XX.XXX.XXX_YYYY.MM.DD_I",
                                  "For example:     159.17.216.157_2023.01.17_2     (Then press the 'Restore Session' button)",
                                  "If a session file was downloaded to the computer (rdata), search and upload it.",
                                  "Confirm the upload and load the data into the app.",
                                  "Download your session to your local computer. Sessions saved on the server are stored temporarily and will be removed often."
                                  
                        ))#steps = data.frame(element = c(
   ))#rintrojs::introjs(session, options = list(
 })#observeEvent(input$Session_Info_help,{	
 
		
 observeEvent(input$ROGUE_help,{
      rintrojs::introjs(session, options = list(
        steps = data.frame(element = c("#h4LoadReadsFirst", "#h4LoadExpressionFirst","#h4ExploreData"),
                           intro = c("If your RNAseq data is in raw reads or counts, click the 'Load Count Data:(EdgeR)' in the left menu bar to convert to RPKM and perform differential gene expression analysis.",
                                     "If your RNAseq data is processed expression tables (FPKM,RPKM,TPM), click on the 'Load Expression Data' in the left menu bar to load your data.",
                                     "After your data is loaded, explore the tools in the menu bar to explore your data. Each Tool has a '?' help button at the top with tips and instructions"
		    		     		                                       
                                   ))#steps = data.frame(element = c(
      ))#rintrojs::introjs(session, options = list(
  })#observeEvent(input$Diff_Expressed_Ontologies_help,{	
	
				

 
		
	
#####################################
#####################################
#output$User_Data_File
  #####################################
  #####################################      

  observe({ 

    if(input$TABS == "Sample_Comparison")
      {
      	if(!is.null(DATA.Values.5min))
      	{
      		if(!is.null(nrow(DATA.Values.5min)))
      		{
      			if(any(Gene.Choices != rownames(DATA.Values.5min)) | length(Gene.Choices)!= nrow(DATA.Values.5min))
      			{    		
      				Gene.Choices <<- as.character(rownames(DATA.Values.5min))       				
      				updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes (max:12)", choices = Gene.Choices,selected = NULL)
      				updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes (max:12)", choices = Gene.Choices,selected = NULL)
      				alert("Data is Loading... Please wait")	     				
      		}#if(any(Gene.Choices != rownames(DATA.Values.5min)) | length(Gene.Choices)!= nrow(DATA.Values.5min))
      	}#if(!is.null(nrow(DATA.Values.5min)))
      }#if(!is.null(nrow(DATA.Values.5min)))
      
    } else if(input$TABS == "Group_Comparison")
      {
          if(!is.null(DATA.Values.5min))
      	{
      		if(!is.null(nrow(DATA.Values.5min)))
      		{
      			if(any(Gene.Choices != rownames(DATA.Values.5min)) | length(Gene.Choices)!= nrow(DATA.Values.5min)) {    				
      				Gene.Choices <<- as.character(rownames(DATA.Values.5min))       				    			
      				updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes (max:12)", choices = Gene.Choices,selected = NULL)
      				updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes (max:12)", choices = Gene.Choices,selected = NULL)
      				alert("Data is Loading... Please wait")	
      			}#if(!is.null(nrow(DATA.Values)))
      		}#if(!is.null(DATA.Values))
      	
    		}#if(input$TABS == "Gene Comparison (Groups)){
    	} else if(input$Groups != PRE_GROUPS & nchar(input$Groups)==0 )#if(input$TABS == "Gene Comparison (Groups)")
	 {
		 #obs.Confirm.Group.button()
                 updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=PRE_GROUPS)
	 }#
  })#observe({

#############################
#############################################
obs.Include.Color <- observeEvent(input$Include.Color,ignoreInit = TRUE,{
	isolate(ALL.COLORS <<- c(ALL.COLORS,input$Color_picker))
	#selectInput(inputId="Single.Color.List",label="Select Colors:", ALL.COLORS,selected=c(input$Single.Color.List,input$Color_picker),multiple=T)) 
	isolate(updateSelectInput(session, "Single.Color.List",label="Select Colors:", ALL.COLORS,selected=c(input$Single.Color.List,input$Color_picker)))	
	})
	
obs.Group.Include.Color <- observeEvent(input$Group.Include.Color,ignoreInit = TRUE,{
	isolate(ALL.COLORS <<- c(ALL.COLORS,input$Group.Color_picker))
	isolate(updateSelectInput(session, "Group.Color.List",label="Select Colors:", ALL.COLORS,selected=c(input$Group.Color.List,input$Group.Color_picker)))
	})

  obs <- observe({    
    

    Dataset.files = list.files(paste(Data.folder,"/",input$Data_User,sep=""))
    Dataset.files = subset(Dataset.files,(substr(Dataset.files,(nchar(Dataset.files)-4),nchar(Dataset.files))!=".info" ))
    Dataset.files = subset(Dataset.files,(substr(Dataset.files,(nchar(Dataset.files)-10),nchar(Dataset.files))!="mData.RData" ))
    data.file.select = input$Data_set
    updateSelectizeInput(session, inputId="Data_set", label = "Select Dataset", choices = Dataset.files,selected = data.file.select)
  

    Reads.Dataset.files = list.files(paste(Reads.Data.folder,"/",input$Reads.Data_User,sep=""))
    Reads.Dataset.files = subset(Reads.Dataset.files,(substr(Reads.Dataset.files,(nchar(Reads.Dataset.files)-4),nchar(Reads.Dataset.files))!=".info" ))
    Reads.Dataset.files = subset(Reads.Dataset.files,(substr(Reads.Dataset.files,(nchar(Reads.Dataset.files)-10),nchar(Reads.Dataset.files))!="mData.RData" ))
    Reads.data.file.select = input$Reads.Data_set
    updateSelectizeInput(session, inputId="Reads.Data_set", label = "Select Dataset", choices = Reads.Dataset.files,selected = Reads.data.file.select)

  
   
  })
  
  
############################
  ##########################
###########################  



  
  #####################################
  #####################################  
  obs.Load_File.Group.button <- observeEvent(input$Load_File,ignoreInit = TRUE,{      
    #####################################
    #####################################  
    #####################
   
    disable("Load_File")
    
    #isolate({ 
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Processing Data", value = 0)
    # Increment the progress bar, and update the detail text.
    progress$inc(0.3, detail = "Please Wait")
    Sys.sleep(0.001)
    ################



    inp_source <- input$Select_input


   (DATASET_SELECT = input$Data_set)

    #if(input$Select_input=="dbase" & nchar(DATASET_SELECT) > 0){isolate(Data_File_path <<- paste(Data.folder,"/",input$Data_User,"/",DATASET_SELECT,sep=""))}
    if(input$Select_input=="dbase"){isolate(Data_File_path <<- paste(Data.folder,"/",input$Data_User,"/",DATASET_SELECT,sep=""))}
    if(input$Select_input=="upload"){
      inFile.rpkm <- input$file_RPKM
      if (is.null(inFile.rpkm)) Data_File_path <<- ""
      if (!is.null(inFile.rpkm)) Data_File_path <<- inFile.rpkm$datapath}

    File.Check = c(Data_File_path)

    if(substr(toupper(Data_File_path),nchar(Data_File_path)-5,nchar(Data_File_path)) != ".RDATA")
    {
  
      if(file.exists(Data_File_path) & !dir.exists(Data_File_path)) {File.Check = readLines(Data_File_path)}
      
      if(length(File.Check)==0 | nchar(Data_File_path)==0 | is.null(Data_File_path) | dir.exists(Data_File_path))
      {
        enable("Load_File")
        return(NULL)
      }
      
       
       if(length(File.Check) < 2)
       {
         enable("Load_File")
         shinyalert(title = "File format error",text = "Check file format\n\nPlease use a TAB-delimited file with the below format:\n\n Genes  Sample1  Sample2\nGene1 Value_1 Value_2\nGene2 Value_1 Value_2", type = "error")
         return(NULL)
       }
   
       
       File.Check_columns = sapply(File.Check,function(X) length(strsplit(X,split = "\t")[[1]]))
  
  	 if(!all(File.Check_columns == File.Check_columns[1] & File.Check_columns[1] >= 2))
  	 {
  	   enable("Load_File")
  	   shinyalert(title = "ERROR: Check file format\n\nPlease use a TAB-delimited file with the below format:\n\n Genes  Sample1  Sample2\nGene1 Value_1 Value_2\nGene2 Value_1 Value_2", type = "error")
         return(NULL)
  	 }
    }#if(substr(toupper(Data_File_path),nchar(Data_File_path)-5,nchar(Data_File_path)) != ".RDATA")

    progress$inc(0.5, detail = "Please Wait")
    Sys.sleep(0.001)


    Group_Count <<- 0
    Groups <<- c()
    Group.Members <<- c()


     DATA1=c("")

     if(nchar(input$Data_set)>1 | length(File.Check)!=0 ) {


    readData <<- c()
    Groups <<- c()
    Group.Members <<- c()
    PRE_GROUPS <<- ""
    updateSelectizeInput(session, inputId="All_Conditions", label = "Select Dataset", choices =c(""),selected = NULL)
    updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes (max:12)", choices = c(""),selected = NULL)
    updateSelectizeInput(session, inputId="Single.Compare.Conditions", label = "Select Samples", choices = c(""),selected = NULL)
    updateSelectizeInput(session, inputId="Single.Control", label = "Select Control", choices = c(""),selected = NULL)
    updateSelectizeInput(session, inputId="Single.Treatment", label = "Select Treatment", choices = c(""),selected = NULL)
    updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes (max:12)", choices = c(""),selected = NULL)
    updateTextAreaInput(session,inputId = "Groups",label = "Groups",value="")
    updateSelectizeInput(session, inputId="All_Conditions",selected = "")
    updateSelectizeInput(session, inputId="Group.Compare.Condition", label = "Select Groups", choices = c(""),selected = NULL)
    updateTextAreaInput(session,inputId="Gene_Table", label = paste("Gene Table"), value = "")
    updateTextAreaInput(session,inputId="FPKM_Table", label = paste("Reads Table"), value = "")
    updateSelectizeInput(session, inputId="Comparison", label = "Select Library", choices = c(""))
    updateTextAreaInput(session,inputId="GeneList_Result", label = paste("Differentially Expressed Genes"), value = "")
    updateTextAreaInput(session,inputId="Groups.GeneList_Result", label = paste("Differentially Expressed Genes"), value = "")
    updateSelectizeInput(session,inputId="Groups.Control", label="Select Group 1",choices = c(""), selected = NULL)
    updateSelectizeInput(session,inputId="Groups.Treatment", label="Select Group 2",choices = c(""), selected = NULL)
    updateSelectizeInput(session, inputId = "Control_GSEA_Test",label = "Select Control",choices = c(""), selected = NULL)
    updateSelectizeInput(session, inputId = "Query_GSEA_Test",label = "Select Query",choices = c(""), selected = NULL)

    
    output$MDSPlot <- renderPlotly(plot_ly())
    output$ComparePlot1 <- renderPlotly(plot_ly())
    output$ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Groups.ComparePlot1 <- renderPlotly(plot_ly())
    output$Groups.ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Groups.ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Groups.ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Groups.ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Groups.ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Groups.ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Groups.ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Single.Sample.barplot.label <- renderText({" "})
    output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Single.Sample.heatmap.label <- renderText({" "})
    output$Single.Sample.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Stacked_Plot.label <- renderText({" "})
    output$Stacked_Plot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$MDS_PLOT.Samples.label <- renderText({" "})
    output$MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Single.Sample.barplot.label <- renderText({" "})
    output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.boxplot.label <- renderText({" "})
    output$Group.boxplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.barplot.label <- renderText({" "})
    output$Group.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.barplot.sem.label <- renderText({" "})
    output$Group.barplot.sem <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.barplot.maxmin.label <- renderText({" "})
    output$Group.barplot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.heatmap.label <- renderText({" "})
    output$Group.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.members.heatmap.label <- renderText({" "})
    output$Group.members.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.stackedPlot.label <- renderText({" "})
    output$Group.stackedPlot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.stackedPlot.maxmin.label <- renderText({" "})
    output$Group.stackedPlot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.MDS_PLOT.Samples.label <- renderText({" "})
    output$Group.MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$GSEA_PLOT_1 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$GSEA_PLOT_2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    
    
    #isolate(Reads.data.file.name <- Reads.data.file.name())
 

     
     #if(length(RPKM_FILE_LIST)==0)
     {
       if(substr(toupper(Data_File_path),nchar(Data_File_path)-3,nchar(Data_File_path)) == ".RDA" | substr(toupper(Data_File_path),nchar(Data_File_path)-5,nchar(Data_File_path)) == ".RDATA")
       {
       	 RDA_DATA = load(Data_File_path)
       	 eval(expr = parse(text = paste("DATA.RAW =", RDA_DATA[1])))
		 rm(RDA_DATA)
		 
		 SAMPLE_NAMES = colnames(DATA.RAW)
		 DATA.RAW = data.frame(rownames(DATA.RAW), DATA.RAW)
		 colnames(DATA.RAW ) = c("gene_names", SAMPLE_NAMES)
		# rm(SAMPLE_NAMES)

	 if(file.exists(gsub("data.RData","mData.RData",Data_File_path)))
    {
		RDA_metaDATA = load(gsub("data.RData","mData.RData",Data_File_path))
		eval(expr = parse(text = paste("metaDATA.RAW =",RDA_metaDATA[1])))
		
		Group.inx = which(apply(metaDATA.RAW,MARGIN=2, function(X) length(unique(X)) < min(c(20,(nrow(metaDATA.RAW)/2))) ))
		ID.inx = which(apply(metaDATA.RAW,MARGIN=2, function(X) length(which(SAMPLE_NAMES %in% (X))) == length(SAMPLE_NAMES)) ) 

		if(length(Group.inx) >= 1 & length(ID.inx) >= 1)
		{
			GROUP_Data = c()
			for(G in 1:length(Group.inx))
			{
			    GROUPS = unlist(unique(metaDATA.RAW[Group.inx[G]]))
			    for(K in 1:length(GROUPS))
			    {
			        Group_Name = gsub(" ","_",paste(colnames(metaDATA.RAW)[Group.inx[G]],GROUPS[K],sep = "_"))
			        Group_Members = metaDATA.RAW[,ID.inx[1]][which(metaDATA.RAW[,Group.inx[G]] %in% GROUPS[K])]
			        Group_Members = gsub("-",".",Group_Members)
			        GROUP_Data = c(GROUP_Data,paste(Group_Name,":",paste(Group_Members,collapse = ";"),sep=""))
			    }#for(K in 1:length(GROUPS))
			}#for(G in 1:length(Group.inx))
			PRE_GROUPS <<- paste(GROUP_Data,collapse = "\n")

			#obs.Confirm.Group.button()
		       # updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=PRE_GROUPS)
			
		}#if(length(Group.inx) >= 1 & length(ID.inx) >= 1)   
	 }#if(file.exists(gsub("data.RData","mData.RData",Data_File_path)))
       }#if(substr(toupper(Data_File_path),nchar(Data_File_path)-3,nchar(Data_File_path)) == ".RDA" | substr(toupper(Data_File_path),nchar(Data_File_path)-5,nchar(Data_File_path)) == ".RDATA")
       
       if(substr(toupper(Data_File_path),nchar(Data_File_path)-3,nchar(Data_File_path)) != ".RDA" & substr(toupper(Data_File_path),nchar(Data_File_path)-5,nchar(Data_File_path)) != ".RDATA")
       {
       	DATA.RAW <- read.delim(Data_File_path,header=T,sep="\t")
       }#if(substr(toupper(Data_File_path),nchar(Data_File_path)-3,nchar(Data_File_path)) == ".txt" | substr(toupper(Data_File_path),nchar(Data_File_path)-5,nchar(Data_File_path)) == ".text")

     }##if(length(RPKM_FILE_LIST)==0)
    
    
  	if(is.null(nrow(DATA.RAW)) | is.null(nrow(DATA.RAW)))
  	{
  	  enable("Load_File")
      return(NULL) 
  	} 
       

      GeneList <<- DATA.RAW[,1]
      DATA.temp =data.frame(lapply(DATA.RAW[,2:ncol(DATA.RAW)], as.character), stringsAsFactors=FALSE)
      
      
      duplicate.genes = unique(GeneList[which(duplicated(GeneList))])
      Progress.value = ceiling(length(duplicate.genes)/20)
      
      
      if(length(duplicate.genes) > 0){
        for(x in 1:length(duplicate.genes))
        {
          
          if((x %% Progress.value)==0)
          {
            
            progress$inc(0.5+((x/Progress.value)/100), detail = "Please Wait - still working")
            Sys.sleep(0.001)
          } 
          
          temp = DATA.temp[which(as.character(GeneList)==as.character(duplicate.genes[x])),]
          temp = data.frame(lapply(temp, as.numeric), stringsAsFactors=FALSE)
          temp[is.na(temp)] <- 0
          temp.sum = colSums(temp)
          DATA.temp[min(which(as.character(GeneList)==as.character(duplicate.genes[x]))),] = temp.sum
        }
        DATA.temp = DATA.temp[which(!duplicated(GeneList)),]
        GeneList <<- GeneList[which(!duplicated(GeneList))]
        #which(c(2,3,4,5,2,3,5,2)==2)
      }
      #DATA.temp = data.matrix(DATA.RAW[,2:ncol(DATA.RAW)])
      #indxx = suppressWarnings(which(is.na(as.numeric(DATA.temp))))
      DATA.temp =data.frame(lapply(DATA.temp, as.numeric), stringsAsFactors=FALSE)
      DATA.temp[is.na(DATA.temp)] <- 0
      #DATA.temp[indxx] = 0
      #DATA.temp = DATA.temp[which(apply(DATA.temp,MARGIN=1,function(X) any(X>=5))), ]     
 
      #indxx = suppressWarnings(which(is.na(as.numeric(DATA.temp))))
      #DATA.temp = data.frame(lapply(DATA.temp, as.numeric), stringsAsFactors=FALSE)
      
      
      #DATA.temp[indxx] = 0
      #DATA.temp = data.matrix(DATA.temp)
      #colnames(DATA.temp) = colnames(DATA.RAW)[1:ncol(DATA.RAW)]
      #rownames(DATA.temp) = rownames(DATA.RAW)
      isolate(DATA.Values <<- DATA.temp)
    }
    
    # return(Data_File_path)
    #})
    
    progress$inc(0.7, detail = "Please Wait")
    Sys.sleep(0.001)
    #####################################
    #####################################
    #obs <- output$distPlot
    #####################################
    #####################################      
    
   
    if(nchar(input$Data_set)>1 | length(File.Check)!=0)
    {
          
    	d = dist(t(DATA.Values[,1:ncol(DATA.Values)]))
    	fit <- isoMDS(d, k=2)

    	MDS_Data = data.frame(X = fit$points[,1], Y = fit$points[,2],Samples = colnames(DATA.Values[,1:ncol(DATA.Values)]))
	
    	MDS_Distance.plot = ggplot(data=MDS_Data,aes(x=X, y=Y)) +
    	  geom_text(label=MDS_Data$Samples)+
	  theme_bw()+
    	  ggtitle("Nonmetric MDS")

	Report.List.Reads <<- c(Report.List.Reads,list(MDS_Distance.plot))

	MDS_Distance.plot.points = ggplot(data=MDS_Data,aes(x=X, y=Y, col = Samples)) +
          geom_point()+
          theme_bw()+
          ggtitle("Nonmetric MDS")

        Report.List.Reads <<- c(Report.List.Reads,list(MDS_Distance.plot.points))
   }#if(nchar(input$Data_set)>1 | length(File.Check)!=0)
    
    shinyjs::show(id = "distPlot_div")  
    output$distPlot <- renderPlot({
      
      
      if(nchar(input$Data_set)>1 | length(File.Check)!=0)
      {
      
	MDS_Distance_distPlot = MDS_Distance.plot
	save(list = c("MDS_Distance_distPlot"), file=paste(Backup_Session_Folder,"/distPlot.rdata",sep=""))  
 
        MDS_Distance.plot

      }#if(nchar(input$Data_set)>1 | length(File.Check)!=0)
      # generate bins based on input$bins from ui.R
      
    }) ##  output$distPlot <- renderPlot({
    
    #####################################
    #####################################
    #obs <- observe
    #####################################
    #####################################       
    #obs <- observe({
    
    progress$inc(0.85, detail = "Please Wait")
    Sys.sleep(0.001)
    
    
    if(nchar(input$Data_set)>1 | length(File.Check)!=0)
    {
      DATA.Values.5min <<- DATA.Values[which (GeneList != "-"), ]
      GeneList <<- GeneList[which (GeneList != "-")]
      GeneList <<- GeneList[apply(as.matrix(DATA.Values.5min), MARGIN = 1, function(x) any(x >= 5))]
      DATA.Values.5min <<- DATA.Values.5min[apply(as.matrix(DATA.Values.5min), MARGIN = 1, function(x) any(x >= 5)), ]
      #DATA.Values.5min[DATA.Values.5min=="FPKM"] <<- 0
      #DATA.Values.5min[DATA.Values.5min>100000] <<- 0
      
      rownames(DATA.Values.5min) <<- GeneList
      #Gene.Choices = as.character(GeneList)
      #Gene.Choices = as.matrix(unique(rownames(DATA.Values.5min)))
      updateSelectizeInput(session, inputId="All_Conditions", label = "Select Dataset", choices = colnames(DATA.Values[,1:ncol(DATA.Values)]),selected = NULL)
      #updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes (max:12)", choices = Gene.Choices,selected = NULL)
      updateSelectizeInput(session, inputId="Single.Compare.Conditions", label = "Select Samples", choices = c("ALL",colnames(DATA.Values)[1:ncol(DATA.Values)]),selected = NULL)
      updateSelectizeInput(session, inputId="Single.Control", label = "Select Control", choices = colnames(DATA.Values)[1:ncol(DATA.Values)],selected = NULL)
      updateSelectizeInput(session, inputId="Single.Treatment", label = "Select Treatment", choices = colnames(DATA.Values)[1:ncol(DATA.Values)],selected = NULL)
      updateSelectizeInput(session, inputId = "Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
      updateSelectizeInput(session, inputId = "Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
      updateSelectizeInput(session, inputId = "DEO_Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
      updateSelectizeInput(session, inputId = "DEO_Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
      updateSelectizeInput(session, inputId = "Control_GSEA_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = colnames(DATA.Values)[1:ncol(DATA.Values)][1])
      updateSelectizeInput(session, inputId = "Query_GSEA_Test",label = "Select Query",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = colnames(DATA.Values)[1:ncol(DATA.Values)][ncol(DATA.Values)])
      #updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes (max:12)", choices = Gene.Choices,selected = NULL)
     
      save(list = c("DATA.Values","DATA.Values.5min","GeneList","Data_File_path"), file=paste(Backup_Session_Folder,"/DATA_VALUES.rdata",sep=""))  
 
      progress$inc(0.9, detail = "Please Wait")
      Sys.sleep(0.001)
      
    }
    
    enable("Load_File")
    #})
    #############################    
    progress$inc(1, detail = "Processing Complete")
    Sys.sleep(0.001)
    ##############
    
   #})#isolate
   
   

  })#Load_File_button
  
  #####################################
  #####################################
  #obs.Create.Group.button
  #####################################
  #####################################       
  obs.Create.Group.button <- observeEvent(input$Create_Group,ignoreInit = TRUE,{
    if(nchar(as.character(input$Group_Name))>=1 & length(as.character(input$All_Conditions))>=1)
    {
      temp.grp <- ""
      temp.grp <- input$Group_Name
      
      #if(length(Groups)<1){Groups=""}
      
      if(Group_Count==0)
      {
        Groups <<- c()
        Group.Members <<- c()
      }
      
      
      if(any(as.character(Groups)==as.character(input$Group_Name)))
      {
        Grp.Indx = which(as.character(Groups)==as.character(input$Group_Name))
        Group.Members[Grp.Indx] <<- paste(unique(c(unlist(strsplit(Group.Members[Grp.Indx],split=";")),input$All_Conditions)),collapse = ";")
      }
      if(!any(as.character(Groups)==as.character(input$Group_Name)))
      {
        Group_Count <<- Group_Count+1
        Groups <<- c(Groups,input$Group_Name)
        Group.Members<<- c(Group.Members,paste(input$All_Conditions,collapse = ";"))
      }
      
      
      if(nchar(nrow(DATA.Values.5min))>1)
      {
        #rownames(DATA.Values)[which(rownames(DATA.Values) %in% input$All_Condition)]
        #updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=as.matrix(DATA.Values[1,]))
        
        #updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=as.matrix(DATA.Values[1,][which(colnames(DATA.Values) %in% unlist(strsplit(Group.Members,split=";")))]))
        #updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=colnames(DATA.Values)[which(colnames(DATA.Values) %in% unlist(as.character(input$All_Conditions)))])
        updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=paste(paste(Groups,":",Group.Members,sep="",collaspe="\n"),collapse=""))
        updateSelectizeInput(session, inputId="All_Conditions",selected = "")
        updateTextInput(session,inputId="Group_Name", value="")
        updateSelectizeInput(session, inputId="Group.Compare.Condition", label = "Select Groups", choices = c("ALL",Groups),selected = NULL)
        updateSelectInput(session, inputId = "Group1_Stat_compare",label = "Select Group1",choices = Groups, selected = NULL)
        updateSelectInput(session, inputId = "Group2_Stat_compare",label = "Select Group2",choices = Groups, selected = NULL)
        updateSelectInput(session, inputId = "Group_Stats_tSNE_Group",label = "Highlight Group (Optional)",choices = Groups, selected = NULL)

	PRE_GROUPS <<- paste(paste(Groups,":",Group.Members,sep="",collaspe="\n"),collapse="")
        
        if(length(readData)>1)
        {
          updateSelectizeInput(session,inputId="Groups.Control", label="Select Group 1",choices = Groups, selected = NULL)
          updateSelectizeInput(session,inputId="Groups.Treatment", label="Select Group 2",choices = Groups, selected = NULL)
        }
        
       
       
        Groups.Medians = matrix(nrow=nrow(DATA.Values.5min),ncol=length(Groups))
        for(k in 1:length(Groups))
        {
          Groups.Medians[,k] = apply(as.matrix(DATA.Values.5min[,match(unlist(strsplit(Group.Members[k],split=";")),as.character(unlist(colnames(DATA.Values.5min))))]),MARGIN=1, function(x) median(x))
        }
        rownames(Groups.Medians) = rownames(DATA.Values.5min)
        colnames(Groups.Medians) = Groups
        Groups.Medians.Matrix <<- Groups.Medians
        
        
         #Group.GTable
       
        Group.GTable <<- data.frame() 
        for(k in 1:length(Groups))
        {
        	Group.Members.data = unlist(strsplit(Group.Members[k],split=";"))
        	for(l in 1:length(Group.Members.data))
        	{
        		Group.GTable <<- rbind(Group.GTable, data.frame(Groups = Groups[k],Samples = Group.Members.data[l], Genes = rownames(DATA.Values.5min),Expression = DATA.Values.5min[,which(colnames(DATA.Values.5min)==Group.Members.data[l])]))		
        		
           	}
        }
    
	
	save(list = c("Groups","Group.GTable","Group.Members","Groups.Medians.Matrix","PRE_GROUPS","Group_Count"), file=paste(Backup_Session_Folder,"/Group_data.rdata",sep=""))              
        
      }#if(nchar(input$Data_set)>1)
    }#if(nchar(as.character(input$Group_Name))>=1 & nchar(as.character(input$All_Conditions))>=1)
    
  })# obs.Create.Group.button <- observeEvent(input$Create_Group,{
  
  #####################################
  #####################################
  #obs.Confirm.Group.button
  #####################################
  #####################################  
  obs.Confirm.Group.button <- observeEvent(input$Confirm_Group,ignoreInit = TRUE,{
   
	 progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Confirming Groups", value = 0)
    # Increment the progress bar, and update the detail text.
    progress$inc(0.3, detail = "Please Wait")
    Sys.sleep(0.001)

 
    if(nchar(input$Groups)>=3)
    {
      
      isolate(Total_Groups <- length(grep(":",unlist(strsplit(input$Groups,"")))))
      isolate(Group_Sets <- unlist(strsplit(input$Groups,split="\n")))
      
      Group_Count <<- Total_Groups
      Groups <<- c()
      Group.Members <<- c()
      
      if(Total_Groups >= 1)
      {
        for(i in 1:Total_Groups)
        {
          Group_set_temp = unlist(strsplit(Group_Sets[i],split=":"))
          Group_set_members = unlist(strsplit(Group_set_temp[2],split="\n"))[1]
          
          
          member.array = as.character(unlist(strsplit(Group_set_members,split=";")))
          colnames(DATA.Values)
          
	  if(length(which(!is.na(match(member.array,colnames(DATA.Values))))) >= 1)
	  {
          	Group_set_members = member.array[which(!is.na(match(member.array,colnames(DATA.Values))))]
          	Group_set_members = paste(Group_set_members,collapse=";")
          
          	Groups <<- c(Groups,Group_set_temp[1])
          	Group.Members <<- c(Group.Members,Group_set_members)
	  }#if(length(which(!is.na(match(member.array,colnames(DATA.Values))))))
        }#for(i in 1:Total_Groups)
        
	progress$inc(0.4, detail = "Please Wait - Loading Fields")
    	Sys.sleep(0.001)
	
	if(length(Groups) >= 1)
	{
        	updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=paste(paste(Groups,":",Group.Members,sep="",collaspe="\n"),collapse=""))
        	updateSelectizeInput(session, inputId="All_Conditions",selected = "")
        	updateTextInput(session,inputId="Group_Name", value="")
        	updateSelectizeInput(session, inputId="Group.Compare.Condition", label = "Select Groups", choices = c("ALL",Groups),selected = NULL)
        	updateSelectInput(session, inputId = "Group1_Stat_compare",label = "Select Group1",choices = Groups, selected = NULL)
        	updateSelectInput(session, inputId = "Group2_Stat_compare",label = "Select Group2",choices = Groups, selected = NULL)
        	updateSelectInput(session, inputId = "Group_Stats_tSNE_Group",label = "Highlight Group (Optional)",choices = Groups, selected = NULL)

		PRE_GROUPS <<- paste(paste(Groups,":",Group.Members,sep="",collaspe="\n"),collapse="")
	}else{

		progress$inc(0.6, detail = "No Valid Groups")
        	Sys.sleep(0.5)
		updateTextAreaInput(session,inputId = "Groups",label = "Groups",value="")
		return(NULL)

	}#if(length(Groups) >= 1)
        
        if(length(readData)>1)
        {
          updateSelectizeInput(session,inputId="Groups.Control", label="Select Group 1",choices = Groups, selected = NULL)
          updateSelectizeInput(session,inputId="Groups.Treatment", label="Select Group 2",choices = Groups, selected = NULL)
        }#if(length(readData)>1)
        
        if(input$GSEA_Group_v_Sample == "Groups")
        {
        	updateRadioButtons(session,inputId = "GSEA_Group_v_Sample",label = "Analyze Groups or Samples?",choices = c("Groups","Sample"),inline = T,selected = "Sample")
        	updateSelectizeInput(session, inputId = "Control_GSEA_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = colnames(DATA.Values)[1:ncol(DATA.Values)][1])
       		updateSelectizeInput(session, inputId = "Query_GSEA_Test",label = "Select Query",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = colnames(DATA.Values)[1:ncol(DATA.Values)][ncol(DATA.Values)])
       	}#if(GSEA_Group_v_Sample == "Groups")

        if(input$GO_Group_v_Sample == "Groups")
        {       
		updateRadioButtons(session,inputId = "GO_Group_v_Sample",label = "Analyze Groups or Samples?",choices = c("Groups","Samples"),inline = T,selected = "Samples")
		updateSelectizeInput(session, inputId = "Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
       		updateSelectizeInput(session, inputId = "Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
       	}#if(input$GO_Group_v_Sample == "Groups")

		
	if(input$DEO_GO_Group_v_Sample == "Groups")
	{	
        	updateRadioButtons(session,inputId = "DEO_GO_Group_v_Sample",label = "Analyze Groups or Samples?",choices = c("Groups","Samples"),inline = T,selected = "Samples")
            	updateSelectizeInput(session, inputId = "DEO_Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
       		updateSelectizeInput(session, inputId = "DEO_Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
       	}#if(input$DEO_GO_Group_v_Sample == "Groups")
       	
        
        
        progress$inc(0.6, detail = "Please Wait - Calculating Group Medians")
        Sys.sleep(0.001)
        
        Groups.Medians = matrix(nrow=nrow(DATA.Values.5min),ncol=length(Groups))
        for(k in 1:length(Groups))
        {
          Groups.Medians[,k] = apply(as.matrix(DATA.Values.5min[,match(unlist(strsplit(Group.Members[k],split=";")),as.character(unlist(colnames(DATA.Values.5min))))]),MARGIN=1, function(x) median(x))
        }
        rownames(Groups.Medians) = rownames(DATA.Values.5min)
        colnames(Groups.Medians) = Groups
        Groups.Medians.Matrix <<- Groups.Medians
        
         #Group.GTable
       
	progress$inc(0.7, detail = "Please Wait - Creating Group Tables")
        Sys.sleep(0.001)

        Group.GTable <<- data.frame() 
        for(k in 1:length(Groups))
        {
        	Group.Members.data = unlist(strsplit(Group.Members[k],split=";"))
        	#for(l in 1:length(Group.Members.data))
        	#{
        	#	Group.GTable <<- rbind(Group.GTable, data.frame(Groups = Groups[k],Samples = Group.Members.data[l], Genes = rownames(DATA.Values.5min),Expression = DATA.Values.5min[,which(colnames(DATA.Values.5min)==Group.Members.data[l])]))		
        		
           	#}

		GTable_subset = data.frame(Groups = Groups[k],melt(data.frame(Genes=rownames(DATA.Values.5min),DATA.Values.5min[,which(colnames(DATA.Values.5min) %in% Group.Members.data)] )))
		colnames(GTable_subset) = c("Groups","Genes","Samples","Expression")
		GTable_subset = GTable_subset[,c(1,3,2,4)]
		Group.GTable <<- rbind(Group.GTable,GTable_subset)

		progress$inc(0.7, detail = paste("Please Wait - Created", Groups[k] ,"Table"))
        	Sys.sleep(0.001)	
        }#for(k in 1:length(Groups))

	
	save(list = c("Groups","Group.GTable","Group.Members","Groups.Medians.Matrix","PRE_GROUPS","Group_Count"), file=paste(Backup_Session_Folder,"/Group_data.rdata",sep=""))     
        
	 progress$inc(0.8, detail = "Please Wait - Complete")
         Sys.sleep(0.001)
        
	
      }#if(Total_Groups>=1)
    }#if(length(Groups>=1))
  })#obs.Confirm.Group.button <- observeEvent(input$Confirm_Group,{
  
  
  #####################################
  #####################################
  #obs.Upload.Group.button
  #####################################
  ##################################### 
  obs.Upload.Group.button <- observeEvent(input$Upload_Group,ignoreInit = TRUE,{
    
    inFile_Groups <- input$Upload_Groups_File$datapath
    if (!is.null(inFile_Groups))
    {
      
      Group_Data = readLines(input$Upload_Groups_File$datapath)
      Group_Data = Group_Data[which(nchar(Group_Data)>=3)]
      Group_Data = paste(Group_Data,collapse="\n")
      updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=Group_Data)
    }
  })
  
  
  #####################################
  #####################################
  #obs.downloadGroups.button
  #####################################
  #####################################
  
  
  
  
  output$downloadGroups <- downloadHandler(
    filename = function() {
      paste("GroupData", ".txt", sep = "")
    },
    content = function(file) {
      write(input$Groups, file)
    })
  
  #####################################
  #####################################
  #obs.Single.Sample.Compare.button
  #####################################
  ##################################### 
  obs.Single.Sample.Compare.button <- observeEvent(input$Compare.Sample.Exp,ignoreInit = TRUE,{
    #####################
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Comparing Samples", value = 0)
    # Increment the progress bar, and update the detail text.
    progress$inc(0.3, detail = "Please Wait")
    Sys.sleep(0.001)
    ################
    
    output$Single.Sample.barplot.label <- renderText({" "})
    output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Single.Sample.heatmap.label <- renderText({" "})
    output$Single.Sample.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Stacked_Plot.label <- renderText({" "})
    output$Stacked_Plot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$MDS_PLOT.Samples.label <- renderText({" "})
    output$MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    
    
    Single.Compare.GeneList <- isolate(input$Single.Compare.GeneList)
    Single.Compare.Conditions <- isolate(input$Single.Compare.Conditions)
    
    Single.Paste.GeneList <- isolate(input$Single.Paste.GeneList)
    
    Single.Paste.GeneList_parsed = unlist(strsplit(Single.Paste.GeneList,split=" |,|\t|\n|\r|;"))
    
    Single.Paste.GeneList_parsed_Present = as.character(unlist(rownames(DATA.Values.5min)))[as.character(unlist(rownames(DATA.Values.5min))) %in% Single.Paste.GeneList_parsed]
    
    Single.Compare.GeneList = c(Single.Compare.GeneList,Single.Paste.GeneList_parsed_Present)
    
    if(length(Single.Compare.GeneList)>=1)
    {
      gene.index.list = match( as.character(unlist(Single.Compare.GeneList)),as.character(unlist(rownames(DATA.Values.5min))))
      
      if(length(Single.Compare.Conditions) > 0 & all(as.character(unlist(Single.Compare.Conditions))!="ALL"))
      {
        Condition.index.list = match( as.character(unlist(Single.Compare.Conditions)),as.character(unlist(colnames(DATA.Values.5min))))
      }
      if(length(Single.Compare.Conditions) == 0 | any(as.character(unlist(Single.Compare.Conditions))=="ALL"))
      {
        Condition.index.list = c(1:ncol(DATA.Values)) 
      }
      
      expr.table = as.matrix(t(as.numeric(as.matrix(DATA.Values.5min[gene.index.list[1],Condition.index.list]))))
      if(length(gene.index.list)>1)
      {
        for(hij in 2:length(gene.index.list))
        {
          expr.table = rbind(expr.table,as.numeric(as.matrix(DATA.Values.5min[gene.index.list[hij],Condition.index.list])))
        }
      }
      
      
      rownames(expr.table) = GeneList[gene.index.list]
      colnames(expr.table) = colnames(DATA.Values.5min[,Condition.index.list])
      
      
      
      output$Stacked_Plot.label = renderText("Barplots: Individual Gene expressions compared among conditions")
    
        
       
if(length(Condition.index.list)>1) Gmatrix = data.frame(DATA.Values.5min[gene.index.list,Condition.index.list])

if(length(Condition.index.list)==1) {
	Gmatrix = data.frame(DATA.Values.5min[gene.index.list,Condition.index.list]); 
	rownames(Gmatrix) = rownames(DATA.Values.5min)[gene.index.list];
	colnames(Gmatrix) = colnames(DATA.Values.5min)[Condition.index.list]
	}


GTable = data.frame()
for(A in 1:nrow(Gmatrix))
{
  for(B in 1:ncol(Gmatrix))
  {
     GTable = rbind(GTable,data.frame(Gene = rownames(Gmatrix)[A],Sample =  colnames(Gmatrix)[B], Value = Gmatrix[A,B]))
  }
}



gg_color_hue <- function(n) {hues = seq(15, 375, length = n + 1);hcl(h = hues, l = 65, c = 100)[1:n]}
COLOR.List = gg_color_hue(length(Condition.index.list))
COLOR.List = c(input$Single.Color.List,COLOR.List)[1:length(Condition.index.list)]

shinyjs::show(id = "Stacked_Plot_div") 
output$Stacked_Plot <- renderPlot({

#if(input$Single.Xaxis.angle == 0 | input$Single.Xaxis.angle == 90) 
#{HJUST = 0.5;VJUST = 0.5} else {HJUST = 1; VJUST=1}

if(input$Single.Xaxis.angle == 0) 
        {HJUST =0.5;VJUST = 1} else if (input$Single.Xaxis.angle == 90) 
        {HJUST = 1;VJUST = 0.5} else {HJUST = 1; VJUST=1}

plot_list = list()
for(xyz in 1:length(gene.index.list))
{

  GNAME = rownames(DATA.Values.5min)[gene.index.list[xyz]]
  

  if(input$Single.Theme.List == "default")
  {
    P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x=Gene, y=Value, fill = Sample)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_manual(values=COLOR.List) + 
    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    ylab("Expression")
  }


  if(input$Single.Theme.List == "Classic")
  {
    P = ggplot(data= subset(GTable,(Gene == GNAME)), aes(x=Gene, y=Value, fill = Sample)) +
    geom_bar(stat="identity", position=position_dodge())+
    theme_classic()+
    scale_fill_manual(values=COLOR.List) + 
    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    ylab("Expression")

  }
    if(input$Single.Theme.List == "Black and White")
  {
    P = ggplot(data= subset(GTable,(Gene == GNAME)), aes(x=Gene, y=Value, fill = Sample)) +
    geom_bar(stat="identity", position=position_dodge())+
    theme_bw()+
    scale_fill_manual(values=COLOR.List) + 
    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    ylab("Expression")

  }
    
    if(input$Single.Theme.List == "Dark")
  {
    P = ggplot(data= subset(GTable,(Gene == GNAME)), aes(x=Gene, y=Value, fill = Sample)) +
    geom_bar(stat="identity", position=position_dodge())+
    theme_dark()+
    scale_fill_manual(values=COLOR.List) + 
    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    ylab("Expression")
  }


    if(input$Single.Theme.List == "Test")
  {
    P = ggplot(data= subset(GTable,(Gene == GNAME)), aes(x=Gene, y=Value, fill = Sample)) +
    geom_bar(stat="identity", position=position_dodge())+
    theme_test()+
    scale_fill_manual(values=COLOR.List) + 
    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    ylab("Expression")
  }
  
    if(input$Single.Theme.List == "Void")
  {
    P = ggplot(data= subset(GTable,(Gene == GNAME)), aes(x=Gene, y=Value, fill = Sample)) +
    geom_bar(stat="identity", position=position_dodge())+
    theme_void()+
    scale_fill_manual(values=COLOR.List) + 
    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    ylab("Expression")
  }


  plot_list[[xyz]] = P
}#for(xyz in 1:length(gene.index.list))

Report.List.Reads <<- c(Report.List.Reads,plot_list)

GRID.COLS = 6-input$Sample.Graph_Width
GRID.ROWS = ceiling(length(gene.index.list) / GRID.COLS)


Stacked_Plot_plot_list = plot_list
Stacked_Plot_GRID.COLS = GRID.COLS

save(list = c("Stacked_Plot_plot_list","Stacked_Plot_GRID.COLS"), file=paste(Backup_Session_Folder,"/Stacked_Plot.rdata",sep=""))  

do.call("grid.arrange", c(plot_list, ncol= GRID.COLS))



      })#output$Stacked_Plot <- renderPlot({
      
      
      if(nrow(expr.table)>=2 & ncol(expr.table)>=2)
      {
        output$Single.Sample.heatmap.label <- renderText({"Heatmap: Relative gene expression"})
        
        shinyjs::show(id = "Single.Sample.heatmap_div") 
        output$Single.Sample.heatmap <- renderPlot({
          
          Font_Adjust = input$Sample.Font_Size
          
          Temp.expr.table = expr.table
          for(k in 1:nrow(Temp.expr.table))
          {
            if(length(unlist(unique(Temp.expr.table[k,])))==1)
            {
              Temp.expr.table[k,1]=0.000001
            }
          }
          
         

          BP8 = pheatmap(Temp.expr.table, cluster_cols = FALSE, cluster_rows = TRUE, scale = "row", cellwidth = 15, fontsize_row = 6*Font_Adjust, main = paste("Norm. Exp. of Selected Genes"))

	  Single.Sample.heatmap_BP8 = BP8

	  save(list = c("Single.Sample.heatmap_BP8"), file=paste(Backup_Session_Folder,"/Single_Sample_heatmap.rdata",sep=""))

	  BP8
          
        }) ## output$Single.Sample.heatmap <- renderPlot({
      }#if(nrow(expr.table)>=2 & ncol(expr.table)>=2)
      
      
      
      
      if(nrow(expr.table)>=2)
      {
        output$Single.Sample.barplot.label <- renderText({"Barplots: Collated genes expressions in each condition"})
        
        shinyjs::show(id = "Single.Sample.barplot_div")
        output$Single.Sample.barplot = renderPlot({
        	
        gg_color_hue <- function(n) {hues = seq(15, 375, length = n + 1);hcl(h = hues, l = 65, c = 100)[1:n]}
		COLOR.List_genes = gg_color_hue(length(gene.index.list))
		COLOR.List_genes = c(input$Single.Color.List,COLOR.List_genes)[1:length(gene.index.list)]

        
      	#if(input$Single.Xaxis.angle == 0 | input$Single.Xaxis.angle == 90) 
	  	#{HJUST = 0.5;VJUST = 0.5} else {HJUST = 1; VJUST=1} 	
	if(input$Single.Xaxis.angle == 0) 
        {HJUST =0.5;VJUST = 1} else if (input$Single.Xaxis.angle == 90) 
        {HJUST = 1;VJUST = 0.5} else {HJUST = 1; VJUST=1}

	      GNAME = rownames(DATA.Values.5min)[gene.index.list]
	  	  if(input$Single.Theme.List == "default")
		  {
		    P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x=Sample, y=Value, fill = Gene)) +
		    geom_bar(stat="identity", position=position_dodge())+
		    scale_fill_manual(values= COLOR.List_genes) + 
		    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
		    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    		ylab("Expression")
		  }
		
		
		   if(input$Single.Theme.List == "Classic")
		  {
		    P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x=Sample, y=Value, fill = Gene)) +
		    geom_bar(stat="identity", position=position_dodge())+
		    theme_classic()+
		    scale_fill_manual(values= COLOR.List_genes) + 
		    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
		    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    		ylab("Expression")
		
		  }
		    if(input$Single.Theme.List == "Black and White")
		  {
		    P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x=Sample, y=Value, fill = Gene)) +
		    geom_bar(stat="identity", position=position_dodge())+
		    theme_bw()+
		    scale_fill_manual(values= COLOR.List_genes) + 
		    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
		    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    		ylab("Expression")
		
		  }
		    
		    if(input$Single.Theme.List == "Dark")
		  {
		    P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x=Sample, y=Value, fill = Gene)) +
		    geom_bar(stat="identity", position=position_dodge())+
		    theme_dark()+
		    scale_fill_manual(values= COLOR.List_genes) + 
		    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
		    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    		ylab("Expression")
		  }
		
		
		    if(input$Single.Theme.List == "Test")
		  {
		    P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x=Sample, y=Value, fill = Gene)) +
		    geom_bar(stat="identity", position=position_dodge())+
		    theme_test()+
		    scale_fill_manual(values= COLOR.List_genes) + 
		    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
		    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    		ylab("Expression")
		  }
		  
		    if(input$Single.Theme.List == "Void")
		  {
		    P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x=Sample, y=Value, fill = Gene)) +
		    geom_bar(stat="identity", position=position_dodge())+
		    theme_void()+
		    scale_fill_manual(values= COLOR.List_genes) + 
		    theme(axis.text.x = element_text(angle=input$Single.Xaxis.angle, hjust = HJUST, vjust = VJUST))+ 
		    theme(text = element_text(size=5*(1+input$Sample.Font_Size)))+
    		ylab("Expression")
		  }


		  Single.Sample.barplot_P = P
		  Single.Sample.barplot_Width = input$Sample.Graph_Width

		  save(list = c("Single.Sample.barplot_P","Single.Sample.barplot_Width"), file=paste(Backup_Session_Folder,"/Single_Sample_barplot.rdata",sep=""))

		  Report.List.Reads <<- c(Report.List.Reads,list(P)) 
		  do.call("grid.arrange", c(list(P), ncol= (6-input$Sample.Graph_Width), nrow= 1))
	

      	  	
        	
        }) ## output$Single.Sample.barplot = renderPlot
      }#if(nrow(expr.table)>=2)
      
      
      
 
      if((nrow(expr.table)>=3) & (ncol(expr.table)>=3))
      {
        d = dist(t(expr.table))
	fit <- isoMDS(d, k=2)

        MDS_Data = data.frame(X = fit$points[,1], Y = fit$points[,2],Samples = colnames(expr.table))

        MDS_Distance.plot = ggplot(data=MDS_Data,aes(x=X, y=Y)) +
          geom_text(label=MDS_Data$Samples)+
	  theme_bw() + 
          ggtitle("Nonmetric MDS plot using selected genes")

        Report.List.Reads <<- c(Report.List.Reads,list(MDS_Distance.plot))	

	MDS_Distance.plot.points = ggplot(data=MDS_Data,aes(x=X, y=Y, col = Samples)) +
          geom_point()+
          theme_bw()+
          ggtitle("Nonmetric MDS")

        Report.List.Reads <<- c(Report.List.Reads,list(MDS_Distance.plot.points))

        output$MDS_PLOT.Samples.label <- renderText("MDSPlot: Based on selected genes only")

        shinyjs::show(id = "MDS_PLOT.Samples_div")
        output$MDS_PLOT.Samples <- renderPlot({


	   MDS_Distance.plot

        }) ## output$MDS_PLOT.Samples <- renderPlot

	MDS_Distance.plot_MDS_PLOT.Samples = MDS_Distance.plot
	save(list = c("MDS_Distance.plot_MDS_PLOT.Samples"), file=paste(Backup_Session_Folder,"/MDS_PLOT.Samples.rdata",sep=""))     
	
      }#if(nrow(expr.table)>=3)
      
    }#if(length(Single.Compare.GeneList)>=1)
    
    progress$inc(1, detail = "Comparing Complete")
    Sys.sleep(0.001)
  })
  
  
  #####################################
  #####################################
  #obs.Group.Compare.button
  #####################################
  ##################################### 
  obs.Group.Compare.button <- observeEvent(input$Compare.Group.Exp,ignoreInit = TRUE,{
    
    ######################
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Comparing Gene Expressions", value = 0)
    # Increment the progress bar, and update the detail text.
    progress$inc(0.3, detail = "Please Wait")
    Sys.sleep(0.001)
    ######################
    
    #output$Single.Sample.barplot.label <- renderText({" "})
    #output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    
    output$Group.boxplot.label <- renderText({" "})
    output$Group.boxplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.barplot.label <- renderText({" "})
    output$Group.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.barplot.sem.label <- renderText({" "})
    output$Group.barplot.sem <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.barplot.maxmin.label <- renderText({" "})
    output$Group.barplot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.heatmap.label <- renderText({" "})
    output$Group.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.members.heatmap.label <- renderText({" "})
    output$Group.members.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.stackedPlot.label <- renderText({" "})
    output$Group.stackedPlot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.stackedPlot.maxmin.label <- renderText({" "})
    output$Group.stackedPlot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group.MDS_PLOT.Samples.label <- renderText({" "})
    output$Group.MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    
    
    Group.Compare.GeneList <- isolate(input$Group.Compare.GeneList)
    Group.Compare.Condition <- isolate(input$Group.Compare.Condition)
    
    Group.Paste.GeneList <- isolate(input$Group.Paste.GeneList)   
    Group.Paste.GeneList_parsed = unlist(strsplit(Group.Paste.GeneList,split=" |,|\t|\n|\r|;"))   
    Group.Paste.GeneList_parsed_Present = as.character(unlist(rownames(DATA.Values.5min)))[as.character(unlist(rownames(DATA.Values.5min))) %in% Group.Paste.GeneList_parsed]
    
    Group.Compare.GeneList = c(Group.Compare.GeneList, Group.Paste.GeneList_parsed_Present)

    
    
    
    
    if(length(Group.Compare.Condition)>=1 & length(Group.Compare.GeneList)>=1)
    {
      Group.Index=c()
      Gene.Index=c()
      
      if(any(as.character(unlist(Group.Compare.Condition)) == "ALL")){Group.Compare.Condition=Groups}
      
      for(a in 1:length(Group.Compare.Condition))
      {
        Group.Index[a] = which(Groups == Group.Compare.Condition[a])
      }
      for(b in 1:length(Group.Compare.GeneList))
      {
        Gene.Index[b] = which(GeneList == Group.Compare.GeneList[b])
      }
      
      Groups.Medians = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
      Groups.sem.max = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
      Groups.sem.min = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
      Groups.Max = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
      Groups.Min = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
      
      Group.Member.matrix = c()
      Group.Member.matrix.labels = c()
      BP.Lengths = c()
      for(k in 1:length(Group.Index))
      {
        Group.set.matrix = c()
        for(l in 1:length(Gene.Index))
        {
          Groups.Medians[l,k] = median(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))
          Groups.Max[l,k] = max(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))
          Groups.Min[l,k] = min(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))
          Groups.sem.max[l,k] = Groups.Medians[l,k] + 2*sd(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))/sqrt(length(unlist(strsplit(Group.Members[Group.Index[k]],split=";"))))
          Groups.sem.min[l,k] = Groups.Medians[l,k] - 2*sd(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))/sqrt(length(unlist(strsplit(Group.Members[Group.Index[k]],split=";"))))
          
          if(is.na(Groups.sem.max[l,k]))Groups.sem.max[l,k]=0
          if(is.na(Groups.sem.min[l,k]))Groups.sem.min[l,k]=0
          
          Group.set.matrix = rbind(Group.set.matrix, as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))
        }
        
        BP.Lengths[k] = length(unlist(strsplit(Group.Members[Group.Index[k]],split=";")))
        Group.Member.matrix.labels = c(Group.Member.matrix.labels, paste(Groups[k],unlist(strsplit(Group.Members[Group.Index[k]],split=";")),sep=":"))
        Group.Member.matrix = cbind(Group.Member.matrix,Group.set.matrix)
      }
      rownames(Groups.Medians) = Group.Compare.GeneList
      colnames(Groups.Medians) = Group.Compare.Condition
      
      
      
      
      
      output$Group.stackedPlot.label <- renderText({"Barplots: Median Gene expressions compared between groups (SEM)"})
      
      shinyjs::show(id = "Group.stackedPlot_div")
      output$Group.stackedPlot <- renderPlot({
        
        Width_Adjust = input$Group.Graph_Width
        Font_Adjust = input$Group.Font_Size
        
        n.rows =  ceiling(length(Gene.Index)/3)
        
        par(mfrow=c(n.rows,3*4/Width_Adjust))


	GSP_Width_Adjust = Width_Adjust
	GSP_Gene.Index = Gene.Index
	GSP_Groups.Medians = Groups.Medians
	GSP_Groups.sem.max = Groups.sem.max
	GSP_Font_Adjust = Font_Adjust

	save(list = c("GSP_Gene.Index","GSP_Groups.Medians","GSP_Groups.sem.max","GSP_Font_Adjust","GSP_Width_Adjust"), file=paste(Backup_Session_Folder,"/Group_stackedPlot.rdata",sep=""))

        for(xyz in 1:length(Gene.Index))
        {
          bpgp.stackSEM = barplot(as.numeric(as.matrix(Groups.Medians[xyz,])),ylim = c(0,max(Groups.sem.max[xyz,])),names.arg=colnames(Groups.Medians),main=rownames(Groups.Medians)[xyz],las=2,col=(xyz+1),cex.names=0.8*Font_Adjust,cex.main=1*Font_Adjust,cex.axis=1*Font_Adjust)
          #segments(bpgp.stackSEM,as.numeric(as.matrix(Groups.Medians[xyz,])),bpgp.stackSEM,as.numeric(as.matrix(Groups.sem.max[xyz,])))
          arrows(bpgp.stackSEM,as.numeric(as.matrix(Groups.Medians[xyz,])),bpgp.stackSEM,as.numeric(as.matrix(Groups.sem.max[xyz,])),angle = 90,code = 2,length = 0.05)
        }#for(xyz in 1:length(Gene.Index))
        par(mfrow=c(1,1))
      })#output$Group.stackedPlot <- renderPlot({
      
      output$Group.stackedPlot.maxmin.label <- renderText({"Barplots: Median Gene expressions compared between groups (error bars: Min/Max range)"})
      
      shinyjs::show(id = "Group.stackedPlot.maxmin_div")
      output$Group.stackedPlot.maxmin <- renderPlot({
        
        n.rows =  ceiling(length(Gene.Index)/3)
        
        Width_Adjust = input$Group.Graph_Width
        Font_Adjust = input$Group.Font_Size

        par(mfrow=c(n.rows,3*4/Width_Adjust))

	GSP_MM_Width_Adjust = Width_Adjust
	GSP_MM_Font_Adjust = Font_Adjust
	GSP_MM_Gene.Index = Gene.Index
	GSP_MM_Groups.Medians = Groups.Medians
	GSP_MM_Groups.Max = Groups.Max
	GSP_MM_Groups.Min = Groups.Min

	save(list = c("GSP_MM_Gene.Index","GSP_MM_Groups.Medians","GSP_MM_Groups.Max","GSP_MM_Groups.Min","GSP_MM_Font_Adjust","GSP_MM_Width_Adjust"), file=paste(Backup_Session_Folder,"/Group_stackedPlot_MM.rdata",sep=""))

        for(xyz in 1:length(Gene.Index))
        {
          bpgp.stackMM = barplot(as.numeric(as.matrix(Groups.Medians[xyz,])),names.arg=colnames(Groups.Medians),main=rownames(Groups.Medians)[xyz],las=2,col=(xyz+1),ylim=c(0,max(Groups.Max[xyz,])),cex.names=0.8*Font_Adjust,cex.main=1*Font_Adjust,cex.axis=1*Font_Adjust)
          segments(bpgp.stackMM,as.numeric(as.matrix(Groups.Min[xyz,])),bpgp.stackMM,as.numeric(as.matrix(Groups.Max[xyz,])))
          arrows(bpgp.stackMM,as.numeric(as.matrix(Groups.Min[xyz,])),bpgp.stackMM,as.numeric(as.matrix(Groups.Max[xyz,])),angle = 90,code = 3,length = 0.05)
          
        }#for(xyz in 1:length(Gene.Index))
        par(mfrow=c(1,1))
      })#output$Group.stackedPlot.maxmin <- renderPlot({
      
      
      output$Group.boxplot.label <- renderText({"Boxplots: Group RPKM values"})
      
      shinyjs::show(id = "Group.boxplot_div")
      output$Group.boxplot <- renderPlot({
        
        Width_Adjust = input$Group.Graph_Width
        Font_Adjust = input$Group.Font_Size
        n.rows =  ceiling(length(Gene.Index)/3)
        par(mfrow=c(n.rows,3*4/Width_Adjust))
        
        GTable = data.frame()
        for(x in 1:length(Gene.Index))
        {
          start=1
          for(h in 1:length(BP.Lengths))
          {
            if(h==1){BP.List = list(Group.Member.matrix[x,(start):(start+BP.Lengths[h]-1)])}
            if(h>1){BP.List = c(BP.List,list(Group.Member.matrix[x,(start):(start+BP.Lengths[h]-1)]))}
            
            start = (start+BP.Lengths[h]) 
          }#for(h in 1:length(BP.Lengths))
               
			for(A in 1:length(BP.List))
			{
 			 for(B in 1:length(BP.List[[A]]))
			  {
 			    GTable = rbind(GTable,data.frame(Gene =  Group.Compare.GeneList[x], Group =  Group.Compare.Condition[A], Value = BP.List[[A]][B]))
 			  }
			}     
          
          #boxplot(BP.List,names = Group.Compare.Condition, notch=F,lwd=2,frame=F,main = Group.Compare.GeneList[x],cex.main=1*Font_Adjust,cex.axis=1*Font_Adjust )
        }#for(x in 1:length(Gene.Index))
        par(mfrow=c(1,1))
        
        
        #if(length(Condition.index.list)>1) Gmatrix = data.frame(DATA.Values.5min[gene.index.list,Condition.index.list])

		#if(length(Condition.index.list)==1) {
		#		Gmatrix = data.frame(DATA.Values.5min[gene.index.list,Condition.index.list]); 
		#		rownames(Gmatrix) = rownames(DATA.Values.5min)[gene.index.list];
		#		colnames(Gmatrix) = colnames(DATA.Values.5min)[Condition.index.list]
		#	}





	      	 
    gg_color_hue <- function(n) {hues = seq(15, 375, length = n + 1);hcl(h = hues, l = 65, c = 100)[1:n]}
	COLOR.List = gg_color_hue(length(unique(GTable$Group)))
	COLOR.List = c(input$Group.Color.List,COLOR.List)[1:length(unique(GTable$Group))]



	#if(input$Group.Xaxis.angle == 0 | input$Group.Xaxis.angle == 90) 
	#{HJUST = 0.5;VJUST = 0.5} else {HJUST = 1; VJUST=1}
	if(input$Group.Xaxis.angle == 0) 
        {HJUST =0.5;VJUST = 1} else if (input$Group.Xaxis.angle == 90) 
        {HJUST = 1;VJUST = 0.5} else {HJUST = 1; VJUST=1}

	plot_list = list()
	for(xyz in 1:length(unique(GTable$Gene)))
	{

	  GNAME = unique(GTable$Gene)[xyz]
  

	  if(input$Group.Theme.List == "default")
	  {
  	 	 P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x= Group, y=Value, fill = Group)) +
   		 geom_boxplot()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab(paste(unique(GTable$Gene)[xyz],"expression"))  		 
  	  }


  if(input$Group.Theme.List == "Classic")#theme_classic
  {
  	 	 P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x= Group, y=Value, fill = Group)) +
   		 geom_boxplot()+
   		 theme_classic()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab(paste(unique(GTable$Gene)[xyz],"expression"))  

  }
    if(input$Group.Theme.List == "Black and White")#theme_bw
  {
     	 P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x= Group, y=Value, fill = Group)) +
   		 geom_boxplot()+
   		 theme_bw()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab(paste(unique(GTable$Gene)[xyz],"expression"))  

  }
    
    if(input$Group.Theme.List == "Dark")#theme_dark
  {
         P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x= Group, y=Value, fill = Group)) +
   		 geom_boxplot()+
   		 theme_dark()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab(paste(unique(GTable$Gene)[xyz],"expression")) 
  }


    if(input$Group.Theme.List == "Test")#theme_test
  {
     	 P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x= Group, y=Value, fill = Group)) +
   		 geom_boxplot()+
   		 theme_test()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab(paste(unique(GTable$Gene)[xyz],"expression")) 
  }
  
    if(input$Group.Theme.List == "Void")#theme_void
  {
     	 P = ggplot(data= subset(GTable,(Gene %in% GNAME)), aes(x= Group, y=Value, fill = Group)) +
   		 geom_boxplot()+
   		 theme_void()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab(paste(unique(GTable$Gene)[xyz],"expression")) 
  }


  plot_list[[xyz]] = P
}

			#do.call("grid.arrange", c(list(P), ncol= (6-input$Sample.Graph_Width), nrow= 1))

GRID.COLS = 6-input$Group.Graph_Width
GRID.ROWS = ceiling(length(unique(GTable$Gene)) / GRID.COLS)

Group.boxplot_GRID.COLS = GRID.COLS
Group.boxplot_GRID.ROWS = GRID.ROWS
Group.boxplot_plot_list = plot_list

save(list = c("Group.boxplot_plot_list","Group.boxplot_GRID.ROWS","Group.boxplot_GRID.COLS"), file=paste(Backup_Session_Folder,"/Group_boxplot.rdata",sep=""))

Report.List.Reads <<- c(Report.List.Reads,plot_list)
do.call("grid.arrange", c(plot_list, ncol= GRID.COLS, nrow= GRID.ROWS))
        
        
        
        
        
            
      })#output$Group.boxplot <- renderPlot({
      	
      	
      	
      	
      output$Group.barplot.label <- renderText({"Barplots: Mean gene expression in each group"})
      
      shinyjs::show(id = "Group.barplot_div")
      output$Group.barplot <- renderPlot({


     GTable = Group.GTable[which(Group.GTable$Groups %in% Group.Compare.Condition & Group.GTable$Genes %in% Group.Compare.GeneList),]
      	 
    gg_color_hue <- function(n) {hues = seq(15, 375, length = n + 1);hcl(h = hues, l = 65, c = 100)[1:n]}
	COLOR.List = gg_color_hue(length(unique(GTable$Group)))
	COLOR.List = c(input$Group.Color.List,COLOR.List)[1:length(unique(GTable$Group))]


	#if(input$Group.Xaxis.angle == 0 | input$Group.Xaxis.angle == 90) 
	#{HJUST = 0.5;VJUST = 0.5} else {HJUST = 1; VJUST=1}
	if(input$Group.Xaxis.angle == 0) 
        {HJUST =0.5;VJUST = 1} else if (input$Group.Xaxis.angle == 90) 
        {HJUST = 1;VJUST = 0.5} else {HJUST = 1; VJUST=1}

	

  	plot_list = list()
  	for(xyz in 1:length(unique(GTable$Genes)))
  	{
  
  	  GNAME = unique(GTable$Genes)[xyz]
    
  
  	  if(input$Group.Theme.List == "default")
  	  {
    	 	 P = ggplot(data= subset(GTable,(GTable$Genes %in% GNAME)), aes(x= Groups, y=Expression, fill = Groups)) +
     		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
    		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
           scale_fill_manual(values=COLOR.List) +
     		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
     		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
     		 ylab(paste(unique(GTable$Genes)[xyz],"expression"))  		 
    	  }
  
  
    if(input$Group.Theme.List == "Classic")#theme_classic
    {
    	 	 P = ggplot(data= subset(GTable,(Genes %in% GNAME)), aes(x= Groups, y= Expression, fill = Groups)) +
     		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
    		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
     		 theme_classic()+
     		 scale_fill_manual(values=COLOR.List) + 
     		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
     		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
     		 ylab(paste(unique(GTable$Genes)[xyz],"expression"))  
  
    }
      if(input$Group.Theme.List == "Black and White")#theme_bw
    {
       	 P = ggplot(data= subset(GTable,(Genes %in% GNAME)), aes(x= Groups, y= Expression, fill = Groups)) +
     		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
    		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
     		 theme_bw()+
     		 scale_fill_manual(values=COLOR.List) + 
     		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
     		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
     		 ylab(paste(unique(GTable$Genes)[xyz],"expression"))  
  
    }
      
      if(input$Group.Theme.List == "Dark")#theme_dark
    {
           P = ggplot(data= subset(GTable,(Genes %in% GNAME)), aes(x= Groups, y= Expression, fill = Groups)) +
     		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
    		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
     		 theme_dark()+
     		 scale_fill_manual(values=COLOR.List) + 
     		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
     		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
     		 ylab(paste(unique(GTable$Genes)[xyz],"expression")) 
    }
  
  
      if(input$Group.Theme.List == "Test")#theme_test
    {
       	 P = ggplot(data= subset(GTable,(Genes %in% GNAME)), aes(x= Groups, y= Expression, fill = Groups)) +
     		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
    		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
     		 theme_test()+
     		 scale_fill_manual(values=COLOR.List) + 
     		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
     		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
     		 ylab(paste(unique(GTable$Genes)[xyz],"expression")) 
    }
    
      if(input$Group.Theme.List == "Void")#theme_void
    {
       	 P = ggplot(data= subset(GTable,(Genes %in% GNAME)), aes(x= Groups, y= Expression	, fill = Groups)) +
     		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
    		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
     		 theme_void()+
     		 scale_fill_manual(values=COLOR.List) + 
     		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
     		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
     		 ylab(paste(unique(GTable$Genes)[xyz],"expression")) 
    }
  
  
    plot_list[[xyz]] = P
  }#for(xyz in 1:length(unique(GTable$Genes)))
  
  			#do.call("grid.arrange", c(list(P), ncol= (6-input$Sample.Graph_Width), nrow= 1))
  
  GRID.COLS = 6-input$Group.Graph_Width
  GRID.ROWS = ceiling(length(unique(GTable$Genes)) / GRID.COLS)
  
  Group.barplot_GRID.COLS = GRID.COLS
  Group.barplot_GRID.ROWS = GRID.ROWS
  Group.barplot_plot_list = plot_list
  
  save(list = c("Group.barplot_plot_list","Group.barplot_GRID.ROWS","Group.barplot_GRID.COLS"), file=paste(Backup_Session_Folder,"/Group_barplot.rdata",sep=""))
  
  
  Report.List.Reads <<- c(Report.List.Reads,plot_list)
  do.call("grid.arrange", c(plot_list, ncol= GRID.COLS, nrow= GRID.ROWS)) 	 
        	 
      
     
  })#output$Group.barplot <- renderPlot({
      
  
      output$Group.barplot.sem.label <- renderText({"Barplots: Collated gene expression medians in each Group (error bars: standard error)"})
      
      shinyjs::show(id = "Group.barplot.sem_div")
      output$Group.barplot.sem <- renderPlot({
      	
     
        GTable = Group.GTable[which(Group.GTable$Groups %in% Group.Compare.Condition & Group.GTable$Genes %in% Group.Compare.GeneList),]
      	 
        gg_color_hue <- function(n) {hues = seq(15, 375, length = n + 1);hcl(h = hues, l = 65, c = 100)[1:n]}
	      COLOR.List = gg_color_hue(length(unique(GTable$Group)))
	      COLOR.List = c(input$Group.Color.List,COLOR.List)[1:length(unique(GTable$Group))]


    	if(input$Group.Xaxis.angle == 0) 
    	{HJUST =0.5;VJUST = 1} else if (input$Group.Xaxis.angle == 90) 
    	{HJUST = 1;VJUST = 0.5} else {HJUST = 1; VJUST=1}
    

	

	plot_list = list()
	#for(xyz in 1:length(unique(GTable$Genes)))
	{


	  if(input$Group.Theme.List == "default")
	  {
  	 	 P = ggplot(data= GTable, aes(x= Genes, y=Expression, fill = Groups)) +
   		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
         scale_fill_manual(values=COLOR.List) +
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab("Expression") 		 
  	  }


  if(input$Group.Theme.List == "Classic")#theme_classic
  {
  	 	 P = ggplot(data= GTable, aes(x= Genes, y=Expression, fill = Groups)) +
   		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
   		 theme_classic()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab("Expression")  		 

  }
    if(input$Group.Theme.List == "Black and White")#theme_bw
  {
  	 	 P = ggplot(data= GTable, aes(x= Genes, y=Expression, fill = Groups)) +
   		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
   		 theme_bw()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab("Expression")  		 

  }
    
    if(input$Group.Theme.List == "Dark")#theme_dark
  {
  	 	 P = ggplot(data= GTable, aes(x= Genes, y=Expression, fill = Groups)) +
   		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
   		 theme_dark()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab("Expression") 		 
  }


    if(input$Group.Theme.List == "Test")#theme_test
  {
  	 	 P = ggplot(data= GTable, aes(x= Genes, y=Expression, fill = Groups)) +
   		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
   		 theme_test()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab("Expression") 		 
  }
  
    if(input$Group.Theme.List == "Void")#theme_void
  {
  	 	 P = ggplot(data= GTable, aes(x= Genes, y=Expression, fill = Groups)) +
   		 stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
  		 stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=0.5,size=1)+ 
   		 theme_void()+
   		 scale_fill_manual(values=COLOR.List) + 
   		 theme(axis.text.x = element_text(angle=input$Group.Xaxis.angle, hjust = HJUST, vjust = VJUST)) + 
   		 theme(text = element_text(size=5*(1+input$Group.Font_Size))) + 
   		 ylab("Expression")	 
  }


  plot_list = list(P)
}

			#do.call("grid.arrange", c(list(P), ncol= (6-input$Sample.Graph_Width), nrow= 1))

Report.List.Reads <<- c(Report.List.Reads,plot_list)
GRID.COLS = 6-input$Group.Graph_Width
GRID.ROWS = 1

Group.barplot.sem_GRID.COLS = GRID.COLS
Group.barplot.sem_GRID.ROWS = GRID.ROWS
Group.barplot.sem_plot_list = plot_list

save(list = c("Group.barplot.sem_plot_list","Group.barplot.sem_GRID.ROWS","Group.barplot.sem_GRID.COLS"), file=paste(Backup_Session_Folder,"/Group_barplot_sem.rdata",sep=""))


do.call("grid.arrange", c(plot_list, ncol= GRID.COLS, nrow= GRID.ROWS)) 	

      })#output$Group.barplot.sem <- renderPlot({
      
      
         
      
      if(nrow(Groups.Medians)>=2)
      {
        
        output$Group.heatmap.label <- renderText({"heatmap: Relative gene expression among selected groups"})
        
        shinyjs::show(id = "Group.heatmap_div")
        output$Group.heatmap = renderPlot({
          Font_Adjust = input$Group.Font_Size
          
          Temp.Groups.Medians = Groups.Medians
          for(k in 1:nrow(Groups.Medians))
          {
            if(length(unique(as.numeric(unlist(Temp.Groups.Medians[k,]))))==1)
            {
              Temp.Groups.Medians[k,1]=0.000001
            }#if(length(unique(as.numeric(unlist(Temp.Groups.Medians[1,]))))==1)
          }#for(k in 1:nrow(Groups.Medians))
          
          HM.GP = pheatmap((Temp.Groups.Medians), cluster_cols = FALSE, cluster_rows = TRUE, scale = "row", cellwidth = 15, fontsize_row = 6*Font_Adjust,fontsize_col = 6*Font_Adjust,main = paste("Norm. Exp. of Selected Genes"))

	  Group_heatmap_HM.GP = HM.GP
	  save(list = c("Group_heatmap_HM.GP"), file=paste(Backup_Session_Folder,"/Group_heatmap.rdata",sep=""))

	  HM.GP

        }) ## output$Group.heatmap = renderPlot

        output$Group.members.heatmap.label <- renderText({"heatmap: Relative gene expression among group members"})
        
        shinyjs::show(id = "Group.members.heatmap_div")
        output$Group.members.heatmap = renderPlot({
          
          Font_Adjust = input$Group.Font_Size
          
          colnames(Group.Member.matrix)=Group.Member.matrix.labels
          rownames(Group.Member.matrix)=Group.Compare.GeneList
          
          Temp.Group.Member.matrix = Group.Member.matrix
          for(k in 1:nrow(Temp.Group.Member.matrix))
          {
            if(length(unique(unlist(Temp.Group.Member.matrix[k,])))==1)
            {
              Temp.Group.Member.matrix[k,1]=0.000001
            }#if(length(unique(unlist(Temp.Group.Member.matrix[1,])))==1)
          }#for(k in 1:nrow(Temp.Group.Member.matrix))
          
          HM.Mem.GP = pheatmap(Temp.Group.Member.matrix, cluster_cols = FALSE, cluster_rows = TRUE, scale = "row", cellwidth = 15, fontsize_row = 6*Font_Adjust,fontsize_col = 6*Font_Adjust, main = paste("Norm. Exp. of Selected Genes"))
          
	  Group_heatmap_HM.Mem.GP = HM.Mem.GP
          save(list = c("Group_heatmap_HM.Mem.GP"), file=paste(Backup_Session_Folder,"/Group_mem_heeatmap.rdata",sep=""))
          
	  HM.Mem.GP

        })#output$Group.members.heatmap = renderPlot({
        
      }#if(nrow(Groups.Medians)>=2)
      
      
      
      if((nrow(Groups.Medians)>=3) & (ncol(Groups.Medians)>=3))
      {
        d = dist(t(Groups.Medians))
	fit <- isoMDS(d, k=2)
        
        MDS_Data = data.frame(X = fit$points[,1], Y = fit$points[,2],Samples = colnames(Groups.Medians))
        
        MDS_Distance.plot = ggplot(data=MDS_Data,aes(x=X, y=Y)) +
          geom_text(label=MDS_Data$Samples)+
          theme_bw() + 
          ggtitle("Nonmetric MDS plot using selected genes")
        
        Report.List.Reads <<- c(Report.List.Reads,list(MDS_Distance.plot)) 

        MDS_Distance.plot.points = ggplot(data=MDS_Data,aes(x=X, y=Y, col = Samples)) +
          geom_point()+
          theme_bw()+
          ggtitle("Nonmetric MDS")

        Report.List.Reads <<- c(Report.List.Reads,list(MDS_Distance.plot.points))

        output$Group.MDS_PLOT.Samples.label <- renderText({"Group MDS Plot: Based on selected genes only"})
        
        shinyjs::show(id = "Group.MDS_PLOT.Samples_div")
        output$Group.MDS_PLOT.Samples <- renderPlot({

		      MDS_Distance.plot

        })#output$Group.MDS_PLOT.Samples <- renderPlot({


	Group.MDS_Distance.plot = MDS_Distance.plot
	save(list = c("Group.MDS_Distance.plot"), file=paste(Backup_Session_Folder,"/Group.MDS_PLOT.Samples.rdata",sep=""))  

      }#if((nrow(Groups.Medians)>=3) & (ncol(Groups.Medians)>=3))
      
      
    }#if(length(Group.Compare.Condition)>=1 & length(Group.Compare.GeneList)>=1)
    ##################  
    progress$inc(1, detail = "Comparison Complete")
    Sys.sleep(0.001)
    ######################
  })#obs.Group.Compare.button <- observeEvent(input$Compare.Group.Exp,{
   
#####################################
#####################################
#Reads.data.file.name
   #####################################
   ##################################### 
   #Reads.data.file.name = output$Reads.User_Data_File <- renderText({
   	Reads.data.file.name = function(){

    #output$Reads.User_Data_File <- renderText({
    #Reads.data.file.name  <- function(){

     Reads.Dataset.files = list.files(paste(Reads.Data.folder,"/",input$Reads.Data_User,sep=""))
     Reads.Dataset.files = subset(Reads.Dataset.files,(substr(Reads.Dataset.files,(nchar(Reads.Dataset.files)-4),nchar(Reads.Dataset.files))!=".info" ))
     Reads.data.file.select = input$Reads.Data_set
     updateSelectizeInput(session, inputId="Reads.Data_set", label = "Select Dataset", choices = Reads.Dataset.files,selected = Reads.data.file.select)
     

     Reads.Data_File_path <<- paste(Reads.Data.folder,"/",input$Reads.Data_User,"/",input$Reads.Data_set,sep="")
     #Reads.data.file.name <<- Reads.Data_File_path
     
     
     #return("Load Data")
     return(Reads.Data_File_path)
   }#Reads.data.file.name = output$Reads.User_Data_File <- renderText({
   
   
#####################################
#####################################
#Edgr. Reads.User_Data_Info
   #####################################     
   output$Reads.User_Data_Info <- renderText({
     Reads.Dataset.files = list.files(paste(Reads.Data.folder,"/",input$Reads.Data_User,sep=""))
     Reads.Dataset.files = subset(Reads.Dataset.files,(substr(Reads.Dataset.files,(nchar(Reads.Dataset.files)-4),nchar(Reads.Dataset.files))!=".info" ))
     
     Reads.Data_File_path <<- paste(Reads.Data.folder,"/",input$Reads.Data_User,"/",input$Reads.Data_set,sep="")
     Output_Message = "\nNo File Info Available"
     if(file.exists(paste(Reads.Data_File_path,".info",sep="")))
     {
       dataINFO = readLines(paste(Reads.Data_File_path,".info",sep=""))
       Output_Message = dataINFO
     }
     
   })#output$Reads.User_Data_Info <- renderText({
   
   
#####################################
#####################################
#Edgr.Reads.Reset_Add_Data
   #####################################   	   
   obs.Edgr.Reads.Reset_Add_Data <- observeEvent(input$Reads.Reset_Add_Data,ignoreInit = TRUE,{ 
   	READS_FILE_LIST <<- c() 
   	READ_TABLE_DATA_added <<- c()
   	
   	})#obs.Edgr.Reads.Reset_Add_Data <- observeEvent(input$Reads.Reset_Add_Data,{ 

   	
#####################################
#####################################
#Edgr.RPKM.Reset_Add_Data
   #####################################   	
   	obs.Edgr.RPKM.Reset_Add_Data <- observeEvent(input$RPKM.Reset_Add_Data,ignoreInit = TRUE,{ 
   	RPKM_FILE_LIST <<- c() 
   	RPKM_TABLE_DATA_added <<- c()
   	
   	})#obs.Edgr.RPKM.Reset_Add_Data <- observeEvent(input$RPKM.Reset_Add_Data,{ 
   
   
#####################################
#####################################
#Edgr.Reads.ADD_Data.button
   #####################################
   
   obs.Edgr.Reads.ADD_Data.button <- observeEvent(input$Reads.Add_Data,ignoreInit = TRUE,{ 
   
      isolate(Reads.data.file.name <- Reads.data.file.name())
      inp_source <- input$Reads.Select_input
     
     if(input$Reads.Select_input=="dbase"){file.datapath <<- Reads.data.file.name}
     if(input$Reads.Select_input=="upload"){
       inFile <- input$file1
       if (is.null(inFile)) file.datapath <<- ""
       if (!is.null(inFile)) file.datapath <<- inFile$datapath}
      
     if(file.exists(file.datapath) && !dir.exists(file.datapath)) 
     {
     	File.Check = readLines(file.datapath)
     	if(length(File.Check)==0)
     	  return(NULL)
     	
     	filename = file.datapath         
     	READS_FILE_LIST <<- c(READS_FILE_LIST,filename);  
     	     
 	   	#READ_TABLE_DATA_added       
 	   if(nchar(filename)>=1)
 		{
 			RNAME_List = list()
 			READ_TABLE_List = list()
 			INTERSECT.List = ""
	 				
	 		i = length(READS_FILE_LIST)
	 		
 			New.Data.Table = read.delim(filename)
	 		 	
	 		GENE.ID.List = sapply(New.Data.Table[,2],function(X) if(substr(X,1,3)=="ENS"){return(unlist(strsplit(as.character(X),split="\\."))[1])}else{return(as.character(X))})
 		
	 		ID.INX = which.max(apply(GENE_ID_Match, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
 			ID.INX.max = max(apply(GENE_ID_Match, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
 		
 		
	 		if(length(ID.INX.max)<2) 
	 		{
 				if(colnames(New.Data.Table)[3] != "len")
    	 			{
     				GENE_LENGTHS.inx = which.max(apply(GENE_LENGTHS, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
     				
     				Gene_Lengths.ordered = GENE_LENGTHS$len[match(GENE.ID.List,GENE_LENGTHS[,GENE_LENGTHS.inx])]
     				
     				Gene_Lengths.ordered[which(is.na(Gene_Lengths.ordered))] = 8446
     				
     				New.Data.Table = data.frame(symbol = New.Data.Table[,1], gene_ID = New.Data.Table[,1],len = Gene_Lengths.ordered, New.Data.Table[,2:ncol(New.Data.Table)])
     				
    				}#if(colnames(New.Data.Table)[3] != "len")	

 				GENE.ID.List = sapply(New.Data.Table[,2],function(X) if(substr(X,1,3)=="ENS"){return(unlist(strsplit(as.character(X),split="\\."))[1])}else{return(as.character(X))})
 				ID.INX = which.max(apply(GENE_ID_Match, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
 			
 			}#if(length(ID.INX.max)<2) 
 		
 								
 								
 								
 			New.Data.Table = New.Data.Table[match(GENE_ID_Match[,ID.INX], GENE.ID.List),]
 			New.Data.Table[,2] =GENE_ID_Match[,1]
 			New.Data.Table = New.Data.Table[!is.na(New.Data.Table[,3]),]
	 		New.Data.Table = unique(New.Data.Table)
 		
 			  
 			if(i==1)
 			{								
 				 READ_TABLE_DATA_added <<- New.Data.Table			 
 				 colnames(READ_TABLE_DATA_added)[4:ncol(READ_TABLE_DATA_added)] <<- paste("dataset",i,colnames(READ_TABLE_DATA_added)[4:ncol(READ_TABLE_DATA_added)] ,sep="_")		 			 
 			}#if(i==1)
 			if(i >1)
 			{ 
 				INTERSECT.RNAME.List = intersect(READ_TABLE_DATA_added[,2], New.Data.Table[,2]) 			
 			 		if(length(INTERSECT.RNAME.List)>2)
 			 	{
 					READ_TABLE_DATA_added_temp = READ_TABLE_DATA_added 
 		 			colnames(New.Data.Table)[4:ncol(New.Data.Table)] = paste("dataset",i,colnames(New.Data.Table)[4:ncol(New.Data.Table)] ,sep="_")
 		 			READ_TABLE_DATA_added <<- cbind(READ_TABLE_DATA_added_temp[match(INTERSECT.RNAME.List,READ_TABLE_DATA_added_temp[,2]),], New.Data.Table[match(INTERSECT.RNAME.List,New.Data.Table[,2]),4:ncol(New.Data.Table)])
 	 			}#if(length(INTERSECT.RNAME.List)>2)		 				 		
 			 }#if(i >1)		 	
 			}#if(nchar(filename)>=1)
 		
    	}#if(file.exists(f) && !dir.exists(f)) 	
   })#obs.Edgr.Reads.ADD_Data.button <- observeEvent(input$Reads.Add_Data,{ 
   	
   	
#####################################
#####################################
#Edgr.RPKM.ADD_Data.button
   #####################################
   
   obs.Edgr.RPKM.ADD_Data.button <- observeEvent(input$RPKM.Add_Data,ignoreInit = TRUE,{ 
   
      inp_source <- input$Select_input
    
    if(input$Select_input=="dbase"){isolate(Data_File_path <<- paste(Data.folder,"/",input$Data_User,"/",input$Data_set,sep=""))}
    if(input$Select_input=="upload"){
      inFile.rpkm <- input$file_RPKM
      if (is.null(inFile.rpkm)) Data_File_path <<- ""
      if (!is.null(inFile.rpkm)) Data_File_path <<- inFile.rpkm$datapath}
    
    if(file.exists(Data_File_path) && !dir.exists(Data_File_path)) 
    {
    		File.Check = readLines(Data_File_path)
    		if(length(File.Check)==0)
     	 return(NULL)     
      
     	filename = Data_File_path         
     	RPKM_FILE_LIST <<- c(RPKM_FILE_LIST,filename);  
    }
    #RPKM_TABLE_DATA_added       
    
    if(file.exists(Data_File_path) && !dir.exists(Data_File_path)) 
 	{
 		
 		RNAME_List = list()
 		RPKM_TABLE_List = list()
 		INTERSECT.List = ""
 				
 		i = length(RPKM_FILE_LIST)
 		
 		New.Data.Table = read.delim(filename)
 		 	
 		GENE.ID.List = sapply(New.Data.Table[,1],function(X) if(substr(X,1,3)=="ENS"){return(unlist(strsplit(as.character(X),split="\\."))[1])}else{return(as.character(X))})
 			
 			ID.INX = which.max(apply(GENE_ID_Match, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
 			ID.INX.max = max(apply(GENE_ID_Match, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
 		
 		
	 		if(length(ID.INX.max)<2) 
	 		{

 				GENE.ID.List = sapply(New.Data.Table[,1],function(X) if(substr(X,1,3)=="ENS"){return(unlist(strsplit(as.character(X),split="\\."))[1])}else{return(as.character(X))})
 				ID.INX = which.max(apply(GENE_ID_Match, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
 			
 			}#if(length(ID.INX.max)<2) 

 			  								
 			New.Data.Table = New.Data.Table[match(GENE_ID_Match[,ID.INX], GENE.ID.List),]
 			New.Data.Table[,1] =GENE_ID_Match[,1]
 			New.Data.Table = New.Data.Table[!is.na(New.Data.Table[,3]),]
	 		New.Data.Table = unique(New.Data.Table)
 		
 
 			  
 			  
 		if(i==1)
 		{								
 			 RPKM_TABLE_DATA_added <<- New.Data.Table			 
 			 colnames(RPKM_TABLE_DATA_added)[4:ncol(RPKM_TABLE_DATA_added)] <<- paste("dataset",i,colnames(RPKM_TABLE_DATA_added)[4:ncol(RPKM_TABLE_DATA_added)] ,sep="_")		 			 
 		}#if(i==1)
 		if(i > 1)
 		{ 
 			INTERSECT.RNAME.List = intersect(RPKM_TABLE_DATA_added[,1], New.Data.Table[,1]) 			
 	 		if(length(INTERSECT.RNAME.List)>2)
 		 	{
 				RPKM_TABLE_DATA_added_temp = RPKM_TABLE_DATA_added 
 	 			colnames(New.Data.Table)[4:ncol(New.Data.Table)] = paste("dataset",i,colnames(New.Data.Table)[4:ncol(New.Data.Table)] ,sep="_")
 	 			RPKM_TABLE_DATA_added <<- cbind(RPKM_TABLE_DATA_added_temp[match(INTERSECT.RNAME.List,RPKM_TABLE_DATA_added_temp[,1]),], New.Data.Table[match(INTERSECT.RNAME.List,New.Data.Table[,1]),4:ncol(New.Data.Table)])
 	 		}#if(length(INTERSECT.RNAME.List)>2)		 				 		
 		 }#if(i >1)		 	
 	}
   })#obs.Edgr.RPKM.ADD_Data.button <- observeEvent(input$RPKM.Add_Data,{ 
   
#####################################
#####################################
#EdgeR Load Data obs.Edgr.Reads.Load_Data.button
   #####################################
   #####################################  
observeEvent(input$Reads.Load_Data,ignoreInit = TRUE,{  
  
  #shinyjs::disable("Reads.Load_Data")
  disable("Reads.Load_Data")
  
 
  Groups <<- c()
  Group.Members <<- c()
  PRE_GROUPS <<- ""
  updateSelectizeInput(session, inputId="All_Conditions", label = "Select Dataset", choices =c(),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes (max:12)", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Compare.Conditions", label = "Select Samples", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Control", label = "Select Control", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Treatment", label = "Select Treatment", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes (max:12)", choices = c(),selected = NULL)
  updateTextAreaInput(session,inputId = "Groups",label = "Groups",value="")
  updateSelectizeInput(session, inputId="All_Conditions",selected = "")
  updateSelectizeInput(session, inputId="Group.Compare.Condition", label = "Select Groups", choices = c(),selected = NULL)
  updateTextAreaInput(session,inputId="Gene_Table", label = paste("Gene Table"), value = "")
  updateTextAreaInput(session,inputId="FPKM_Table", label = paste("Reads Table"), value = "")
  updateSelectizeInput(session, inputId="Comparison", label = "Select Library", choices = c())
  updateTextAreaInput(session,inputId="GeneList_Result", label = paste("Differentially Expressed Genes"), value = "")
  updateTextAreaInput(session,inputId="Groups.GeneList_Result", label = paste("Differentially Expressed Genes"), value = "")
  updateSelectizeInput(session, inputId="Groups.Control", label = "Select Group", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Groups.Treatment", label = "Select Group", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId = "Control_GSEA_Test",label = "Select Control",choices = c(), selected = NULL)
  updateSelectizeInput(session, inputId = "Query_GSEA_Test",label = "Select Query",choices = c(), selected = NULL)

  
  output$MDSPlot <- renderPlotly(plot_ly())
  output$ComparePlot1 <- renderPlotly(plot_ly())
  output$ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot1 <- renderPlotly(plot_ly())
  output$Groups.ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Single.Sample.barplot.label <- renderText({" "})
  output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Single.Sample.heatmap.label <- renderText({" "})
  output$Single.Sample.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Stacked_Plot.label <- renderText({" "})
  output$Stacked_Plot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$MDS_PLOT.Samples.label <- renderText({" "})
  output$MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Single.Sample.barplot.label <- renderText({" "})
  output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.boxplot.label <- renderText({" "})
  output$Group.boxplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.barplot.label <- renderText({" "})
  output$Group.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.barplot.sem.label <- renderText({" "})
  output$Group.barplot.sem <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.barplot.maxmin.label <- renderText({" "})
  output$Group.barplot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.heatmap.label <- renderText({" "})
  output$Group.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.members.heatmap.label <- renderText({" "})
  output$Group.members.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.stackedPlot.label <- renderText({" "})
  output$Group.stackedPlot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.stackedPlot.maxmin.label <- renderText({" "})
  output$Group.stackedPlot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.MDS_PLOT.Samples.label <- renderText({" "})
  output$Group.MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$GSEA_PLOT_1 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$GSEA_PLOT_2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  

  DATA.Values_Flag <<- F
  
#######################################################


  ################################   
     progress <- shiny::Progress$new()
     # Make sure it closes when we exit this reactive, even if there's an error
     on.exit(progress$close())
     
     progress$set(message = "Processing Reads", value = 0)
     nstep <- 10
     # Increment the progress bar, and update the detail text.
     progress$inc(1/nstep, detail = paste("Progress: ",10,"%",sep=""))
     Sys.sleep(0.001)
     
     # input$file1 will be NULL initially. After the user selects
     # and uploads a file, it will be a data frame with 'name',
     # 'size', 'type', and 'datapath' columns. The 'datapath'
     # column will contain the local filenames where the data can
     # be found.
    
     isolate(Reads.data.file.name <- Reads.data.file.name())

     inp_source <- input$Reads.Select_input
     
     
     if(input$Reads.Select_input=="dbase" ){file.datapath <<- Reads.data.file.name}
     if(input$Reads.Select_input=="upload"){
       inFile <- input$file1
       if (is.null(inFile)) file.datapath <<- ""
       if (!is.null(inFile)) file.datapath <<- inFile$datapath}
     
     File.Check = c()
     if(file.exists(file.datapath) & !dir.exists(file.datapath)) {File.Check = readLines(file.datapath)}


     if(length(File.Check) < 2)
     {
       enable("Reads.Load_Data")
       shinyalert(title = "File format Error", text="Check file format\n\nPlease use a TAB-delimited file with the below format:\n\n Genes  Sample1  Sample2\nGene1 reads_1 reads_2\nGene2 reads_1 reads_2", type = "error")
       return(NULL)
     }
          

     
     File.Check_columns = sapply(File.Check,function(X) length(strsplit(X,split = "\t")[[1]]))

	 if(!all(File.Check_columns == File.Check_columns[1] & File.Check_columns[1] >= 2))
	 {
	   enable("Reads.Load_Data")
	   shinyalert(title = "File format Error", text="Check file format\n\nPlease use a TAB-delimited file with the below format:\n\n Genes  Sample1  Sample2\nGene1 reads_1 reads_2\nGene2 reads_1 reads_2", type = "error")
       return(NULL)
	 }
    

     
     #######################################################
     	

     progress$inc(2/nstep, detail = paste("Progress: ",20,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################     
     ## edgeR Pipeline ##
     
     ## stolen from Peng ##
     
     # Note: Be careful with dplyr functions. They are wonderfully self-explanatory, but they often (e.g. select, filter) silently drop rownames. #
     # Since rownames = gene_symbol is used for much of the ordering and plotting, I have limited the use of dplyr functions. #
     
   
     filename = file.datapath
     

  
      allData = read.delim(filename);
    
     
     
     if(input$Reads.Select_input=="upload") allData = allData[,c(colnames(allData)[1],input$Reads.Library_select)]

     #######################
################################
	New.Data.Table = allData
	GENE.ID.List = sapply(New.Data.Table[,2],function(X) if(substr(X,1,3)=="ENS"){return(unlist(strsplit(as.character(X),split="\\."))[1])}else{return(as.character(X))})
 		

 		
	 		ID.INX = which.max(apply(GENE_ID_Match, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
 			ID.INX.max = max(apply(GENE_ID_Match, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
 		
 		
	 		if(length(ID.INX.max)<2) 
	 		{
 				if(colnames(New.Data.Table)[3] != "len")
    	 			{
     				GENE_LENGTHS.inx = which.max(apply(GENE_LENGTHS, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
     				
     				Gene_Lengths.ordered = GENE_LENGTHS$len[match(GENE.ID.List,GENE_LENGTHS[,GENE_LENGTHS.inx])]
     				
     				Gene_Lengths.ordered[which(is.na(Gene_Lengths.ordered))] = 8446
     				
     				New.Data.Table = data.frame(symbol = New.Data.Table[,1], gene_ID = New.Data.Table[,1],len = Gene_Lengths.ordered, New.Data.Table[,2:ncol(New.Data.Table)])
     				
    				}#if(colnames(New.Data.Table)[3] != "len")	

 				GENE.ID.List = sapply(New.Data.Table[,2],function(X) if(substr(X,1,3)=="ENS"){return(unlist(strsplit(as.character(X),split="\\."))[1])}else{return(as.character(X))})
 				ID.INX = which.max(apply(GENE_ID_Match, MARGIN=2, function(X) length(which(X %in% GENE.ID.List))))
 			
 			}#if(length(ID.INX.max)<2) 
 		
 													
 								
 			New.Data.Table = New.Data.Table[match(GENE_ID_Match[,ID.INX], GENE.ID.List),]
 			New.Data.Table[,2] =GENE_ID_Match[,1]
 			New.Data.Table = New.Data.Table[!is.na(New.Data.Table[,3]),]
	 		New.Data.Table = unique(New.Data.Table)

			allData = New.Data.Table
		
################
#######

     NORM.COUNTS = F
     if(colnames(allData)[3] != "len")
     {
     	GENE_LENGTHS.inx = which.max(apply(GENE_LENGTHS, MARGIN=2, function(X) length(which(X %in% allData[,1]))))
     			
     	Gene_Lengths.ordered = GENE_LENGTHS$len[match(allData[,1],GENE_LENGTHS[,GENE_LENGTHS.inx])]
     	Gene_Lengths.ordered[which(is.na(Gene_Lengths.ordered))] = 8446
     			     	
     	allData = data.frame(symbol = allData[,1], gene_ID = allData[,1],len = Gene_Lengths.ordered, allData[,2:ncol(allData)])
     }
     

     
     #allData = allData[order(allData$len,decreasing=TRUE),] #order by decreasing length
     
     allData = allData[!duplicated(allData$symbol),] #remove redundant rows
     
     allData = allData[order(allData$symbol),] #order by gene symbol
     
     rownames(allData) = allData$symbol #name rows by gene symbol
     #rownames(allData) = allData$symbol
 

 
#######################################################
#######################################################
     progress$inc(3/nstep, detail = paste("Progress: ",30,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################
     
     REPLICATES=F
     
     
     #################################################################################################################
     
     ## Make a table with normalized reads of all genes ##
     
     #readData <- allData[,c(4:length(colnames(allData)))] #create matrix of just read data
    
     experimentGroup <- colnames(allData)[4:length(colnames(allData))] #make column titles
     
      
     #################################################################################################################

#######################################################
#######################################################
     progress$inc(4/nstep, detail = paste("Progress: ",40,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################
          
     ## Make a table with normalized reads of genes that are expressed significantly ## 
     

     readData <- allData[, c(4:length(colnames(allData)))] #create matrix of just read data
     
     experimentGroup <- colnames(allData)[4:length(colnames(allData))] #make column titles
     
     Norm_Counts_data = readData
     readData <- DGEList(counts = readData, group = experimentGroup) #make DGEList object of reads
     
     if(input$Reads.Raw_Norm == "Raw")
     {
     	counts_per_milli <- cpm(readData, normalized.lib.sizes = F) #unnormalized cpm (norm factors all set to 1 in readData$samples)
     
     	RPKM <- 1000 * counts_per_milli / allData$len #unnormalized rpkm

     
     	#readData <- readData[rowMeans(RPKM > 1) >= 1 & rowSums(RPKM >= 5) >=1,] #eliminate rows with small means and small sums
     	readData <- readData[which(rowSums(RPKM) >=1),] #eliminate rows with small means and small sums
     
     	readData <- calcNormFactors(readData) #normalize "expressed" genes
     
     	logCPM <- cpm(readData, normalized.lib.sizes = TRUE, log = TRUE, prior.count = .125) #log2 counts per million
     
     ## RPKM <- rpkm(readData, gene.length = allData$len, normalized.lib.sizes = TRUE) #testing - this is the new rpkm function
     
     	pengRPKM <- (2**logCPM)*1e3/allData[rownames(logCPM),3] #counts per million divided by gene length
     	pengRPKMTable <- data.frame(allData[rownames(logCPM), c(1,2)], pengRPKM) #make data frame with gene name/symbol and rpkm
	}#if(input$Reads.Raw_Norm == "Raw")
     	
     	
     if(input$Reads.Raw_Norm == "Normalized")	
     {
     	pengRPKM <- Norm_Counts_data*1e3/allData[rownames(Norm_Counts_data),3] #counts per million divided by gene length
     	pengRPKMTable <- data.frame(allData[rownames(Norm_Counts_data), c(1,2)], pengRPKM) #make data frame with gene name/symbol and rpkm
     }
     

#######################################################
#######################################################
     progress$inc(5/nstep, detail = paste("Progress: ",50,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################     
     
     if(REPLICATES==T)
     {
       readData <- estimateCommonDisp(readData)
       
       readData <- estimateTagwiseDisp(readData)
     }else
     {
       readData <- estimateGLMCommonDisp(readData, method = "deviance", robust = TRUE, subset = NULL)
     }
     dataDispersion <- readData$common.dispersion^2
     
      
#######################################################
#######################################################
     progress$inc(6/nstep, detail = paste("Progress: ",60,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################   
     
     if(length(readData[1,])>=3)
     {
     	
     		allData.subset = allData[apply(allData[,4:ncol(allData)], MARGIN=1, function(X) any(X!=0)),]
     		
            allData.tSNE = Rtsne(X= t((allData.subset[,4:ncol(allData.subset)])),dim=2,perplexity=1, max_iter= 1000)$Y
			allData.tSNE = data.frame(allData.tSNE)
			rownames(allData.tSNE) = colnames(allData.subset[,4:ncol(allData.subset)])
            colnames(allData.tSNE) = c("X","Y")

			
			
			plotlyMargins <- list(
          		l = 50,
          		r = 50,
          		b = 100,
          		t = 100,
          		pad = 4
        		)
        
			shinyjs::show(id = "MDSPlot_div")     
     	output$MDSPlot <- renderPlotly({

     		plot_ly(x=allData.tSNE$X, y= allData.tSNE$Y,text=rownames(allData.tSNE)) %>%
            		layout(title = "t-SNE Plot",autosize = F, width = 600, height = 600, margin = plotlyMargins)
              

       	 }) ## output$MDSPlot <- renderPlotly({

	 save(list = c("allData.tSNE","plotlyMargins"), file=paste(Backup_Session_Folder,"/allData.tSNE.rdata",sep=""))      
 
   
     }#if(length(readData[1,])>=3)
#######################################################
#######################################################
     progress$inc(6/nstep, detail = paste("Progress: ",65,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################      
     #################################################################################################################



Gene.Table.Data = paste(apply(pengRPKMTable, MARGIN=1, function(X) paste(X,collapse="\t")),collapse="\n")
Gene.Table.Data = paste(paste(colnames(pengRPKMTable),collapse="\t"),Gene.Table.Data,sep="\n")


#######################################################
#######################################################
     progress$inc(7/nstep, detail = paste("Progress: ",70,"%",sep=""))
     Sys.sleep(0.000001)
     #######################################################  
     #######################################################    
     
     #updateTextAreaInput(session,inputId="Gene_Table", label = paste("Gene Table"), value = Gene.Table.Data)
     #updateTextAreaInput(session,inputId="FPKM_Table", label = paste("Reads Table"), value = Reads.Table.Data)

     updateTextAreaInput(session,inputId="Gene_Table", label = paste("Gene Table"), value = gsub("\t",",",Gene.Table.Data))	
     updateTextAreaInput(session,inputId="FPKM_Table", label = paste("Reads Table"), value = Gene.Table.Data)
     updateSelectizeInput(session, inputId="Comparison", label = "Select Library", choices = experimentGroup, selected = c(experimentGroup[1:min(2,length(experimentGroup))]))
     shinyjs::show(id = "Comparison_Box_wrapper")
     
     #ALL_Genes_List =  rownames(pengRPKMTable)
     #ALL_conditions = c("ALL",colnames(readData))      
     
     pengRPKMTable <<- pengRPKMTable 
     readData <<- readData
     
     #updateSelectInput(session,inputId="All_Genes", label="All Genes",choices = ALL_Genes_List)
     #updateSelectInput(session,inputId="Conditions", label="Conditions",choices = ALL_conditions,selected = 1)
     
     
     ############################################### 
     ###############################################       
    
#######################################################
#######################################################
     progress$inc(8/nstep, detail = paste("Progress: ",80,"% - Analyzing Read data",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################     
  
    Group_Count <<- 0
     Groups <<- c()
     Group.Members <<- c()
     
     
     DATA1=c("")
     #if(nchar(input$Data_set)>1)
     {
       DATA.RAW <- pengRPKMTable[,2:ncol(pengRPKMTable)]
       #colnames(DATA.RAW) = pengRPKMTable[1,2:ncol(pengRPKMTable)]
       GeneList <<- DATA.RAW[,1]
       DATA.temp =data.frame(lapply(DATA.RAW[,2:ncol(DATA.RAW)], as.character), stringsAsFactors=FALSE)
       
       duplicate.genes = unique(GeneList[which(duplicated(GeneList))])
       if(length(duplicate.genes) > 0){
         for(x in 1:length(duplicate.genes))
         {
           temp = DATA.temp[which(as.character(GeneList)==as.character(duplicate.genes[x])),]
           temp = data.frame(lapply(temp, as.numeric), stringsAsFactors=FALSE)
           temp[is.na(temp)] <- 0
           temp.sum = colSums(temp)
           DATA.temp[min(which(as.character(GeneList)==as.character(duplicate.genes[x]))),] = temp.sum
         }
         DATA.temp = DATA.temp[which(!duplicated(GeneList)),]
         GeneList <<- GeneList[which(!duplicated(GeneList))]
       }
       
       DATA.temp =data.frame(lapply(DATA.temp, as.numeric), stringsAsFactors=FALSE)
       DATA.temp[is.na(DATA.temp)] <- 0
       
       isolate(DATA.Values <<- DATA.temp)
       DATA.Values_Flag <<- T
     }
 
     
#######################################################
#######################################################
     progress$inc(9/nstep, detail = paste("Progress: ",90,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################      
     
     DATA.Values.5min <<- DATA.Values[which (GeneList != "-"), ]
     GeneList <<- GeneList[which (GeneList != "-")]
     GeneList <<- GeneList[apply(as.matrix(DATA.Values.5min), MARGIN = 1, function(x) any(x >= 5))]
     DATA.Values.5min <<- DATA.Values.5min[apply(as.matrix(DATA.Values.5min), MARGIN = 1, function(x) any(x >= 5)), ]
     #DATA.Values.5min[DATA.Values.5min=="FPKM"] <<- 0
     #DATA.Values.5min[DATA.Values.5min>100000] <<- 0

     rownames(DATA.Values.5min) <<- GeneList
     #Gene.Choices = as.character(GeneList)
     #Gene.Choices = as.matrix(unique(rownames(DATA.Values.5min)))
     
     
     Reads_Reset <<- T
     

	 save(list = c("DATA.Values","DATA.Values.5min","GeneList","pengRPKMTable","Reads_Reset","DATA.Values_Flag","readData","file.datapath"), file=paste(Backup_Session_Folder,"/READS_VALUES.rdata",sep=""))
         progress$inc(10/nstep, detail = paste("Progress: ",100,"%",sep=""))
 
 
   
   enable("Reads.Load_Data")
   #shinyjs::enable("Reads.Load_Data")
    
 })#-obs.Edgr.Reads.Load_Data.button

     ###############################################    
     ###############################################    
      
      	 
 




     
     
     
#####################################
###############################    

  observe({ 
  	
  	if(length(input$Comparison)>=1)
  	if(Reads_Reset==T & DATA.Values_Flag == T)
  	{
  	
  	  progress <- shiny::Progress$new()
     # Make sure it closes when we exit this reactive, even if there's an error
     on.exit(progress$close())
     
     progress$set(message = "Updating Selection fields", value = 0)
     # Increment the progress bar, and update the detail text.
     progress$inc(1/10, detail = paste("Progress: ",10,"%",sep=""))
     Sys.sleep(0.001)
  	
  	
  	 updateSelectizeInput(session, inputId="All_Conditions", label = "Select Dataset", choices = colnames(DATA.Values[,1:ncol(DATA.Values)]),selected = NULL)

     #updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes (max:12)", choices = Gene.Choices,selected = NULL)
              
     updateSelectizeInput(session, inputId="Single.Compare.Conditions", label = "Select Samples", choices = c("ALL",colnames(DATA.Values)[1:ncol(DATA.Values)]),selected = NULL)
     updateSelectizeInput(session, inputId="Single.Control", label = "Select Control", choices = colnames(DATA.Values)[1:ncol(DATA.Values)],selected = NULL)
             
     updateSelectizeInput(session, inputId="Single.Treatment", label = "Select Treatment", choices = colnames(DATA.Values)[1:ncol(DATA.Values)],selected = NULL)
     #updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes (max:12)", choices = Gene.Choices,selected = NULL)
     updateSelectizeInput(session, inputId = "Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
              
     updateSelectizeInput(session, inputId = "Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
               
     updateSelectizeInput(session, inputId = "DEO_Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
              
     updateSelectizeInput(session, inputId = "DEO_Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
               progress$inc(9.8/10, detail = paste("Progress: ",50,"% Updating Selection fields",sep=""))
     Sys.sleep(0.001)
     updateSelectizeInput(session, inputId = "Control_GSEA_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = colnames(DATA.Values)[1:ncol(DATA.Values)][1])
     
               progress$inc(9.9/10, detail = paste("Progress: ",99,"% Updating Selection fields",sep=""))
     Sys.sleep(0.001)
     updateSelectizeInput(session, inputId = "Query_GSEA_Test",label = "Select Query",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = colnames(DATA.Values)[1:ncol(DATA.Values)][ncol(DATA.Values)])


Reads_Reset <<- F

}#if(Reads_Reset==T & DATA.Values_Flag == T)

               

  	
  	
  	})#observe({ 

   
     
#####################################
#####################################
#EdgeR Sample Pairwise obs.Edgr.Compare.Conditions.button
   #####################################
   ##################################### 
obs.Edgr.Compare.Conditions.button <- observeEvent(input$CompareButton,ignoreInit = TRUE,{  
  
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Calculating Differential Expression", value = 0)
  nstep <- 10
  # Increment the progress bar, and update the detail text.
###############################################         
  progress$inc(1/nstep, detail = paste("Progress: ",10,"%",sep=""))
  Sys.sleep(0.001)
  ############################################### 

  suppressWarnings(suppressPackageStartupMessages(library(DESeq2))) 
  
  output$ComparePlot1 <- renderPlotly(plot_ly())
  output$ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
 
  #Report.List.Reads <<- list()  
  
  
     isolate({
        
       
         
       ###########################################
       ###################################
       ###################################
       ###################################
       
       ## Do a pairwise comparison between experimental conditions ##
       
       ## Volcano Plot, followed by Smear Plot, followed by Heat Map ##
       
       ## Volcano Plot
       
       
       #if(nchar(x12)>5)
       #{readData = read.table(text=gsub("(?<=[a-z])\\s+", "\n", x12, perl=TRUE),header=T,sep=",")
       #}
       
       #if(input$FPKM_Table)
       

      
       if(length(input$Comparison)>1){
         
         
         
         Sample1 = input$Comparison[1]
         Sample2 = input$Comparison[2]
         
         thresholdRatio <- as.numeric(input$Log2FCThreshold)
	 experimentGroup = colnames(readData)
	 rownames(readData) = make.unique(as.character(pengRPKMTable[,2]))
         rownames(pengRPKMTable) = make.unique(as.character(pengRPKMTable[,2]))
		 
	 xLabel <- "logCPM"       
         yLabel <- paste("log2(",Sample2," Expression / ",Sample1," Expression)",sep="")
      

      
           ComparePlot1_MIN.dim = min(log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample1)]),log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample2)]))
           ComparePlot1_MAX.dim = max(log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample1)]),log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample2)]))
           
             
             ComparePlot1_Point_Colors = rep("blue",nrow(pengRPKMTable))
             Abs.Log2FC = abs(log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample2)]/pengRPKMTable[,which(colnames(pengRPKMTable)==Sample1)]))
             ComparePlot1_Point_Colors[which(Abs.Log2FC>=thresholdRatio)] = "red"
             
             
             ComparePlot1_m <- list(
               l = 50,
               r = 50,
               b = 100,
               t = 100,
               pad = 4
             )
       

	     ComparePlot1_Table = pengRPKMTable[,c(Sample1,Sample2)]

	#####################
	shinyjs::show(id = "ComparePlot1_div")
	output$ComparePlot1 <- renderPlotly({ 
          ########
	  ########
             plot_ly(x=log2(ComparePlot1_Table[,1] +1),y=log2(ComparePlot1_Table[,2]+1),
                     text = paste(rownames(ComparePlot1_Table),paste(colnames(ComparePlot1_Table)[1],round(ComparePlot1_Table[,1],2),sep=": "),paste(colnames(ComparePlot1_Table)[2],round(ComparePlot1_Table[,2],2),sep=": "),sep="\n"),
                     marker = list(color = ComparePlot1_Point_Colors,size=3)
                     ) %>%
               layout(autosize = F, width = 500, height = 500, margin = ComparePlot1_m) %>%
               layout(xaxis = list(title = paste(colnames(ComparePlot1_Table)[1],"log2(RPKM + 1)"),range=c(ComparePlot1_MIN.dim,ComparePlot1_MAX.dim)),yaxis = list(title = paste(colnames(ComparePlot1_Table)[2],"log2(RPKM + 1)"), range=c(ComparePlot1_MIN.dim,ComparePlot1_MAX.dim)))
              
             
           })#output$ComparePlot1 <- renderPlot({
        
	save(list = c("ComparePlot1_Table","ComparePlot1_MIN.dim","ComparePlot1_MAX.dim","ComparePlot1_m","ComparePlot1_Point_Colors"), file=paste(Backup_Session_Folder,"/ComparePlot1_Table.rdata",sep="")) 

         
       if(input$Reads.EdgeR_DEseq == "EdgeR")
       { 
           
         #readData = read.table(text=gsub("(?<=[a-z])\\s+", "\n", input$FPKM_Table, perl=TRUE),header=T,sep=",")
         
         #pengRPKMTable = read.table(text=gsub("(?<=[a-z])\\s+", "\n", input$Gene_Table, perl=TRUE), header=T,sep=",")
         experimentGroup = colnames(readData)
         rownames(readData) = make.unique(as.character(pengRPKMTable[,2]))
         rownames(pengRPKMTable) = make.unique(as.character(pengRPKMTable[,2]))
         readData <- DGEList(counts = readData, group = colnames(readData))
         readData <- estimateGLMCommonDisp(readData, method = "deviance", robust = TRUE, subset = NULL)
         dataDispersion <- readData$common.dispersion^2
         
         
         experimentPair <- c(Sample1,Sample2) #choose a pair of experiments to compare
        
         
        an.error.occured <- FALSE
	 tryCatch( {  comparison <- exactTest(readData, pair = experimentPair) }
		 , error = function(e) {an.error.occured <<- TRUE})

	#	 {
	#		shinyalert(title = "Error comparing these samples", text="EdgeR Error:\n\nbinomTest: y1 and y2 must be non-negative\n\nTry DESeq", type = "warning")
     	#	return(NULL)
	#	 }

        if(an.error.occured)
	{
		comparison <- exactTest(readData, pair = experimentPair, dispersion=0.1) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
	}
	
	if(!an.error.occured)
	{
		comparison <- exactTest(readData, pair = experimentPair) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
	}	

 
         #comparison <- exactTest(readData, pair = experimentPair) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
         #comparison <- exactTest(readData, pair = experimentPair, dispersion=0.1) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
	 #comparison <- exactTestDoubleTail(y1 = readData$counts[,Sample1], y2 = readData$counts[,Sample2] , dispersion=0)
	 
         
         comparisonTable <- comparison$table #rename the variable within comparison so RStudio won't complain all the time
         
                 
         mainTitle <- paste(Sample2,"vs.",Sample1,"(FC >",2**thresholdRatio,", p-value <",as.numeric(as.matrix(input$FDR_PvalThreshold)) ,")") #set up a title for graphs
         
         numGenes <- sum(abs(decideTestsDGE(comparison,adjust.method = input$Pval_Correction, p.value = input$FDR_PvalThreshold))) # number of genes with significant differential expression

                 
         if (numGenes > 0) { #check to make sure there is any differential expression
           
           
    
           topGenes <- topTags(comparison, n = numGenes) #top genes
           
           allGenes <- topTags(comparison, n = nrow(comparisonTable)) #all genes
           
           topGenesTable <- topGenes$table[topGenes$table$logCPM > 1,] #filter top genes for logCPM > 1, keep original variable intact
           
           
           allGenesTable <- allGenes$table #rename for ease
           
           FC2 <- rownames(topGenesTable[abs(topGenesTable$logFC) > log2(thresholdRatio),]) #FC2 = rownames of fold change # greater than log2 threshold ratio
           NonSig <- rownames(topGenesTable[abs(topGenesTable$logFC) <= log2(thresholdRatio),]) 
           
           FC2_table <- data.frame(pengRPKMTable[FC2,], topGenesTable[FC2,], 2**topGenesTable[FC2, c(1)]) #make a table of FC2 genes with their expression ratio
           FC2_table_non_sig <- data.frame(pengRPKMTable[NonSig,], topGenesTable[NonSig,], 2**topGenesTable[NonSig, c(1)]) #make a table of monsig genes with their expression ratio

           
           colnames(FC2_table)[length(colnames(FC2_table))] <- "foldChange" #rename last column
           
           #write.table(FC2_table[1:5,], file = paste(Directory,Sample1,".vs.",Sample2,".FDR0.05.FC2.txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE) #make a file
           
           #FC2_table
           
           comparedAllGenes <- data.frame(pengRPKMTable[rownames(allGenesTable),], allGenesTable, 2**allGenesTable[, c(1)]) #make a table of all compared genes
           
           colnames(comparedAllGenes)[length(colnames(comparedAllGenes))] <- "foldChange" #rename last column
           
           #write.table(comparedAllGenes, file = paste(Directory,Sample1,".vs.",Sample2,".allGenes.txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE) #make a file
           
           evenExpression <- setdiff(comparedAllGenes, FC2_table) #make a table of evenly expressed genes

           
           #write.table(evenExpression, file = paste(Directory,Sample1,".vs.",Sample2,".evenExpression.txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE) #make a file

	   updateSliderInput(session = session,inputId = "Reads.Volcano_Genes",label = "Show Volcano Plot Genes",min = 0,max = nrow(FC2_table) ,value = ifelse(input$Reads.Volcano_Genes < nrow(FC2_table),input$Reads.Volcano_Genes,nrow(FC2_table)))
	   
	       }#if (numGenes > 0) { 
	      }#if(input$Reads.EdgeR_DEseq == "EdgeR")
      ########################################
      #########################
      if(input$Reads.EdgeR_DEseq == "DESeq2")
      {
      	counts <- readData$counts
		# converting column gene symbols to rownames
		#rownames(counts) <-  counts[,1]
		#counts$gene_name <-  NULL
		
		
         Sample1 = input$Comparison[1]
         Sample2 = input$Comparison[2]
         
         experimentPair <- c(Sample1,Sample2) #choose a pair of experiments to compare



         an.error.occured <- FALSE
		 tryCatch( {  comparison <- exactTest(readData, pair = experimentPair) }
		 , error = function(e) {an.error.occured <<- TRUE})

		# if(an.error.occured)
		# {
		#	shinyalert(title = "Error comparing these samples", text="Error:\n\nbinomTest: y1 and y2 must be non-negative", type = "warning")
     		#return(NULL)
		# }#if(an.error.occured)

         #comparison <- exactTest(readData, pair = experimentPair) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
        if(an.error.occured)
        {
                comparison <- exactTest(readData, pair = experimentPair, dispersion=0.1) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
        }

        if(!an.error.occured)
        {
                comparison <- exactTest(readData, pair = experimentPair) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
        }


         comparisonTable <- comparison$table #rename the variable within comparison so RStudio won't complain all the time               
         mainTitle <- paste(Sample2,"vs.",Sample1,"(FC >",2**thresholdRatio,", p-value <",as.numeric(as.matrix(input$FDR_PvalThreshold)) ,")") #set up a title for graphs        
         numGenes <- sum(abs(decideTestsDGE(comparison,adjust.method = input$Pval_Correction, p.value = input$FDR_PvalThreshold))) # number of genes with significant differential expression


        coldata_test = data.frame(Treatment = rep(c(Sample1, Sample2),each=2),Samples = rep(c(Sample1, Sample2),each=2))
		rownames(coldata_test) = make.unique(rep(c(Sample1, Sample2),each=2))


		counts_Compare = counts[,which(colnames(counts) %in% c(Sample1, Sample2))]
		counts_Compare = counts_Compare[,c(1,1,2,2)]
		counts_Compare[,c(2,4)] <- counts_Compare[,c(2,4)]+1
	
	
		dds <- DESeqDataSetFromMatrix(countData = counts_Compare,
    		                          colData = coldata_test,
    		                          design = ~Treatment)
		# pre-filtering Step: remove rows with 0 reads

		
		dds <- dds[ rowSums(counts(dds)) > 0, ]


		dds$Treatment <- relevel(dds$Treatment, ref=c(Sample1, Sample2)[1])
		dds$Treatment <- factor(dds$Treatment, levels=c(Sample1, Sample2))
		
		DDS.error.occured <- FALSE
		tryCatch( {  dds <- DESeq(dds) }
		          , error = function(e) {DDS.error.occured <<- TRUE})
		
		if(DDS.error.occured)
		{
		  shinyalert(title = "DESeq2 Error", text="Error in estimateDispersionsFit: all gene-wise dispersion estimates are within 2 orders of magnitude
  from the minimum value, and so the standard curve fitting techniques will not work. This may be because the samples are technical replicates or very similar biological replicates. Please try EdgeR as the function has a manual overide included to avoid this error.", type = "warning")
		  return(NULL)
		}
				
		dds <- DESeq(dds)
		
		res <- results(dds)
		resOrdered <- res[order(res$padj),]

		
		DEG_Results = results(dds, name = resultsNames(dds)[2])


		evenExpression <- data.frame(logFC = DEG_Results$log2FoldChange, PValue = DEG_Results$pvalue, FDR = p.adjust(DEG_Results$padj,method = input$Pval_Correction))
        rownames(evenExpression) = rownames(DEG_Results)
           
       
        numGenes <- sum(abs(decideTestsDGE(comparison,adjust.method = input$Pval_Correction, p.value = input$FDR_PvalThreshold))) # number of genes with significant differential expression
        
        cpm <- DEG_Results$baseMean/sum(DEG_Results$baseMean)*1000000
        
        comparisonTable = data.frame(logFC = DEG_Results$log2FoldChange, logCPM = log2(cpm), PValue = DEG_Results$pvalue, FDR = p.adjust(DEG_Results$padj,method = input$Pval_Correction))
        rownames(comparisonTable) = rownames(DEG_Results)

        
        topGenesTable = comparisonTable
                
        comparedAllGenes <- data.frame(pengRPKMTable[rownames(comparisonTable),],comparison[rownames(comparisonTable),]) #make a table of FC2 genes with their expression ratio
        rownames(comparedAllGenes) = rownames(comparisonTable)

        FC2 <- rownames(comparedAllGenes[abs(comparedAllGenes$logFC) > (thresholdRatio),]) #FC2 = rownames of fold change # greater than log2 threshold ratio
        NonSig <- rownames(comparedAllGenes[abs(comparedAllGenes$logFC) <= (thresholdRatio),]) #FC2 = rownames of fold change # greater than log2 threshold ratio

                
        FC2_table <- data.frame(pengRPKMTable[FC2,], comparisonTable[FC2,]) #make a table of FC2 genes with their expression ratio
        FC2_table_non_sig <- data.frame(pengRPKMTable[NonSig,], comparisonTable[NonSig,]) #make a table of FC2 genes with their expression ratio

        
        
        evenExpression <- setdiff(comparedAllGenes, FC2_table) #make a table of evenly expressed genes


		FC2_table <- FC2_table[order(abs(FC2_table$logFC),decreasing=T),]


        topGenesTable <- topGenesTable[which(topGenesTable$logCPM > 1),]


		#evenExpression = FC2_table

      	 updateSliderInput(session = session,inputId = "Reads.Volcano_Genes",label = "Show Volcano Plot Genes",min = 0,max = nrow(FC2_table) ,value = ifelse(input$Reads.Volcano_Genes < nrow(FC2_table),input$Reads.Volcano_Genes,nrow(FC2_table)))
      	 
      }#if(input$Reads.EdgeR_DEseq == "DESeq2")
      #####################       

          if (numGenes > 0) { 
          	
    #     output$ComparePlot2 = renderPlot({

            Volc.Reads <- ggplot()+
             ggtitle(mainTitle)+ #put in the title
             theme_bw()+ #make background white, must come before theme function call
             theme(plot.title = element_text(hjust = .8, size = 15))+ #center title and make it bigger
             geom_point(data = FC2_table_non_sig, aes(x = logFC, y= -log10(PValue + 1e-300)), alpha = 0.1)+ #plot the even expression points
             geom_point(data = evenExpression, aes(x = logFC, y= -log10(PValue + 1e-300)), alpha = 1/150)+ #plot the even expression points
             geom_point(data = FC2_table, aes(x = logFC, y = -log10(PValue + 1e-300)), color = "darkblue", alpha = 2/5)+ #plot the differentially expressed points
             geom_vline(xintercept = c(-(thresholdRatio),(thresholdRatio)), color = "red")+ #plot the vertical boundary lines
             geom_hline(yintercept = ifelse(is.na(as.numeric(input$FDR_PvalThreshold)),0,-log10(as.numeric(input$FDR_PvalThreshold) + 1e-300)), color = "red")+ #plot the vertical boundary lines
             xlab(paste("log2FC (",Sample2,"/",Sample1,")",sep=""))+
             ylab("-log10(p-value)")+
             geom_text(data = FC2_table[1:ifelse(input$Reads.Volcano_Genes < nrow(FC2_table),input$Reads.Volcano_Genes,nrow(FC2_table)),], aes(x = logFC, y = -log10(PValue + 1e-300)), label = rownames(FC2_table)[1:ifelse(input$Reads.Volcano_Genes < nrow(FC2_table),input$Reads.Volcano_Genes,nrow(FC2_table))], color = "firebrick", size = input$Reads.Volcano_Font_Size, nudge_y = -0.2) #label the diff. expr. points

            shinyjs::show(id = "ComparePlot2_div")
           output$ComparePlot2 = renderPlot({
             #p1 #must be done to render the image
	     PLOT_WIDTH = rep(2:10)
             PLOT_WIDTH[1:input$Reads.Plot_Widths] = 1
             Volc.Reads.plot = grid.arrange(Volc.Reads,layout_matrix = rbind(PLOT_WIDTH))           
	     Volc.Reads.plot = grid.arrange(Volc.Reads,layout_matrix = rbind(PLOT_WIDTH))
	     Report.List.Reads <<- c(Report.List.Reads,list(Volc.Reads.plot))

	     save(list = c("Volc.Reads.plot"), file=paste(Backup_Session_Folder,"/ComparePlot2.rdata",sep="")) 
	     
	     Volc.Reads.plot
	      #Volc.Reads
           })#output$ComparePlot2 
          
         

           ## Smear Plot
           
           yLabel <- paste("log2(",Sample2," Expression / ",Sample1," Expression)",sep="") #change the y label
           
           up <- nrow(topGenesTable[which(comparisonTable$logFC > (thresholdRatio)  & comparisonTable$PValue <= input$FDR_PvalThreshold),]) #number of rows of upregulated genes
           
           down <- nrow(topGenesTable[which(comparisonTable$logFC < -(thresholdRatio)  & comparisonTable$PValue <= input$FDR_PvalThreshold),]) #number of rows of downregulated genes
           
           #plot the compared genes, highlighting the differentially expressed ones
           #mirroring horizontal orange boundary lines
           #vertical grey boundary line
           #label the upregulation region of the graph
           #label the downregulation region of the graph
 			    All_cols = rep("grey",length(comparisonTable$logFC))
			    All_cols[which(abs(comparisonTable$logFC) >= thresholdRatio & comparisonTable$PValue <= input$FDR_PvalThreshold)] = "red"
			    eigth = (max(comparisonTable$logFC)-min(comparisonTable$logFC))/8


           
           
             
             #if(input$Reads.EdgeR_DEseq == "DESeq2")
             {
               shinyjs::show(id = "ComparePlot3_div")
               output$ComparePlot3 = renderPlot({
             	 Plot_smear_Plot = ggplot(comparisonTable, aes(x=logCPM, y=logFC)) + geom_point(col=All_cols)+
  				 theme_bw()+
  				 geom_hline(yintercept = thresholdRatio,col="orange")+
  				 geom_hline(yintercept = -thresholdRatio,col="orange")+
  				 geom_text(label=paste("downregulated=",down),x=min(comparisonTable$logCPM)+(max(comparisonTable$logCPM)-min(comparisonTable$logCPM))/2, y = min(comparisonTable$logFC)+eigth)+
  				 geom_text(label=paste("upregulated=",up),x=min(comparisonTable$logCPM)+(max(comparisonTable$logCPM)-min(comparisonTable$logCPM))/2, y = max(comparisonTable$logFC)-eigth)+
 				 xlab(xLabel)+
  				 ylab(yLabel)
             	 
           save(list = c("Plot_smear_Plot"), file=paste(Backup_Session_Folder,"/ComparePlot3.rdata",sep=""))
           
  				 Plot_smear_Plot

				 
  			  })#output$ComparePlot3
             }#if(input$Reads.EdgeR_DEseq == "DESeq2")
          

	    #save(list = c("Plot_smear_Plot"), file=paste(Backup_Session_Folder,"/ComparePlot3.rdata",sep=""))  

  
           ## Heat Map
           
           FC2_table <- FC2_table[!duplicated(FC2_table$symbol),] #remove duplicated genes from table
           FC2_table.sorted <- FC2_table[order(FC2_table$FDR,decreasing = F),]
           FC2_table = FC2_table.sorted
           
           FC_Genes <- FC2_table.sorted$symbol
           FC_Genes_UP <- FC2_table.sorted$symbol[which(FC2_table.sorted$logFC > 0)]
           FC_Genes_DOWN <- FC2_table.sorted$symbol[which(FC2_table.sorted$logFC < 0)]
           
           GeneList_Table <-  pengRPKMTable[match(FC_Genes,pengRPKMTable$symbol),]
           GeneList_Table_UP <-  pengRPKMTable[match(FC_Genes_UP,pengRPKMTable$symbol),]
           GeneList_Table_DOWN <-  pengRPKMTable[match(FC_Genes_DOWN,pengRPKMTable$symbol),]
       
   			
 
           
           #FC2_table <- FC2_table[,3:(length(experimentGroup)+2)]
           


	  #Creates table with only two columns if user selects to show heatmap with only Compared columns           
	  if(input$Reads.Heatmap_Samples == "Compared Samples")
	  {
	  	FC2_table <- FC2_table[,which(colnames(FC2_table) %in% c(Sample1,Sample2))]
 	  }

          shinyjs::show(id = "ComparePlot4_div")
          output$ComparePlot4 = renderPlot({

          if(input$Reads.Heatmap_Samples != "Compared Samples"){
           if(length(readData[1,])>=3)
           {
             
            
             P6 <- plotMDS.DGEList(readData, main = "Multidimensional Scaling Plot", labels = experimentGroup, top = 200)
             renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
            
 
             P7=P6
             Sorted.distances <- c()
             Sorted.distances[1] = 1
             
             K=1
             for(J in 1:(length(P7$distance.matrix[,1])-1))
             {
               P7$distance.matrix[P7$distance.matrix==0] = 1000
               if(K==1)
               {
                 Dists =c(P7$distance.matrix[K,1:K], P7$distance.matrix[(K+1):length(P7$distance.matrix[,1]),K])
               }else
               {
                 Dists =c(P7$distance.matrix[K,1:(K-1)], P7$distance.matrix[(K):length(P7$distance.matrix[,1]),K])
               }
               
               Sorted.distances[J+1]=which.min(Dists)
               P7$distance.matrix[,K] = 1000
               P7$distance.matrix[K,] = 1000
               K = which.min(Dists)
             }#for(J in 1:(length(P7$distance.matrix[,1])-1))
             
             
             FC2_table <- FC2_table[, Sorted.distances] #re-order columns
           }#if(length(readData[1,])>=3)
          }#if(input$Reads.Heatmap_Samples != "Compared Samples"){
           
             if(length(FC2_table[,1])>=2)
             {
               shinyjs::show(id = "ComparePlot4_div")
               output$ComparePlot4 = renderPlot({
               #pheatmap(log2(FC2_table + 1), cluster_cols = FALSE, cluster_rows = TRUE, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1)"))
               
               		FC2_table.melted = melt(data.frame(Gene=rownames(FC2_table),FC2_table),id.vars = c("Gene"))
               		colnames(FC2_table.melted) = c("Gene","Sample","Expression")

			FC2_table.melted$Expression = log10(FC2_table.melted$Expression+1)

			FC2_table.melted$Gene <- factor(FC2_table.melted$Gene, levels = rownames(FC2_table)[pheatmap(FC2_table)$tree_row[3]$order])

               	       HM.Reads = ggplot(FC2_table.melted,aes(x=Sample,y=Gene,fill=log10(Expression+1))) + 
  			   geom_tile() +
  			   theme_classic()+
        	   	   theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1,size = input$Reads.Heatmap_Font_Size))+
			   theme(axis.text.y = element_text(size = input$Reads.Heatmap_Font_Size))+
  			   theme(axis.title.x = element_blank())+
  			   scale_fill_gradient2(name="log10(Expression+1)", low = "dodgerblue4",mid="white", high ="firebrick2",midpoint=median(log10(FC2_table.melted$Expression+1)), limit=log10(c(min(FC2_table.melted$Expression),max(FC2_table.melted$Expression))+1))


			PLOT_WIDTH = rep(2:10)
			PLOT_WIDTH[1:input$Reads.Plot_Widths] = 1
			HM.Reads.plot = grid.arrange(HM.Reads,layout_matrix = rbind(PLOT_WIDTH))			

			Report.List.Reads <<- c(Report.List.Reads,list(HM.Reads.plot))
			HM.Reads.plot_ComparePlot4 = HM.Reads.plot
			save(list = c("HM.Reads.plot_ComparePlot4"), file=paste(Backup_Session_Folder,"/HM.Reads.plot_ComparePlot4.rdata",sep=""))   
                       HM.Reads.plot

		})#output$ComparePlot4
             }#if(length(FC2_table[,1])>=2)
           
          
           
           if(length(FC2_table[,1])>100){
             #output$ComparePlot5 = renderPlot( pheatmap(log2(FC2_table[1:50,] + 1), cluster_cols = FALSE, cluster_rows = F, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 50 genes ranked")))

             shinyjs::show(id = "ComparePlot5_div")
             output$ComparePlot5 = renderPlot({
                        FC2_table.melted = melt(data.frame(Gene=rownames(FC2_table[1:50,]),FC2_table[1:50,]),id.vars = c("Gene"))

                        colnames(FC2_table.melted) = c("Gene","Sample","Expression")
                        FC2_table.melted$Expression = log10(FC2_table.melted$Expression+1)

			FC2_table.melted$Gene <- factor(FC2_table.melted$Gene, levels = rownames(FC2_table[1:50,])[pheatmap(FC2_table[1:50,])$tree_row[3]$order])	

                       HM.Reads.50 =  ggplot(FC2_table.melted,aes(x=Sample,y=Gene,fill=log10(Expression+1))) +
                           geom_tile() +
                           theme_classic()+
                           theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1,size = input$Reads.Heatmap_Font_Size))+
			   theme(axis.text.y = element_text(size = input$Reads.Heatmap_Font_Size))+
                           theme(axis.title.x = element_blank())+
                           scale_fill_gradient2(name="log10(Expression+1)", low = "dodgerblue4",mid="white", high ="firebrick2",midpoint=median(log10(FC2_table.melted$Expression+1)), limit=log10(c(min(FC2_table.melted$Expression),max(FC2_table.melted$Expression))+1))

		
		        PLOT_WIDTH = rep(2:10)
                        PLOT_WIDTH[1:input$Reads.Plot_Widths] = 1
                        HM.Reads.50.plot = grid.arrange(HM.Reads.50,layout_matrix = rbind(PLOT_WIDTH))
 
			Report.List.Reads <<- c(Report.List.Reads,list(HM.Reads.50.plot))
			save(list = c("HM.Reads.50.plot"), file=paste(Backup_Session_Folder,"/HM.Reads.50.plot.rdata",sep=""))  
			HM.Reads.50.plot

               })#output$ComparePlot5
             #output$ComparePlot6 = renderPlot(pheatmap(log2(FC2_table[1:50,] + 1), cluster_cols = FALSE, cluster_rows = T, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 50 genes clustered")))
             #output$ComparePlot7 = renderPlot(pheatmap(log2(FC2_table[1:100,] + 1), cluster_cols = FALSE, cluster_rows = F, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 100 genes ranked")))

                        shinyjs::show(id = "ComparePlot6_div")
                        output$ComparePlot6 = renderPlot({
                        FC2_table.melted = melt(data.frame(Gene=rownames(FC2_table[1:100,]),FC2_table[1:100,]),id.vars = c("Gene"))

                        colnames(FC2_table.melted) = c("Gene","Sample","Expression")
                        FC2_table.melted$Expression = log10(FC2_table.melted$Expression+1)

                        FC2_table.melted$Gene <- factor(FC2_table.melted$Gene, levels = rownames(FC2_table[1:100,])[pheatmap(FC2_table[1:100,])$tree_row[3]$order])

                       HM.Reads.100 =  ggplot(FC2_table.melted,aes(x=Sample,y=Gene,fill=log10(Expression+1))) +
                           geom_tile() +
                           theme_classic()+
                           theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1,size = input$Reads.Heatmap_Font_Size))+
                           theme(axis.text.y = element_text(size = input$Reads.Heatmap_Font_Size))+
                           theme(axis.title.x = element_blank())+
                           scale_fill_gradient2(name="log10(Expression+1)", low = "dodgerblue4",mid="white", high ="firebrick2",midpoint=median(log10(FC2_table.melted$Expression+1)), limit=log10(c(min(FC2_table.melted$Expression),max(FC2_table.melted$Expression))+1))


                        PLOT_WIDTH = rep(2:10)
                        PLOT_WIDTH[1:input$Reads.Plot_Widths] = 1
                        HM.Reads.100.plot = grid.arrange(HM.Reads.100,layout_matrix = rbind(PLOT_WIDTH))

                        Report.List.Reads <<- c(Report.List.Reads,list(HM.Reads.100.plot))

			save(list = c("HM.Reads.100.plot"), file=paste(Backup_Session_Folder,"/HM.Reads.100.plot.rdata",sep=""))

                        HM.Reads.100.plot
               })#output$ComparePlot6


             #output$ComparePlot8 = renderPlot(pheatmap(log2(FC2_table[1:100,] + 1), cluster_cols = FALSE, cluster_rows = T, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 100 genes clustered")))
             
           }#if(length(FC2_table[,1])>100)
           
          })#output$ComparePlot4
   
   
          
          #########################################################
           ##### Human GENE ENSEMBL IDs to Human GENE symbols ######
           #########################################################
           FC_Genes <- as.character(FC_Genes)         
           FC_Genes_Symbols = as.character(FC_Genes)
           if(length(FC_Genes)>0)
           {
           	  if(any(FC_Genes %in% GENE_ID_Match$Gene.stable.ID))
           	  {
           	  	FC_Genes_Symbols[which(FC_Genes %in% GENE_ID_Match$Gene.stable.ID)] = as.character(GENE_ID_Match$Gene.name[match(FC_Genes[which(FC_Genes %in% GENE_ID_Match$Gene.stable.ID)],GENE_ID_Match$Gene.stable.ID)])
           	  }
           } 
           
           FC_Genes_UP <- as.character(FC_Genes_UP)
           FC_Genes_UP_Symbols = as.character(FC_Genes_UP)
           if(length(FC_Genes_UP)>0)
           {
           	  if(any(FC_Genes_UP %in% GENE_ID_Match$Gene.stable.ID))
           	  {
           	  	FC_Genes_UP_Symbols[which(FC_Genes_UP %in% GENE_ID_Match$Gene.stable.ID)] = as.character(GENE_ID_Match$Gene.name[match(FC_Genes_UP[which(FC_Genes_UP %in% GENE_ID_Match$Gene.stable.ID)],GENE_ID_Match$Gene.stable.ID)])
           	  }
           }
           
           FC_Genes_DOWN <- as.character(FC_Genes_DOWN)
           FC_Genes_DOWN_Symbols = as.character(FC_Genes_DOWN)
           if(length(FC_Genes_DOWN)>0)
           {
           	  if(any(FC_Genes_DOWN %in% GENE_ID_Match$Gene.stable.ID))
           	  {
           	  	FC_Genes_DOWN_Symbols[which(FC_Genes_DOWN %in% GENE_ID_Match$Gene.stable.ID)] = as.character(GENE_ID_Match$Gene.name[match(FC_Genes_DOWN[which(FC_Genes_DOWN %in% GENE_ID_Match$Gene.stable.ID)],GENE_ID_Match$Gene.stable.ID)])
           	  }
           }
           
           #########################################################
           ##### Mouse GENE ENSEMBL IDs to Mouse GENE Name ######
           #########################################################
           

           if(length(FC_Genes)>0)
           {
           	  if(any(FC_Genes %in% GENE_ID_Match$Mouse.gene.stable.ID))
           	  {
           	  	FC_Genes <- as.character(FC_Genes)          
           	    FC_Genes_Symbols = as.character(FC_Genes)
           	  	#FC_Genes_Symbols[which(FC_Genes %in% GENE_ID_Match$Mouse.gene.stable.ID)] = as.character(GENE_ID_Match$Mouse.gene.name[match(FC_Genes[which(FC_Genes %in% GENE_ID_Match$Mouse.gene.stable.ID)],GENE_ID_Match$Mouse.gene.stable.ID)])
           	  }
           } 
           
           
           if(length(FC_Genes_UP)>0)
           {
           	  if(any(FC_Genes_UP %in% GENE_ID_Match$Mouse.gene.stable.ID))
           	  {
           	  	FC_Genes_UP <- as.character(FC_Genes_UP)
          		FC_Genes_UP_Symbols = as.character(FC_Genes_UP)
           	  	#FC_Genes_UP_Symbols[which(FC_Genes_UP %in% GENE_ID_Match$Mouse.gene.stable.ID)] = as.character(GENE_ID_Match$Mouse.gene.name[match(FC_Genes_UP[which(FC_Genes_UP %in% GENE_ID_Match$Mouse.gene.stable.ID)],GENE_ID_Match$Mouse.gene.stable.ID)])
           	  }
           }
           
           
           if(length(FC_Genes_DOWN)>0)
           {
           	  if(any(FC_Genes_DOWN %in% GENE_ID_Match$Mouse.gene.stable.ID))
           	  {
           	  	FC_Genes_DOWN <- as.character(FC_Genes_DOWN)
           		FC_Genes_DOWN_Symbols = as.character(FC_Genes_DOWN)
           	  	#FC_Genes_DOWN_Symbols[which(FC_Genes_DOWN %in% GENE_ID_Match$Mouse.gene.stable.ID)] = as.character(GENE_ID_Match$Mouse.gene.name[match(FC_Genes_DOWN[which(FC_Genes_DOWN %in% GENE_ID_Match$Mouse.gene.stable.ID)],GENE_ID_Match$Mouse.gene.stable.ID)])
           	  }
           }
            #########################################################
             #########################################################
              #########################################################
 
   
   
           updateTextAreaInput(session,inputId="GeneList_Result", label = paste("Differentially Expressed Genes",mainTitle), value = paste(FC_Genes_Symbols,collapse="\n"))
           updateTextAreaInput(session,inputId="GeneList_Result_UP", label = paste("Up Regulated Genes",mainTitle), value = paste(FC_Genes_UP_Symbols,collapse="\n"))
           updateTextAreaInput(session,inputId="GeneList_Result_DOWN", label = paste("Down Regulated Genes",mainTitle), value = paste(FC_Genes_DOWN_Symbols,collapse="\n"))
          
 
	   output$FC_List_Download <- downloadHandler(
             filename = function() {
               paste("DE_List.txt", sep = "")
             },
             content = function(file) {
               write(paste(FC_Genes_Symbols,collapse="\n"),file=file)
             })

	   output$FC_Up_List_Download <- downloadHandler(
             filename = function() {
               paste("DE_UP_List.txt", sep = "")
             },
             content = function(file) {
               write(FC_Genes_UP_Symbols,file=file)
             })

	    output$FC_Down_List_Download <- downloadHandler(
             filename = function() {
               paste("DE_DOWN_List.txt", sep = "")
             },
             content = function(file) {
               write(FC_Genes_DOWN_Symbols,file=file)
             })

           output$FC_Download <- downloadHandler(
             filename = function() {
               paste("DE_FC.txt", sep = "")
             },
             content = function(file) {
               write.table(FC2_table.sorted, file,quote=F,row.names = F)
             })
           output$FC_Download_UP <- downloadHandler(
             filename = function() {
               paste("DE_FC_Upregulated.txt", sep = "")
             },
             content = function(file) {
               write.table(FC2_table.sorted[which(FC2_table.sorted$logFC > 0),], file,quote=F,row.names = F)
             })

           
           output$FC_Download_DOWN <- downloadHandler(
             filename = function() {
               paste("DE_FC_DownRegulated.txt", sep = "")
             },
             content = function(file) {
               
               write.table(FC2_table.sorted[which(FC2_table.sorted$logFC < 0),],quote=F, file,row.names = F)
             })
           
           
           output$FC_GeneList <- downloadHandler(
             filename = function() {
               paste("DE_GeneList.txt", sep = "")
             },
             content = function(file) {
               write.table(GeneList_Table, file,quote=F,row.names = F)
             })
           output$FC_GeneList_UP <- downloadHandler(
             filename = function() {
               paste("DE_GeneList_Upregulated.txt", sep = "")
             },
             content = function(file) {
               write.table(GeneList_Table_UP, file,quote=F,row.names = F)
             })
           
           
           output$FC_GeneList_DOWN <- downloadHandler(
             filename = function() {
               paste("DE_GeneList_DownRegulated.txt", sep = "")
             },
             content = function(file) {
               
               write.table(GeneList_Table_DOWN,quote=F, file,row.names = F)
             })

         }#if (numGenes > 0)
         
         updateSelectizeInput(session, inputId="Comparison", label = "Select Library", choices = experimentGroup,selected = c(Sample1,Sample2))
         shinyjs::show(id = "Comparison_Box_wrapper")
         
      
         #################################################################################################################
         
         ##################################
         ##################################
         ##################################
         ##################################
       }#if(length(input$Comparison)>1){
       #})#
   
       
     })#isolate
     
   
     #################################
     #################################
     #################################

   
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("RPKM_data", ".csv", sep = "")
     },
     content = function(file) {
       write(input$Gene_Table, file)
  })


  output$downloadFPKM <- downloadHandler(
     filename = function() {
       paste("RPKM_data", ".txt", sep = "")
     },   
     content = function(file) {
       write(input$FPKM_Table, file)
  }) 


#output$Download_Summary_Plots <- downloadHandler(
#    filename = function() { paste(input$dataset, '.png', sep='') },
#    content = function(file) {
#        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
#        ggsave(file, plot = HM.Reads, device = device)
#    }
#)


   
###############################################         
   progress$inc((9/nstep), detail = paste("Progress: ",99,"%",sep=""))
   Sys.sleep(0.001)
   ###############################################   
})#-obs.Edgr.Compare.Conditions.button

 
#####################################
#####################################
#EdgeR Group Pairwise obs.Edgr.Compare.C.button
   #####################################
   #####################################    
   obs.Edgr.Compare.Groups.button <- observeEvent(input$Groups.CompareButton,ignoreInit = TRUE,{
     
     if(length(pengRPKMTable) <= 1)
     {
     	shinyalert(title = "Load counts/reads file", text="Load counts/reads data in the 'Load Count Data' tool, then create groups before using this tool.", type = "warning")
     	return(NULL)
     }
     
     
    Orig_pengRPKMTable <- pengRPKMTable
#####################
     progress <- shiny::Progress$new()
     # Make sure it closes when we exit this reactive, even if there's an error
     on.exit(progress$close())
     progress$set(message = "Comparing Groups", value = 0)
     # Increment the progress bar, and update the detail text.
     progress$inc(0.3, detail = "Please Wait")
     Sys.sleep(0.001)
################
     
     output$Groups.ComparePlot1 <- renderPlotly(plot_ly())
     output$Groups.ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     
     
       isolate({
         ###########################################
         ###################################
         ###################################
         ###################################
         
         ## Do a pairwise comparison between experimental conditions ##
         
         ## Volcano Plot, followed by Smear Plot, followed by Heat Map ##
         
         ## Volcano Plot
         
         

         
         #if(input$FPKM_Table)
         if(length(input$Groups.Control)==1 & length(input$Groups.Treatment)==1){
           
             
           
           
           isolate(Sample1 <- input$Groups.Control)
           isolate(Sample2 <- input$Groups.Treatment)
           
           
           if(nchar(Sample1) == 0 | nchar(Sample2) == 0)
           {
           	  shinyalert(title = "Create Groups", text="Create at least 2 groups with the 'Create Groups' tool before using this tool.", type = "warning")
           	  return(NULL)
           }
           
           if(Sample1 == Sample2)
           {
           	  shinyalert(title = "Select Different Groups", text="Select two differrent groups to perform this analysis.", type = "warning")
           	  return(NULL)
           }
           
           
           
           Group.Index = c()
           Group.Index[1] = which(Groups == Sample1)
           Group.Index[2] = which(Groups == Sample2)
           
           
           xLabel <- "logCPM"
           
           yLabel <- paste("log2(",Sample2," Expression / ",Sample1," Expression)",sep="")
           
           
           #Groups.readData.Medians[l,k] = median(as.numeric(as.matrix(readData[,match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))

			Group1_Members = unlist(strsplit(Group.Members[Group.Index[1]],split=";"))
			Group2_Members = unlist(strsplit(Group.Members[Group.Index[2]],split=";"))
			
			if(length(Group1_Members) == 1){Group1_Members = c(Group1_Members,Group1_Members)}
			if(length(Group2_Members) == 1){Group2_Members = c(Group2_Members,Group2_Members)}


           Groups.readData.ctrl = readData[,match(Group1_Members,as.character(unlist(colnames(readData))))]
           Groups.readData.treat = readData[,match(Group2_Members,as.character(unlist(colnames(readData))))]
           
           Groups.readData.ctrl.medians = apply(Groups.readData.ctrl,1, median, na.rm = TRUE)
           Groups.readData.treat.medians = apply(Groups.readData.treat,1, median, na.rm = TRUE)

           Groups.readData.med = cbind(Groups.readData.ctrl.medians,Groups.readData.treat.medians)
           colnames(Groups.readData.med) = as.character(c(Sample1,Sample2))
           
           Groups.pengRPKMTable.ctrl = pengRPKMTable[,match(Group1_Members,as.character(unlist(colnames(pengRPKMTable))))]
           Groups.pengRPKMTable.treat = pengRPKMTable[,match(Group2_Members,as.character(unlist(colnames(pengRPKMTable))))]
           
           Groups.pengRPKMTable.ctrl.medians = apply(Groups.pengRPKMTable.ctrl,1, median, na.rm = TRUE)
           Groups.pengRPKMTable.treat.medians = apply(Groups.pengRPKMTable.treat,1, median, na.rm = TRUE)
           
           Groups.pengRPKMTable.med = cbind(pengRPKMTable[,1:2],Groups.pengRPKMTable.ctrl.medians,Groups.pengRPKMTable.treat.medians)
           colnames(Groups.pengRPKMTable.med) = as.character(c("gene_name","symbol",Sample1,Sample2))
           
           pengRPKMTable=Groups.pengRPKMTable.med
           readData = Groups.readData.med
           
           
           
           #readData = read.table(text=gsub("(?<=[a-z])\\s+", "\n", input$FPKM_Table, perl=TRUE),header=T,sep=",")
           
           #pengRPKMTable = read.table(text=gsub("(?<=[a-z])\\s+", "\n", input$Gene_Table, perl=TRUE), header=T,sep=",")
           
           experimentGroup = colnames(readData)
           rownames(readData) = make.unique(as.character(pengRPKMTable[,2]))
           rownames(pengRPKMTable) = make.unique(as.character(pengRPKMTable[,2]))
           readData <- DGEList(counts = readData, group = colnames(readData))
           readData <- estimateGLMCommonDisp(readData, method = "deviance", robust = TRUE, subset = NULL)
           dataDispersion <- readData$common.dispersion^2
           
           
           experimentPair <- c(Sample1,Sample2) #choose a pair of experiments to compare
           
           comparison <- exactTest(readData, pair = experimentPair) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
           
           comparisonTable <- comparison$table #rename the variable within comparison so RStudio won't complain all the time
           
           thresholdRatio <- as.numeric(input$Groups.Log2FCThreshold)
           
           mainTitle <- paste(Sample2,"vs.",Sample1,"(FC >",2**thresholdRatio,", p-value <",as.numeric(as.matrix(input$Groups.FDR_PvalThreshold)) ,")") #set up a title for graphs
           
           numGenes <- sum(abs(decideTestsDGE(comparison,adjust.method = input$Groups.Pval_Correction, p.value = input$Groups.FDR_PvalThreshold))) # number of genes with significant differential expression
           
           if (numGenes > 0) { #check to make sure there is any differential expression
             
          
             Group.Compare_MIN.dim = min(log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample1)]),log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample2)]))
             Group.Compare_MAX.dim = max(log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample1)]),log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample2)]))
            
            
	     
 
             ##################
             ##output$Groups.ComparePlot1 = renderPlotly({ 
               #######
               #######
               Group.Compare_Point_Colors = rep("blue",nrow(pengRPKMTable))
               Abs.Log2FC = abs(log2(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample2)]/pengRPKMTable[,which(colnames(pengRPKMTable)==Sample1)]))
               Group.Compare_Point_Colors[which(Abs.Log2FC>=thresholdRatio)] = "red"
               
               Group.Compare_m <- list(
                 l = 50,
                 r = 50,
                 b = 100,
                 t = 100,
                 pad = 4
               )


	      Group.CompareTable1 = pengRPKMTable[,c(Sample1,Sample2)]

	   #########################   
	      shinyjs::show(id = "Groups.ComparePlot1_div")
 	      output$Groups.ComparePlot1 = renderPlotly({ 
		#########
		#########
               plot_ly(x=log2(Group.CompareTable1[,1]+1),y=log2(Group.CompareTable1[,2]+1),
                       text = paste(rownames(Group.CompareTable1),paste(colnames(Group.CompareTable1)[1],round(Group.CompareTable1[,1],2),sep=": "),paste(colnames(Group.CompareTable1)[2],round(Group.CompareTable1[,2],2),sep=": "),sep="\n"),
                       marker = list(color = Group.Compare_Point_Colors,size=3)
                      ) %>%
                 layout(autosize = F, width = 500, height = 500, margin = Group.Compare_m) %>%
                 layout(xaxis = list(title = paste(colnames(Group.CompareTable1)[1],"log2(RPKM)"),range=c(Group.Compare_MIN.dim,Group.Compare_MAX.dim)),yaxis = list(title = paste(colnames(Group.CompareTable1)[2],"log2(RPKM)"), range=c(Group.Compare_MIN.dim,Group.Compare_MAX.dim)))
               
             })#output$Groups.ComparePlot1 = renderPlotly({ 
             
		            
            save(list = c("Group.CompareTable1","Group.Compare_MIN.dim","Group.Compare_MIN.dim","Group.Compare_m","Group.Compare_Point_Colors"), file=paste(Backup_Session_Folder,"/Group.ComparePlot1_Table.rdata",sep=""))

         
             topGenes <- topTags(comparison, n = numGenes) #top genes
             
             allGenes <- topTags(comparison, n = nrow(comparisonTable)) #all genes
             
             topGenesTable <- topGenes$table[topGenes$table$logCPM > 1,] #filter top genes for logCPM > 1, keep original variable intact
             
             allGenesTable <- allGenes$table #rename for ease
             
             FC2 <- rownames(topGenesTable[abs(topGenesTable$logFC) > log2(thresholdRatio),]) #FC2 = rownames of fold change # greater than log2 threshold ratio
             
             FC2_table <- data.frame(pengRPKMTable[FC2,], topGenesTable[FC2,], 2**topGenesTable[FC2, c(1)]) #make a table of FC2 genes with their expression ratio
             
             colnames(FC2_table)[length(colnames(FC2_table))] <- "foldChange" #rename last column
             
             
             
             comparedAllGenes <- data.frame(pengRPKMTable[rownames(allGenesTable),], allGenesTable, 2**allGenesTable[, c(1)]) #make a table of all compared genes
             
             colnames(comparedAllGenes)[length(colnames(comparedAllGenes))] <- "foldChange" #rename last column
             

             evenExpression <- setdiff(comparedAllGenes, FC2_table) #make a table of evenly expressed genes
             
	       updateSliderInput(session = session,inputId = "Groups.Reads.Volcano_Genes",label = "Show Volcano Plot Genes",min = 0,max = nrow(FC2_table) ,value = ifelse(input$Groups.Reads.Volcano_Genes < nrow(FC2_table),input$Groups.Reads.Volcano_Genes,nrow(FC2_table)))
             
               Volc.Groups.Reads <- ggplot(xlab = paste("log2FC (",Sample2,"/",Sample1,")",sep=""), ylab = "-log10(p-value)")+
               ggtitle(mainTitle)+ #put in the title
               theme_bw()+ #make background white, must come before theme function call
               theme(plot.title = element_text(hjust = .8, size = 15))+ #center title and make it bigger
               geom_point(data = evenExpression, aes(x = logFC, y= -log10(PValue + 1e-300)), alpha = 1/150)+ #plot the even expression points
               geom_point(data = FC2_table, aes(x = logFC, y = -log10(PValue + 1e-300)), color = "darkblue", alpha = 2/5)+ #plot the differentially expressed points
               geom_vline(xintercept = c(-log2(thresholdRatio), log2(thresholdRatio)), color = "forestgreen")+ #plot the vertical boundary lines
               #geom_text(data = FC2_table, aes(x = logFC, y = -log10(PValue + 1e-300)), label = rownames(FC2_table), color = "firebrick", size = 2, nudge_y = -1) #label the diff. expr. points
               geom_text(data = FC2_table[1:ifelse(input$Groups.Reads.Volcano_Genes < nrow(FC2_table),input$Groups.Reads.Volcano_Genes,nrow(FC2_table)),], aes(x = logFC, y = -log10(PValue + 1e-300)), label = rownames(FC2_table)[1:ifelse(input$Groups.Reads.Volcano_Genes < nrow(FC2_table),input$Groups.Reads.Volcano_Genes,nrow(FC2_table))], color = "firebrick", size = input$Groups.Reads.Volcano_Font_Size, nudge_y = -0.2) #label the diff. expr. points

               shinyjs::show(id = "Groups.ComparePlot2_div")
               output$Groups.ComparePlot2 = renderPlot({
               
		PLOT_WIDTH = rep(2:10)
             	PLOT_WIDTH[1:input$Reads.Plot_Widths] = 1
             	Volc.Groups.Reads.plot = grid.arrange(Volc.Groups.Reads,layout_matrix = rbind(PLOT_WIDTH))
             	Report.List.Reads <<- c(Report.List.Reads,list(Volc.Groups.Reads.plot))
		save(list = c("Volc.Groups.Reads.plot"), file=paste(Backup_Session_Folder,"/Volc.Groups.Reads.plot.rdata",sep=""))

             	Volc.Groups.Reads.plot
		#p1 #must be done to render the image
                
             })#output$ComparePlot2 
             
             ## Smear Plot
             
             yLabel <- paste("log2(",Sample2," Expression / ",Sample1," Expression)",sep="") #change the y label
             
             up <- nrow(topGenesTable[topGenesTable$logFC > log2(thresholdRatio),]) #number of rows of upregulated genes
             
             down <- nrow(topGenesTable[topGenesTable$logFC < -log2(thresholdRatio),]) #number of rows of downregulated genes
             
             #plot the compared genes, highlighting the differentially expressed ones
             #mirroring horizontal orange boundary lines
             #vertical grey boundary line
             #label the upregulation region of the graph
             #label the downregulation region of the graph
             

		GC3_comparison = comparison
		GC3_topGenesTable = topGenesTable
		GC3_mainTitle = mainTitle
		GC3_xLabel = xLabel
		GC3_yLabel = yLabel
		GC3_up = up
		GC3_down = down

		shinyjs::show(id = "Groups.ComparePlot3_div")
		output$Groups.ComparePlot3 = renderPlot({

                 
		  plotSmear(comparison, de.tags = rownames(topGenesTable), main = mainTitle, xlab = xLabel, ylab = yLabel, ylim = c(-5,5), col = "grey")
		  abline(h=c(-1, 1), col = rep("orange", 2))
		  abline(v=1, col = "grey")
		  text(8, 3, paste("upregulated=",up))
		  text(8, -3, paste("downregulated=",down))
		  
		})#output$ComparePlot3

		save(list = c("GC3_comparison","GC3_topGenesTable","GC3_mainTitle","GC3_xLabel","GC3_yLabel","GC3_up","GC3_down"), file=paste(Backup_Session_Folder,"/plotSmear.Groups.Reads.plot.rdata",sep=""))

             #grid.arrange(p1, p2, ncol = 2, nrow = 2)
#################
             progress$inc(1, detail = "Please Wait: 65%")
             Sys.sleep(0.001)  
##################                          
             ## Heat Map
             
             FC2_table <- FC2_table[!duplicated(FC2_table$symbol),] #remove duplicated genes from table
             FC2_table.sorted <- FC2_table[order(FC2_table$FDR,decreasing = F),]
             FC2_table = FC2_table.sorted
             
             #FC_Genes <- FC2_table.sorted$symbol
             FC_Genes <- rownames(FC2_table.sorted)
             FC_Genes_UP <- FC2_table.sorted$symbol[which(FC2_table.sorted$logFC > 0)]
             FC_Genes_DOWN <- FC2_table.sorted$symbol[which(FC2_table.sorted$logFC < 0)]
             
             GeneList_Table <-  Orig_pengRPKMTable[match(FC_Genes,Orig_pengRPKMTable$symbol),]
             GeneList_Table_UP <-  Orig_pengRPKMTable[match(FC_Genes_UP,Orig_pengRPKMTable$symbol),]
             GeneList_Table_DOWN <-  Orig_pengRPKMTable[match(FC_Genes_DOWN,Orig_pengRPKMTable$symbol),]
             
             #FC_Genes <- FC2_table$symbol
             rownames(FC2_table) <- FC2_table$symbol #name rows for gene symbols
             
             
             FC2_table <- FC2_table[,3:(length(experimentGroup)+2)]

	     if(input$Groups.Reads.Heatmap_Groups == "Compared Groups")
             {  
                 FC2_table <- FC2_table[,which(colnames(FC2_table) %in% c(Sample1,Sample2))]
             }
              
             shinyjs::show(id = "Groups.ComparePlot4_div")
             output$Groups.ComparePlot4 = renderPlot({
               if(length(FC2_table[,1])>=2)
	       {
			FC2_table.melted = melt(data.frame(Gene=rownames(FC2_table),FC2_table),id.vars = c("Gene"))
                        colnames(FC2_table.melted) = c("Gene","Sample","Expression")

                        FC2_table.melted$Expression = log10(FC2_table.melted$Expression+1)

                        FC2_table.melted$Gene <- factor(FC2_table.melted$Gene, levels = rownames(FC2_table)[pheatmap(FC2_table)$tree_row[3]$order])

                       HM.Groups.Reads = ggplot(FC2_table.melted,aes(x=Sample,y=Gene,fill=log10(Expression+1))) +
                           geom_tile() +
                           theme_classic()+
                           theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1,size = input$Groups.Reads.Heatmap_Font_Size))+
                           theme(axis.text.y = element_text(size = input$Groups.Reads.Heatmap_Font_Size))+
                           theme(axis.title.x = element_blank())+
                           scale_fill_gradient2(name="log10(Expression+1)", low = "dodgerblue4",mid="white", high ="firebrick2",midpoint=median(log10(FC2_table.melted$Expression+1)), limit=log10(c(min(FC2_table.melted$Expression),max(FC2_table.melted$Expression))+1))


                        PLOT_WIDTH = rep(2:10)
                        PLOT_WIDTH[1:input$Groups.Reads.Plot_Widths] = 1
                        HM.Groups.Reads.plot = grid.arrange(HM.Groups.Reads,layout_matrix = rbind(PLOT_WIDTH))

                        Report.List.Reads <<- c(Report.List.Reads,list(HM.Groups.Reads.plot))
			save(list = c("HM.Groups.Reads.plot"), file=paste(Backup_Session_Folder,"/HM.Groups.Reads.plot.rdata",sep=""))

                        HM.Groups.Reads.plot
	       }#f(length(FC2_table[,1])>=2)
             })#output$Groups.ComparePlot4
             


             if(length(FC2_table[,1])>100){
               
               shinyjs::show(id = "Groups.ComparePlot5_div")
               output$Groups.ComparePlot5 = renderPlot({ 
			FC2_table.melted = melt(data.frame(Gene=rownames(FC2_table[1:50,]),FC2_table[1:50,]),id.vars = c("Gene"))
                        colnames(FC2_table.melted) = c("Gene","Sample","Expression")
             
                        FC2_table.melted$Expression = log10(FC2_table.melted$Expression+1)

                        FC2_table.melted$Gene <- factor(FC2_table.melted$Gene, levels = rownames(FC2_table[1:50,])[pheatmap(FC2_table[1:50,])$tree_row[3]$order])

                       HM.Groups.Reads.50 = ggplot(FC2_table.melted,aes(x=Sample,y=Gene,fill=log10(Expression+1))) +
                           geom_tile() +
                           theme_classic()+
                           theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1,size = input$Groups.Reads.Heatmap_Font_Size))+
                           theme(axis.text.y = element_text(size = input$Groups.Reads.Heatmap_Font_Size))+
                           theme(axis.title.x = element_blank())+
                           scale_fill_gradient2(name="log10(Expression+1)", low = "dodgerblue4",mid="white", high ="firebrick2",midpoint=median(log10(FC2_table.melted$Expression+1)), limit=log10(c(min(FC2_table.melted$Expression),max(FC2_table.melted$Expression))+1))


                        PLOT_WIDTH = rep(2:10)
                        PLOT_WIDTH[1:input$Groups.Reads.Plot_Widths] = 1
                        HM.Groups.Reads.plot.50 = grid.arrange(HM.Groups.Reads.50,layout_matrix = rbind(PLOT_WIDTH))

                        Report.List.Reads <<- c(Report.List.Reads,list(HM.Groups.Reads.plot.50))
			save(list = c("HM.Groups.Reads.plot.50"), file=paste(Backup_Session_Folder,"/HM.Groups.Reads.plot.50.rdata",sep=""))

                        HM.Groups.Reads.plot.50
		})# output$Groups.ComparePlot5 = renderPlot({

               shinyjs::show(id = "Groups.ComparePlot6_div")
               output$Groups.ComparePlot6 = renderPlot({
                        FC2_table.melted = melt(data.frame(Gene=rownames(FC2_table[1:100,]),FC2_table[1:100,]),id.vars = c("Gene"))
                        colnames(FC2_table.melted) = c("Gene","Sample","Expression")
               
                        FC2_table.melted$Expression = log10(FC2_table.melted$Expression+1)

                        FC2_table.melted$Gene <- factor(FC2_table.melted$Gene, levels = rownames(FC2_table[1:100,])[pheatmap(FC2_table[1:100,])$tree_row[3]$order])

                       HM.Groups.Reads.100 = ggplot(FC2_table.melted,aes(x=Sample,y=Gene,fill=log10(Expression+1))) +
                           geom_tile() +
                           theme_classic()+
                           theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1,size = input$Groups.Reads.Heatmap_Font_Size))+
                           theme(axis.text.y = element_text(size = input$Groups.Reads.Heatmap_Font_Size))+
                           theme(axis.title.x = element_blank())+
                           scale_fill_gradient2(name="log10(Expression+1)", low = "dodgerblue4",mid="white", high ="firebrick2",midpoint=median(log10(FC2_table.melted$Expression+1)), limit=log10(c(min(FC2_table.melted$Expression),max(FC2_table.melted$Expression))+1))


                        PLOT_WIDTH = rep(2:10)
                        PLOT_WIDTH[1:input$Groups.Reads.Plot_Widths] = 1
                        HM.Groups.Reads.plot.100 = grid.arrange(HM.Groups.Reads.100,layout_matrix = rbind(PLOT_WIDTH))

			save(list = c("HM.Groups.Reads.plot.100"), file=paste(Backup_Session_Folder,"/HM.Groups.Reads.plot.100.rdata",sep=""))

                        Report.List.Reads <<- c(Report.List.Reads,list(HM.Groups.Reads.plot.100))
                        HM.Groups.Reads.plot.100
                })# output$Groups.ComparePlot6 = renderPlot({
#               output$Groups.ComparePlot7 = renderPlot(pheatmap(log2(FC2_table[1:100,] + 1), cluster_cols = FALSE, cluster_rows = F, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 100 genes ranked")))
#               output$Groups.ComparePlot8 = renderPlot(pheatmap(log2(FC2_table[1:100,] + 1), cluster_cols = FALSE, cluster_rows = T, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 100 genes clustered")))
               
             }#if(length(FC2_table[,1])>100)
             
             
          #########################################################
           ##### Human GENE ENSEMBL IDs to Human GENE symbols ######
           #########################################################
           FC_Genes <- as.character(FC_Genes)         
           FC_Genes_Symbols = as.character(FC_Genes)
           if(length(FC_Genes)>0)
           {
           	  if(any(FC_Genes %in% GENE_ID_Match$Gene.stable.ID))
           	  {
           	  	FC_Genes_Symbols[which(FC_Genes %in% GENE_ID_Match$Gene.stable.ID)] = as.character(GENE_ID_Match$Gene.name[match(FC_Genes[which(FC_Genes %in% GENE_ID_Match$Gene.stable.ID)],GENE_ID_Match$Gene.stable.ID)])
           	  }
           } 
           
           FC_Genes_UP <- as.character(FC_Genes_UP)
           FC_Genes_UP_Symbols = as.character(FC_Genes_UP)
           if(length(FC_Genes_UP)>0)
           {
           	  if(any(FC_Genes_UP %in% GENE_ID_Match$Gene.stable.ID))
           	  {
           	  	FC_Genes_UP_Symbols[which(FC_Genes_UP %in% GENE_ID_Match$Gene.stable.ID)] = as.character(GENE_ID_Match$Gene.name[match(FC_Genes_UP[which(FC_Genes_UP %in% GENE_ID_Match$Gene.stable.ID)],GENE_ID_Match$Gene.stable.ID)])
           	  }
           }
           
           FC_Genes_DOWN <- as.character(FC_Genes_DOWN)
           FC_Genes_DOWN_Symbols = as.character(FC_Genes_DOWN)
           if(length(FC_Genes_DOWN)>0)
           {
           	  if(any(FC_Genes_DOWN %in% GENE_ID_Match$Gene.stable.ID))
           	  {
           	  	FC_Genes_DOWN_Symbols[which(FC_Genes_DOWN %in% GENE_ID_Match$Gene.stable.ID)] = as.character(GENE_ID_Match$Gene.name[match(FC_Genes_DOWN[which(FC_Genes_DOWN %in% GENE_ID_Match$Gene.stable.ID)],GENE_ID_Match$Gene.stable.ID)])
           	  }
           }
           
           #########################################################
           ##### Mouse GENE ENSEMBL IDs to Mouse GENE Name ######
           #########################################################
           

           if(length(FC_Genes)>0)
           {
           	  if(any(FC_Genes %in% GENE_ID_Match$Mouse.gene.stable.ID))
           	  {
           	  	FC_Genes <- as.character(FC_Genes)          
           	    FC_Genes_Symbols = as.character(FC_Genes)
           	  	FC_Genes_Symbols[which(FC_Genes %in% GENE_ID_Match$Mouse.gene.stable.ID)] = as.character(GENE_ID_Match$Mouse.gene.name[match(FC_Genes[which(FC_Genes %in% GENE_ID_Match$Mouse.gene.stable.ID)],GENE_ID_Match$Mouse.gene.stable.ID)])
           	  }
           } 
           
           
           if(length(FC_Genes_UP)>0)
           {
           	  if(any(FC_Genes_UP %in% GENE_ID_Match$Mouse.gene.stable.ID))
           	  {
           	  	FC_Genes_UP <- as.character(FC_Genes_UP)
          		FC_Genes_UP_Symbols = as.character(FC_Genes_UP)
           	  	FC_Genes_UP_Symbols[which(FC_Genes_UP %in% GENE_ID_Match$Mouse.gene.stable.ID)] = as.character(GENE_ID_Match$Mouse.gene.name[match(FC_Genes_UP[which(FC_Genes_UP %in% GENE_ID_Match$Mouse.gene.stable.ID)],GENE_ID_Match$Mouse.gene.stable.ID)])
           	  }
           }
           
           
           if(length(FC_Genes_DOWN)>0)
           {
           	  if(any(FC_Genes_DOWN %in% GENE_ID_Match$Mouse.gene.stable.ID))
           	  {
           	  	FC_Genes_DOWN <- as.character(FC_Genes_DOWN)
           		FC_Genes_DOWN_Symbols = as.character(FC_Genes_DOWN)
           	  	FC_Genes_DOWN_Symbols[which(FC_Genes_DOWN %in% GENE_ID_Match$Mouse.gene.stable.ID)] = as.character(GENE_ID_Match$Mouse.gene.name[match(FC_Genes_DOWN[which(FC_Genes_DOWN %in% GENE_ID_Match$Mouse.gene.stable.ID)],GENE_ID_Match$Mouse.gene.stable.ID)])
           	  }
           }
            #########################################################
             #########################################################
              #########################################################
 
   
             
             updateTextAreaInput(session,inputId="Groups.GeneList_Result", label = paste("Differentially Expressed Genes",mainTitle), value = paste(FC_Genes_Symbols,collapse="\n"))
             updateTextAreaInput(session,inputId="Groups.GeneList_Result_UP", label = paste("Up Regulated Genes",mainTitle), value = paste(FC_Genes_UP_Symbols,collapse="\n"))
             updateTextAreaInput(session,inputId="Groups.GeneList_Result_DOWN", label = paste("Down Regulated Expressed Genes",mainTitle), value = paste(FC_Genes_DOWN_Symbols,collapse="\n"))
             
             
             output$FC_Download.Groups <- downloadHandler(
               filename = function() {
                 paste("DE_FC_Genes.txt", sep = "")
               },
               content = function(file) {
                 write.table(FC2_table.sorted, file,quote=F, row.names = F, sep="\t")
               })
             output$FC_Download_UP.Groups <- downloadHandler(
               filename = function() {
                 paste("DE_FC_Genes_Upregulated.txt", sep = "")
               },
               content = function(file) {
                 write.table(FC2_table.sorted[which(FC2_table.sorted$logFC > 0),], file,quote=F, row.names = F, sep="\t")
               })
             output$FC_Download_DOWN.Groups <- downloadHandler(
               filename = function() {
                 paste("DE_FC_Genes_DownRegulated.txt", sep = "")
               },
               content = function(file) {
                 write.table(FC2_table.sorted[which(FC2_table.sorted$logFC < 0),], file,quote=F, row.names = F,sep="\t")
               })
             
             
             output$FC_GeneList.Groups <- downloadHandler(
               filename = function() {
                 paste("DE_GenesList.txt", sep = "")
               },
               content = function(file) {
                 write.table(GeneList_Table, file,quote=F, row.names = F,sep="\t")
               })
             output$FC_GeneList_UP.Groups <- downloadHandler(
               filename = function() {
                 paste("DE_GeneList_Upregulated.txt", sep = "")
               },
               content = function(file) {
                 write.table(GeneList_Table_UP, file,quote=F, row.names = F,sep="\t")
               })
             output$FC_GeneList_DOWN.Groups <- downloadHandler(
               filename = function() {
                 paste("DE_GeneList_DownRegulated.txt", sep = "")
               },
               content = function(file) {
                 write.table(GeneList_Table_DOWN, file,quote=F, row.names = F,sep="\t")
               })
             
           }#if (numGenes > 0)
           
           updateSelectizeInput(session, inputId="Groups.Control", label = "Select Group", choices = Groups,selected = Sample1)
           updateSelectizeInput(session, inputId="Groups.Treatment", label = "Select Group", choices = Groups,selected = Sample2)
           
           
           
           #################################################################################################################
           
           ##################################
           ##################################
           ##################################
           ##################################
         }
         #})
         
       })#isolate
       
       #################################
       #################################
       #################################
     
     
     output$Groups.downloadData <- downloadHandler(
       filename = function() {
         paste("RPKM_data", ".csv", sep = "")
       },
       content = function(file) {
         write(input$Groups.Gene_Table, file)
       })
     
##################
     progress$inc(1, detail = "Comparison Complete")
     Sys.sleep(0.001)     
#################     
   })#-obs.Edgr.Compare.Conditions.button
   
   
################################################################
##############################################################
############################################################
############# ADVANCED ANALYSES ###########################
############# ADVANCED ANALYSES ##########################
############  ADVANCED ANALYSES  ########################
########################################################


##############################################################   
   ############################################################
   #############################################################
  
	observeEvent(input$Ontology_Select_Parent,ignoreInit = TRUE,{

  	         if(!exists("GO.info"))
   		 {
        	    Existing_Objects = ls()
       		    load("Data/ROGUE_GO_Workspace.RData")
     		    New_Objects = setdiff(ls(), Existing_Objects)
    		    Make_Global_Object_Command = paste(New_Objects,"<<-", New_Objects)

		    for(Command in Make_Global_Object_Command)
      		    {
 	               eval(expr = parse(text = Command))
     		    }#for(Command in Make_Global_Object_Command)

   		 }#if(!exists("GO.info"))


 		GO_ID_info = GO.info[which(GO.info[,2] == gsub("Biological Process - ","",input$Ontology_Select_Parent)),1]	
 		GO_NAME_List = GO.info[which(GO.info[,1] %in% unlist(strsplit(GO.EXT.hierarchy[grep(paste("^",GO_ID_info,sep=""),GO.EXT.hierarchy)]," "))),2]		
 		updateSelectInput(session = session,inputId = "Ontology_List",label = "Select GO (Select GO from drop-down list)",choices = GO_NAME_List, selected = NULL)	
 	})#observeEvent(input$Ontology_Select_Parent,ignoreInit = TRUE,{


	observeEvent(input$Ontology_Select_Parent_Combine,ignoreInit = TRUE,{

                        GO_ID_info_Combine = GO.info[which(GO.info[,2] == gsub("Biological Process - ","",input$Ontology_Select_Parent_Combine)),1]
                        GO_NAME_List_Combine = GO.info[which(GO.info[,1] %in% unlist(strsplit(GO.EXT.hierarchy[grep(paste("^",GO_ID_info_Combine,sep=""),GO.EXT.hierarchy)]," "))),2]
                        updateSelectInput(session = session,inputId = "Ontology_Combine",label = "Select GO (Select GO from drop-down list)",choices = GO_NAME_List_Combine, selected = NULL)
        })#observeEvent(input$Ontology_Select_Parent_Combine,ignoreInit = TRUE,{
 
	
  
  #####################################
  #####################################
  #####################################
#######################################
#########################################
###########################################

  ###########################  
  #####################################
  #####################################
  observeEvent(input$GO_Group_v_Sample,ignoreInit = TRUE,{
    
  
    CHECK_DV <- tryCatch(
    {
      CHECK_DV <- ncol(DATA.Values)
      CHECK_DV <- TRUE
    },
    error = function(cond){
      CHECK_DV <- FALSE
    },
    finally = {
      #pass
    })
  
    #"Groups","Sample"
    if(input$GO_Group_v_Sample == "Samples" & CHECK_DV)
    { 
      updateSelectizeInput(session, inputId = "Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
      updateSelectizeInput(session, inputId = "Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
    }#if(input$GO_Group_v_Sample == "Samples")
    if(input$GO_Group_v_Sample == "Groups" & exists("Groups"))
    {
      if(Group_Count>0)
      {
        updateSelectizeInput(session, inputId = "Control_GO_Test",label = "Select Control",choices = Groups, selected = NULL)
        updateSelectizeInput(session, inputId = "Subjects_GO_Test",label = "Select Subjects",choices = Groups, selected = NULL)
      }#if(Group_Count>0)
      if(Group_Count==0)
      {
        updateSelectizeInput(session, inputId = "Control_GO_Test",label = "Select Control",choices = NULL, selected = NULL)
        updateSelectizeInput(session, inputId = "Subjects_GO_Test",label = "Select Subjects",choices = NULL, selected = NULL)
      }#if(Group_Count==0)
      
    }#if(input$GO_Group_v_Sample == "Groups" & exists("Groups"))
    
  })#observeEvent(input$GO_Group_v_Sample,{
  	
  	  #####################################
  #####################################
  #####################################
#######################################
#########################################
###########################################

  ###########################  
  #####################################
  #####################################
  #####################################
  #input$GO_Analysis
  ####################################
#
  observeEvent(input$GO_Analysis,ignoreInit = TRUE,{  
    #updateSliderInput(session = session,inputId = "GO_Text_FC_limit",label = "Label absFC Threshold",min = 0,max = 0,value = 0)
    
    if(length(DATA.Values.5min) <= 1)
	{	
		shinyalert(title = "Data Error",text = "Please load data into the tool", type = "warning")
		return(NULL)
	}
    
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Determining Gene Ontology", value = 0)
    # Increment the progress bar, and update the detail text.
    ###############################################         
    progress$inc(0.1, detail = paste("Progress: ",10,"%",sep=""))
    Sys.sleep(0.001)
    ###############################################  
######
    #######
    ########


    if(!exists("GO.info"))
    {
           Existing_Objects = ls()
           load("Data/ROGUE_GO_Workspace.RData")
           New_Objects = setdiff(ls(), Existing_Objects)
           Make_Global_Object_Command = paste(New_Objects,"<<-", New_Objects)

           for(Command in Make_Global_Object_Command)
           {
                eval(expr = parse(text = Command))
           }#for(Command in Make_Global_Object_Command)

    }#if(!exists("GO.info"))


Fpkm.table.New.txt = c()

    if(exists("DATA.Values.5min"))
    { 
	if(!is.null(DATA.Values.5min))
	{    
 	   if(input$GO_Group_v_Sample == "Samples"){Fpkm.table.New.txt = DATA.Values.5min}
	}#if(!missing("DATA.Values.5min"))
    }#if(exists(DATA.Values.5min))


     if(exists("Groups.Medians.Matrix"))
     { 
       if(!is.null(Groups.Medians.Matrix))
      { 
	    if(input$GO_Group_v_Sample == "Groups"){Fpkm.table.New.txt = Groups.Medians.Matrix}
	 }#if(!is.null(Groups.Medians.Matrix))
     }#if(exists(Groups.Medians.Matrix))

    
    GO.exmp  = input$Ontology_List
    GO.comb  = input$Ontology_Combine
    
 
    
    Control.fpkm = input$Control_GO_Test
    Control.fpkm.inx = which(colnames(Fpkm.table.New.txt)==Control.fpkm)
    Test.fpkm = input$Subjects_GO_Test
    Test.fpkm.inx = which(colnames(Fpkm.table.New.txt) %in% Test.fpkm)
    GO_Source = substr(input$Ontology_Method_2Compare,1,3)
    GO_Gene_Lists = c()
    RPKM.threshold = input$GO_Min_RPKM
########
    #######
    #######
    

    
    if(length(GO.exmp)>0 & length(Test.fpkm)>0 & length(Control.fpkm)>0 & nchar(RPKM.threshold)>0)
    {
    
      
      GO.exmp.IDs = GO.info[match(GO.exmp,GO.info[,2]),1]
      GO.comb.IDs = GO.info[match(GO.comb,GO.info[,2]),1]
      
     
      if(length(GO.comb.IDs)>0)
      {
          
        Group_comb_List = c()
        for(i in 1:length(GO.comb.IDs))
        {
          Group.mems.comb = grep(GO.comb.IDs[i],substr(GO.EXT.hierarchy,1,10))
          Group.mems.comb = unique(c(as.character(GO.comb.IDs[i]),unlist(strsplit(GO.EXT.hierarchy[Group.mems.comb]," "))))
          Group_comb_List = c(Group_comb_List,list(Group.mems.comb))
        }
        
        Group_comb_List.intersect = unlist(Group_comb_List[[1]])
       
        if(length(GO.comb.IDs)>1)
        {
          for(i in 2:length(GO.comb.IDs))
          {
            Group_comb_List.intersect = intersect(Group_comb_List.intersect,unlist(Group_comb_List[[i]]))
          }
        }
       
        GO_Gene_Table.intersect = intersect(Group_comb_List.intersect,GO_Gene_Table[,2])
      }#if(length(GO.comb.IDs)>0)

      if(length(GO.comb.IDs)==0)
      {
        GO_Gene_Table.intersect = GO_Gene_Table[,2]
      }#if(length(GO.comb.IDs)==0)
      
     
     	
 
      GO_Gene_Table.sub = GO_Gene_Table[grep(paste(GO_Source,collapse="|"),GO_Gene_Table[,3]),]
      
      GO_Gene_Table.sub.comb = GO_Gene_Table.sub[which(GO_Gene_Table.sub[,2] %in% GO_Gene_Table.intersect),]
      
      
      GO_Gene_Lists = c()
      
      Largest_FC = 0
      Largest_FC_Union = 0
      Output_Gene_List = "Gene List"
      
      if(nrow(GO_Gene_Table.sub.comb)>2)
      for(i in 1:length(GO.exmp.IDs))
      {
        
        GO_expressed_genes.inx.union = c()
        Group.mems.exmp = grep(GO.exmp.IDs[i],substr(GO.EXT.hierarchy,1,10))
        Group.mems.exmp = unique(c(as.character(GO.exmp.IDs[i]),unlist(strsplit(GO.EXT.hierarchy[Group.mems.exmp]," "))))
        
        if(any(is.na(Group.mems.exmp)))
        {
          Group.mems.exmp = Group.mems.exmp[-which(is.na(Group.mems.exmp))]
        }
        
        GO_genes = intersect(unique(GO_Gene_Table.sub.comb[,1]),unique(GO_Gene_Table.sub[which(GO_Gene_Table.sub[,2] %in% Group.mems.exmp),1]))
        GO_genes.inx = which(rownames(Fpkm.table.New.txt) %in% GO_genes) 
        FC_LOG2_ALLsets = c()
        
      
        
          for( j in 1:length(Test.fpkm.inx))
          {
            GO_expressed_genes.inx = c()
            
            if(length(GO_genes.inx)>=2)
            {
              for(k in 1:nrow(Fpkm.table.New.txt[GO_genes.inx,]))
              {
                if(max(as.numeric(as.matrix(Fpkm.table.New.txt[GO_genes.inx[k],c(Test.fpkm.inx[j],Control.fpkm.inx)]))) >= RPKM.threshold)
                {
                  GO_expressed_genes.inx =  c(GO_expressed_genes.inx,GO_genes.inx[k])
                }#if(max(as.numeric(as.matrix(Fpkm.table.New.txt[GO_genes.inx[k],c(Test.fpkm.inx[j],Control.fpkm.inx)]))) >= RPKM.threshold)
              }#for(k in 1:nrow(Fpkm.table.New.txt[GO_genes.inx,]))
              FC_LOG2_ALLsets = c(FC_LOG2_ALLsets,log2((as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Test.fpkm.inx[j]])+1)/(as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Control.fpkm.inx]+1))))
              
              if(length(GO_expressed_genes.inx)>0)
              {
                GO_expressed_genes.inx.union = c(GO_expressed_genes.inx.union,GO_expressed_genes.inx)
              }#if(length(GO_expressed_genes.inx)>0)
              
            }#if(length(GO_genes.inx)>0)
            if(length(GO_genes.inx)==0)
            {
              FC_LOG2_ALLsets = c(FC_LOG2_ALLsets,0)
            }#if(length(GO_genes)==0)
          }#for( j in 1:length(Test.fpkm.inx))
          GO_Gene_Lists = c(GO_Gene_Lists,list(FC_LOG2_ALLsets))
     
      
     	  
     	  if(length(GO_expressed_genes.inx.union) >= 2)
     	  {
     	  	

          	GO_expressed_genes.inx.union = unique(GO_expressed_genes.inx.union)
          	GO_expressed_genes.value.union = rbind(Fpkm.table.New.txt[GO_expressed_genes.inx.union,c(Control.fpkm.inx,Test.fpkm.inx)])
          	Gene.value.set.temp = cbind(rownames(Fpkm.table.New.txt)[GO_expressed_genes.inx.union],GO_expressed_genes.value.union)
          	colnames(Gene.value.set.temp) = c("Gene_Name",colnames(Fpkm.table.New.txt)[c(Control.fpkm.inx,Test.fpkm.inx)])
  

          	if(ncol(GO_expressed_genes.value.union)>=3 & nrow(GO_expressed_genes.value.union)>=2)
          	{
          	  ncol(GO_expressed_genes.value.union)
          	  Largest_FC = apply(abs(log2((GO_expressed_genes.value.union[,-1]+1)/(GO_expressed_genes.value.union[,1]+1) )),MARGIN = 1,function(X) max(X))
          	}#if(ncol(GO_expressed_genes.value.union)>=3)
          	

          	if(ncol(GO_expressed_genes.value.union)<=2)
          	{
          	  ncol(GO_expressed_genes.value.union)
          	  Largest_FC = abs(log2((GO_expressed_genes.value.union[,-1]+1)/(GO_expressed_genes.value.union[,1]+1))) 
         	 }#if(ncol(GO_expressed_genes.value.union)<=2)
          
   
            if(!is.null(dim(Gene.value.set.temp)))   	
            {		
       	   		Gene.value.set.temp = Gene.value.set.temp[order(Largest_FC,decreasing = T),]
      		}#if(!is.null(dim(Gene.value.set.temp)))   	


          
	          Output_Gene_List = paste(Output_Gene_List,"\n\n",toupper(GO.exmp[i]),"\n",paste(colnames(Gene.value.set.temp),collapse="\t"),sep="")
         
 	         if(length(GO_expressed_genes.inx.union)>0 & !is.null(dim(Gene.value.set.temp)))
   	 	     {
              
         	   Gene_name_sect = max(nchar(as.matrix(Gene.value.set.temp[,1]))) + 5
         	   if(is.na(Gene_name_sect)) Gene_name_sect = 5
    		       for(n in 1:nrow(Gene.value.set.temp))
           	   {

        		      Output_Line = paste(rep(" ",Gene_name_sect + (length(2:ncol(Gene.value.set.temp))*7)),collapse="")
        	    		  substr(Output_Line,1,nchar(as.matrix(Gene.value.set.temp[n,1]))) = as.matrix(Gene.value.set.temp[n,1])
            		  for(b in 2:(ncol(Gene.value.set.temp)))
            		  {

            		    substr(Output_Line,Gene_name_sect+(b-2)*7+1,Gene_name_sect+(b-2)*7+6) = substr(as.matrix(Gene.value.set.temp[n,b]),1,6)
              	  }#for(b in 2:(ncol(Gene.value.set.temp)))
           	   Output_Gene_List = paste(Output_Gene_List,"\n",paste(c(substr(Output_Line,1,Gene_name_sect),substr(as.matrix(Gene.value.set.temp[n,2:ncol(Gene.value.set.temp)]),1,6)),collapse="\t"),sep="")
           	   #Output_Gene_List = paste(Output_Gene_List,"\n",Output_Line,sep="")
           	   #Output_Gene_List = paste(Output_Gene_List,"\n",paste(as.matrix(Gene.value.set.temp[n,]),collapse="\t"),sep="")
           	   }#for(n in 1:nrow(Gene.value.set.temp))
          	}#if(length(GO_expressed_genes.inx.union)>0)
                   
          	updateTextAreaInput(session,inputId = "GENE_GO_FC_RPKM",label = "GO RPKM Table",value=Output_Gene_List)
          	Largest_FC_Union = max(c(Largest_FC_Union,Largest_FC))
          	

          	
           }#if(length(GO_expressed_genes.inx.union) > 0)          
        	  }#for(i in 1:length(GO.exmp.IDs))
        	  

      updateSliderInput(session = session,inputId = "GO_Text_FC_limit",label = "Label absFC Threshold",min = 0,max = floor(Largest_FC_Union) ,value = 0,step = 0.1)
      

      ###################  
        shinyjs::show(id = "GO_FC_Expression_div")
        output$GO_FC_Expression <- renderPlot({
      ###################  
          
          Width_Adjust = input$GO.Graph_Width
          Font_Adjust = input$GO.Font_Size
                   
        par(mfrow=c(1,5-Width_Adjust))
        par(xpd=F)
        boxplot(GO_Gene_Lists,outpch = NA,ylab="Log2 FC",ylim=c(min(unlist(GO_Gene_Lists))-length(Test.fpkm.inx)-2,max(unlist(GO_Gene_Lists))),names=gsub(" ","\n",GO.exmp),las=2,cex.axis=0.6*Font_Adjust)
        #boxplot(GO_Gene_Lists,ylab="Log2 FC",ylim=c(min(unlist(GO_Gene_Lists))-length(Test.fpkm.inx)-2,max(unlist(GO_Gene_Lists))),names=gsub(" ","\n",GO.exmp),las=2,cex.axis=0.6)
               
        for(i in 1:length(GO.exmp.IDs))
        {
          Group.mems.exmp = grep(GO.exmp.IDs[i],substr(GO.EXT.hierarchy,1,10))
          Group.mems.exmp = unique(c(as.character(GO.exmp.IDs[i]),unlist(strsplit(GO.EXT.hierarchy[Group.mems.exmp]," "))))

          if(any(is.na(Group.mems.exmp)))
          {
            Group.mems.exmp = Group.mems.exmp[-which(is.na(Group.mems.exmp))]
          }#if(any(is.na(Group.mems.exmp)))
          
          GO_genes = intersect(unique(GO_Gene_Table.sub.comb[,1]),unique(GO_Gene_Table.sub[which(GO_Gene_Table.sub[,2] %in% Group.mems.exmp),1]))
          GO_genes.inx = which(rownames(Fpkm.table.New.txt) %in% GO_genes) 
          FC_LOG2_ALLsets = c()
          
         if(length(GO_genes.inx)>=2)
         {
          for( j in 1:length(Test.fpkm.inx))
          {          
            GO_expressed_genes.inx = c()
            for(k in 1:nrow(Fpkm.table.New.txt[GO_genes.inx,]))
            {     
              if(length(GO_genes.inx)>0)
              {
                if(max(as.numeric(unlist(Fpkm.table.New.txt[GO_genes.inx[k],c(Test.fpkm.inx[j],Control.fpkm.inx)]))) >= RPKM.threshold)
                {
                  GO_expressed_genes.inx =  c(GO_expressed_genes.inx,GO_genes.inx[k])
                }#if(max(as.numeric(unlist(Fpkm.table.New.txt[GO_genes.inx[k],c(Test.fpkm.inx[j],Control.fpkm.inx)]))) >= RPKM.threshold)
              }#if(length(GO_genes.inx)>0)
              
            }#for(k in 1:nrow(Fpkm.table.New.txt[GO_genes.inx,]))
            #FC_LOG2_ALLsets = c(FC_LOG2_ALLsets,log2((as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Test.fpkm.inx[j]])+0.1)/(as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Control.fpkm.inx]+0.1))))
            if(length(GO_genes.inx)>0)
            {
              FC_DATA = log2((as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Test.fpkm.inx[j]])+1)/(as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Control.fpkm.inx]+1)))
             
              if(input$GO_Overlay_v_Side == "Overlay")
              {
               stripchart(FC_DATA, 
                         vertical = TRUE, method = "jitter", 
                         pch = (15+j), col = j, bg = "bisque", 
                         add = TRUE,at = i) 
              }#if(input$GO_Overlay_v_Side == "Overlay")
              
              if(input$GO_Overlay_v_Side == "Beside")
              {
                slight = rep(c(0.02,0,-0.02),ceiling(length(FC_DATA)/3))
                slight = slight[1:length(FC_DATA)]
      
                points(x=rep((i-length(Test.fpkm.inx)*0.025+j*0.05-0.025),length(FC_DATA))-slight,y=FC_DATA,pch=(15+j),col=j,cex=0.95)
              }#if(input$GO_Overlay_v_Side == "Beside")
              
            }# if(length(GO_genes.inx)>0)
          }#for( j in 1:length(Test.fpkm.inx))
          #GO_Gene_Lists = c(GO_Gene_Lists,list(FC_LOG2_ALLsets))
         }#if(length(GO_genes.inx)>=1)
         
        }#for(i in 1:length(GO.exmp.IDs))
        abline(h=0, lty=2,lwd =1.5,col="red")
        abline(h=1, lty=2,lwd =0.5,col="blue")
        abline(h=-1, lty=2,lwd =0.5,col="blue")
        legend(0.5,min(unlist(GO_Gene_Lists))-1,legend=colnames(Fpkm.table.New.txt)[Test.fpkm.inx],pch = c((15+1):(15+length(Test.fpkm.inx))), col=c(1:length(Test.fpkm.inx)),cex = 0.63*Font_Adjust,box.col = "white")
        
        ######################
        ######################
        })# output$GO_FC_Expression
        
      ##############  
      shinyjs::show(id = "GO_FC_Expression_Sep_div")
      output$GO_FC_Expression_Sep <- renderPlot({
      ##############
        Width_Adjust = input$GO.Graph_Width
        Font_Adjust = input$GO.Font_Size
        
        GO_Gene_Lists = c()
        
        par(mfrow=c(1,length(Test.fpkm.inx)*(5-Width_Adjust)))
        for( j in 1:length(Test.fpkm.inx))
        {
          GO_Gene_Lists = c()
          for(i in 1:length(GO.exmp.IDs))
          {
            Group.mems.exmp = grep(GO.exmp.IDs[i],substr(GO.EXT.hierarchy,1,10))
            Group.mems.exmp = unique(c(as.character(GO.exmp.IDs[i]),unlist(strsplit(GO.EXT.hierarchy[Group.mems.exmp]," "))))
            if(any(is.na(Group.mems.exmp)))
            {
              Group.mems.exmp = Group.mems.exmp[-which(is.na(Group.mems.exmp))]
            }
              
            GO_genes = intersect(unique(GO_Gene_Table.sub.comb[,1]),unique(GO_Gene_Table.sub[which(GO_Gene_Table.sub[,2] %in% Group.mems.exmp),1]))
            GO_genes.inx = which(rownames(Fpkm.table.New.txt) %in% GO_genes) 
            FC_LOG2_ALLsets = c()
            
            
            
              GO_expressed_genes.inx = c()
              if(length(GO_genes.inx)>=2)
              {
                for(k in 1:nrow(Fpkm.table.New.txt[GO_genes.inx,]))
                {
                  if(max(as.numeric(as.matrix(Fpkm.table.New.txt[GO_genes.inx[k],c(Test.fpkm.inx[j],Control.fpkm.inx)]))) >= RPKM.threshold)
                  {
                    GO_expressed_genes.inx =  c(GO_expressed_genes.inx,GO_genes.inx[k])
                  }
                }#for(k in 1:nrow(Fpkm.table.New.txt[GO_genes.inx,]))
                FC_LOG2_ALLsets = c(FC_LOG2_ALLsets,log2((as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Test.fpkm.inx[j]])+1)/(as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Control.fpkm.inx]+1))))
              }#if(length(GO_genes.inx)>0)
              if(length(GO_genes.inx)==0){FC_LOG2_ALLsets = c(FC_LOG2_ALLsets,0)}
              
          
            GO_Gene_Lists = c(GO_Gene_Lists,list(FC_LOG2_ALLsets))
          }#for(i in 1:length(GO.exmp.IDs))
          
         
          
          par(xpd=F)
          min_GO_val = min(unlist(GO_Gene_Lists))
          max_GO_val = max(unlist(GO_Gene_Lists))
          boxplot(GO_Gene_Lists,outpch = NA,ylab="Log2 FC",ylim=c(min_GO_val-(max_GO_val-min_GO_val)*0.2,max_GO_val),names=gsub(" ","\n",GO.exmp),las=2,cex.axis=0.6*Font_Adjust)
          #boxplot(GO_Gene_Lists,ylab="Log2 FC",ylim=c(min(unlist(GO_Gene_Lists))-length(Test.fpkm.inx)-2,max(unlist(GO_Gene_Lists))),names=gsub(" ","\n",GO.exmp),las=2,cex.axis=0.6)
          
          
          for(i in 1:length(GO.exmp.IDs))
          {
            Group.mems.exmp = grep(GO.exmp.IDs[i],substr(GO.EXT.hierarchy,1,10))
            Group.mems.exmp = unique(c(as.character(GO.exmp.IDs[i]),unlist(strsplit(GO.EXT.hierarchy[Group.mems.exmp]," "))))
            if(any(is.na(Group.mems.exmp)))
            {
              Group.mems.exmp = Group.mems.exmp[-which(is.na(Group.mems.exmp))]
            }
            
            #GO_expressed_genes = intersect(unique(GO_Gene_Table.sub.comb[,1]),unique(GO_Gene_Table.sub[which(GO_Gene_Table.sub[,2] %in% Group.mems.exmp),1]))
            #GO_expressed_genes.inx = which(Fpkm.table.New.txt[,1] %in% GO_expressed_genes) 
            
            GO_genes = intersect(unique(GO_Gene_Table.sub.comb[,1]),unique(GO_Gene_Table.sub[which(GO_Gene_Table.sub[,2] %in% Group.mems.exmp),1]))
            GO_genes.inx = which(rownames(Fpkm.table.New.txt) %in% GO_genes) 
            FC_LOG2_ALLsets = c()
            
            
            
            #FC_LOG2_ALLsets = c()
            
            {
              
              GO_expressed_genes.inx = c()
              for(k in 1:nrow(Fpkm.table.New.txt[GO_genes.inx,]))
              {
                if(length(GO_genes.inx)>0)
                {
                  if(max(as.numeric(unlist(Fpkm.table.New.txt[GO_genes.inx[k],c(Test.fpkm.inx[j],Control.fpkm.inx)]))) >= RPKM.threshold)
                  {
                    GO_expressed_genes.inx =  c(GO_expressed_genes.inx,GO_genes.inx[k])
                  }
                }#if(length(GO_genes.inx)>0)
              }
              #FC_LOG2_ALLsets = c(FC_LOG2_ALLsets,log2((as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Test.fpkm.inx[j]])+0.1)/(as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Control.fpkm.inx]+0.1))))
              
              if(length(GO_genes.inx)>0)
              {
                FC_DATA = log2((as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Test.fpkm.inx[j]])+1)/(as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Control.fpkm.inx]+1)))

                slight = rep(c(0.05,-0.125,-0.05,0.125),ceiling(length(FC_DATA)/4))
                slight.text = rep(c(0.2,-0.2,0.275,-0.275),ceiling(length(FC_DATA)/4))
                slight.text = slight.text[1:length(FC_DATA)]
                slight = slight[1:length(FC_DATA)]
                
                slight[order(FC_DATA,decreasing = T)] = slight
                slight.text[order(FC_DATA,decreasing = T)] = slight.text 
                points(x=rep(i,length(FC_DATA))-slight,y=FC_DATA,pch=(15+j),col=j)

                if(input$Label_GO_Gene == TRUE)
                {
                  FC.thrsh.inx = which(abs(FC_DATA)>=input$GO_Text_FC_limit)
                  if(length(FC.thrsh.inx)>0)
                  {
                    text(x=rep(i,length(FC.thrsh.inx))-(slight.text[FC.thrsh.inx]),y=FC_DATA[FC.thrsh.inx],labels = rownames(Fpkm.table.New.txt)[GO_expressed_genes.inx][FC.thrsh.inx],cex=0.8*Font_Adjust,col=j)
                  }#if(length(FC.thrsh.inx)>0)
                }#if(input$Label_GO_Gene == TRUE)
              }#if(length(GO_genes.inx)>0)
            }
            #GO_Gene_Lists = c(GO_Gene_Lists,list(FC_LOG2_ALLsets))
          }#for(i in 1:length(GO.exmp.IDs))
          abline(h=0, lty=2,lwd =1.5,col="red")
          abline(h=1, lty=2,lwd =0.5,col="blue")
          abline(h=-1, lty=2,lwd =0.5,col="blue")
          legend(0.5,min_GO_val-(max_GO_val-min_GO_val)*0.1,legend=colnames(Fpkm.table.New.txt)[Test.fpkm.inx[j]],pch = (15+j), col=j,cex = 0.63*Font_Adjust,box.col = "white")
          
          ######################
          ######################
          
        }#for( j in 1:length(Test.fpkm.inx))
        par(mfrow=c(1,1))
      })#output$GO_FC_Expression_Sep
    

        
        
    ########################################
        #########################################
           
          Dist.Legend_Labels = c()
          Group_GO_Values = c()
          Heatmap_gene.inx_List = c()
          combined.fpkm.inx = c(Control.fpkm.inx,Test.fpkm.inx)
          
          
          for(i in 1:length(GO.exmp.IDs))
          {
            Group.mems.exmp = grep(GO.exmp.IDs[i],substr(GO.EXT.hierarchy,1,10))
            Group.mems.exmp = unique(c(as.character(GO.exmp.IDs[i]),unlist(strsplit(GO.EXT.hierarchy[Group.mems.exmp]," "))))
    
            if(any(is.na(Group.mems.exmp)))
            {
              Group.mems.exmp = Group.mems.exmp[-which(is.na(Group.mems.exmp))]
            }
    
            GO_genes = intersect(unique(GO_Gene_Table.sub.comb[,1]),unique(GO_Gene_Table.sub[which(GO_Gene_Table.sub[,2] %in% Group.mems.exmp),1]))
            GO_genes.inx = which(rownames(Fpkm.table.New.txt) %in% GO_genes) 
            FC_LOG2_ALLsets = c()
         
           ALL_GO_expressed_genes.inx = c()
           if(length(GO_genes.inx)>0)
           {
            #ALL_GO_expressed_genes.inx = c()
            for( j in 1:length(combined.fpkm.inx))
            {
              
              GO_expressed_genes.inx = c()
              for(k in 1:nrow(rbind(Fpkm.table.New.txt[GO_genes.inx,])))
              {
                
                if(length(GO_genes.inx)>0)
                {
                  if(max(as.numeric(unlist(Fpkm.table.New.txt[GO_genes.inx[k],combined.fpkm.inx]))) >= RPKM.threshold)
                  {
                    GO_expressed_genes.inx =  c(GO_expressed_genes.inx,GO_genes.inx[k])
                  }
                }#if(length(GO_genes.inx)>0)
                
              }#for(k in 1:nrow(Fpkm.table.New.txt[GO_genes.inx,]))
              #FC_LOG2_ALLsets = c(FC_LOG2_ALLsets,log2((as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Test.fpkm.inx[j]])+0.1)/(as.numeric(Fpkm.table.New.txt[GO_expressed_genes.inx,Control.fpkm.inx]+0.1))))
              if(length(GO_genes.inx)>0)
              {
                
                Group_GO_Values = c(Group_GO_Values,list(Fpkm.table.New.txt[GO_expressed_genes.inx,combined.fpkm.inx[j]]))
                Dist.Legend_Labels = c(Dist.Legend_Labels,paste(GO.exmp[i],colnames(Fpkm.table.New.txt)[combined.fpkm.inx[j]],sep="-"))
                
              }# if(length(GO_genes.inx)>0)
              ALL_GO_expressed_genes.inx = unique(c(ALL_GO_expressed_genes.inx,GO_expressed_genes.inx))
            }#for( j in 1:length(combined.fpkm.inx))
           
            #Heatmap_gene.inx_List = c(Heatmap_gene.inx_List,list(ALL_GO_expressed_genes.inx))
           }#if(length(GO_genes.inx)>0)
           Heatmap_gene.inx_List = c(Heatmap_gene.inx_List,list(ALL_GO_expressed_genes.inx))
          }#for(i in 1:length(GO.exmp.IDs))
          
         
         if(length(Group_GO_Values)>0)
         {
          Distributions.max.val = max(unlist(Group_GO_Values))
          Distributions.min.val = min(unlist(Group_GO_Values))
          
          Distributions.max.density=0
          Distributions.max.freq = 0
          for(h in 1:length(Group_GO_Values))
          {
          	if(length(unlist(Group_GO_Values[h])) > 2)
          	{
            		Distributions.max.density = max(Distributions.max.density,density(unlist(Group_GO_Values[h]))$y)
            		Distributions.max.freq = max(Distributions.max.freq,hist(unlist(Group_GO_Values[h]),plot=F)$count)
            }#if(length(unlist(Group_GO_Values[h])) > 2)
          }
         
        
          
          RGB.order = (permutations(ceiling(length(Dist.Legend_Labels)^(1/3)),3,repeats.allowed = T))/max(permutations(ceiling(length(Dist.Legend_Labels)^(1/3)),3,repeats.allowed = T))
          #RGB.order = RGB.order[c(-1,-27),]
          peak.counts = hist(apply(RGB.order,MARGIN = 1,function(x) sum(x)),plot=F)$counts
          peak.mids = hist(apply(RGB.order,MARGIN = 1,function(x) sum(x)),plot=F)$mids
          peak.sums = apply(RGB.order,MARGIN = 1,function(x) sum(x))
          
          peak.mids.sorted = peak.mids[order(peak.counts,decreasing = T)]
          peak.mids.sorted.exp = as.numeric(rbind(peak.mids.sorted-0.25,peak.mids.sorted,peak.mids.sorted+0.25))
          peak.mids.sorted.exp = unique(peak.mids.sorted.exp)
          order(match(peak.sums,peak.mids.sorted.exp),decreasing=F)
          RGB = RGB.order[order(match(peak.sums,peak.mids.sorted.exp),decreasing=F),]
          
          x.25pct.val = (Distributions.max.val - Distributions.min.val)*0.25
          x.min.rpkm.val = Distributions.min.val - x.25pct.val
          x.max.rpkm.val = Distributions.max.val + x.25pct.val
          updateSliderInput(session = session,inputId = "GO_Dist_axis_limit",label = "Expression Distribution axis range",min = floor(max(c(0,x.min.rpkm.val))),max = ceiling(x.max.rpkm.val) ,value = c(Distributions.min.val,Distributions.max.val),step = 5)
          


          ###############################################  
          ##########
          shinyjs::show(id = "GO_Distribution_div")
          output$GO_Distribution <- renderPlot({
            ##############
            ########
            Width_Adjust = input$GO.Graph_Width
            Font_Adjust = input$GO.Font_Size
            
            par(mfrow=c(1,6-Width_Adjust))
            par(xpd=F)
            
          
          XLIM_min = min(input$GO_Dist_axis_limit)
          XLIM_max = max(input$GO_Dist_axis_limit)
          
          if(XLIM_min == Inf | XLIM_min == -Inf | XLIM_max == Inf | XLIM_max == -Inf )
          {
          	XLIM_min = 0
          	XLIM_max = 1
          }
          
          
          plot(0,xlim=c(XLIM_min, XLIM_max),ylim = c(0,Distributions.max.freq),cex=0,xlab = "Expression", ylab="Frequency",cex.lab=0.8*Font_Adjust,cex.axis=0.8*Font_Adjust)
          for(h in 1:length(Group_GO_Values))
          {
          	if(length(unlist(Group_GO_Values[h])) >= 2)
          	{         	
            	polygon(density(unlist(Group_GO_Values[h]),bw=0.1)$x,density(unlist(Group_GO_Values[h]))$y/max(density(unlist(Group_GO_Values[h]))$y)*max(hist(unlist(Group_GO_Values[h]),plot=F)$counts),col=rgb(RGB[h,1],RGB[h,2],RGB[h,3],0.5),border=rgb(RGB[h,1],RGB[h,2],RGB[h,3],1))
            }#if(length(unlist(Group_GO_Values[h])) >= 2)
          }# for(h in 1:length(Group_GO_Values))
          
          plot(0,xlim= c(XLIM_min, XLIM_max),ylim = c(0,Distributions.max.density),cex=0,xlab = "Expression", ylab="Density",cex.lab=0.8*Font_Adjust,cex.axis=0.8*Font_Adjust)
          
       
          
          for(h in 1:length(Group_GO_Values))
          {
          	if(length(unlist(Group_GO_Values[h])) >= 2)
          	{     
           		 polygon(density(unlist(Group_GO_Values[h]),bw=0.1)$x,density(unlist(Group_GO_Values[h]))$y,col=rgb(RGB[h,1],RGB[h,2],RGB[h,3],0.5),border=rgb(RGB[h,1],RGB[h,2],RGB[h,3],1))
           	}#if(length(unlist(Group_GO_Values[h])) >= 2)
            #polygon(density(unlist(Group_GO_Values[h]),bw=0.1)$x,density(unlist(Group_GO_Values[h]))$y/max(density(unlist(Group_GO_Values[h]))$y)*max(hist(unlist(Group_GO_Values[h]),plot=F)$count),col=rgb(1,0,0,0.5),border=rgb(1,0,0,1))
          }  
          
          X_coord = max(input$GO_Dist_axis_limit)-(max(input$GO_Dist_axis_limit)-min(input$GO_Dist_axis_limit))*0.5
          Y_coord = Distributions.max.density*0.9
          legend(X_coord,Y_coord,Dist.Legend_Labels,text.col = rgb(RGB[1:length(Dist.Legend_Labels),1],RGB[1:length(Dist.Legend_Labels),2],RGB[1:length(Dist.Legend_Labels),3],1),cex=(0.5+(Font_Adjust-1)*0.25),box.lty = 0, box.lwd = 0)
          
        })
        ##########
        ############
          
          ##################################
            #################################
          shinyjs::show(id = "GO_Heatmap_rowScale_div")
          output$GO_Heatmap_rowScale <- renderPlotly({
            ############################
            ########################
            
            par(mfrow=c(1,1))
#            pheatmap_List = c()
#            for(i in 1:length(GO.exmp.IDs))
#            {
#              GO_Pheatmap = pheatmap(Fpkm.table.New.txt[Heatmap_gene.inx_List[[i]],combined.fpkm.inx],scale="row",fontsize_row = 6*Font_Adjust,main = GO.exmp[i])
#	      
#              pheatmap_List = c(pheatmap_List,list(GO_Pheatmap[[4]]))	
#            }
#            do.call(grid.arrange,c(pheatmap_List,ncol=3,nrow=ceiling(length(pheatmap_List)/3)))
#          })
          ###########

	  Heatmaply_TEXT = c()
	  for(i in 1:length(GO.exmp.IDs))
	  {
		  	
	  	if(length(Heatmap_gene.inx_List[[i]])>=3)
	  	{
		#Heatmaply_TEXT[i] = paste("heatmaply(Fpkm.table.New.txt[Heatmap_gene.inx_List[[",i,"]],combined.fpkm.inx],scale=\"row\",xlab = GO.exmp[",i,"],colors = colorRampPalette(colors = c(\"blue\",\"white\",\"red\")))",sep="")
		
					
				ROW.INDEX = as.numeric(unlist(Heatmap_gene.inx_List[[i]]))
							
				Heatmaply_TEXT = c(Heatmaply_TEXT,paste("heatmaply(Fpkm.table.New.txt[", paste("c(",paste(ROW.INDEX,collapse = ","),")",sep=""),",combined.fpkm.inx],scale=\"row\",xlab = GO.exmp[",i,"],colors = colorRampPalette(colors = c(\"blue\",\"white\",\"red\")))",sep=""))
				
		}#if(length(Heatmap_gene.inx_List[[i]])>=3)
	  }#	for(i in 1:length(GO.exmp.IDs)) 

	    
	  if(length(Heatmaply_TEXT) > 1 & length(unique(combined.fpkm.inx)) >= 2 )
	  {
	  	Final_PLOT_Text = paste("subplot(",paste(Heatmaply_TEXT,collapse = ","),",nrows = ceiling(length(Heatmaply_TEXT)/3), titleX = T, shareY = F, shareX = F)")
	  	#eval(parse(text=Final_PLOT_Text))
	  }#if(length(Heatmaply_TEXT) > 1)
	  if(length(Heatmaply_TEXT) == 1 & length(unique(combined.fpkm.inx)) >= 2 )
	  {
	    Final_PLOT_Text = Heatmaply_TEXT 
	    #eval(parse(text=Final_PLOT_Text))

	  }#if(length(Heatmaply_TEXT) == 1)
	  if(length(Heatmaply_TEXT) == 0 | length(unique(combined.fpkm.inx)) <= 1 )
	  {
	  	Final_PLOT_Text  = "heatmaply(matrix(c(0,0,0,0),nrow = 2))"
	  }#if(length(Heatmaply_TEXT) == 0)

	  GO_Heatmap_Final_PLOT_Text_rowScale = Final_PLOT_Text

	  save(list = c("GO_Heatmap_Final_PLOT_Text_rowScale"), file=paste(Backup_Session_Folder,"/GO_Heatmap_FP_rowScale.rdata",sep=""))  


	  eval(parse(text= GO_Heatmap_Final_PLOT_Text_rowScale ))


        })#output$GO_Heatmap_rowScale <- renderPlotly({
        	
        	
   #################################
          shinyjs::show(id = "GO_Heatmap_div")
          output$GO_Heatmap <- renderPlotly({
            ############################
            ########################
 #           Font_Adjust = input$GO.Font_Size
 #           par(mfrow=c(1,1))
 #           pheatmap_List = c()
 #           for(i in 1:length(GO.exmp.IDs))
 #           {
 #           		if(length(Heatmap_gene.inx_List[[i]])>=3)
 #           		{
 #             		GO_Pheatmap = pheatmap(Fpkm.table.New.txt[Heatmap_gene.inx_List[[i]],combined.fpkm.inx],scale="none",fontsize_row = 6*Font_Adjust,show_rownames = T,show_colnames = T,main = #paste(GO.exmp[i],"(",GO.exmp.IDs[i],")",sep="") )
 #             		pheatmap_List = c(pheatmap_List,list(GO_Pheatmap[[4]]))
 #             	}#if(length(Heatmap_gene.inx_List[[i]])>=3)
 #           }#for(i in 1:length(GO.exmp.IDs))
#	    		Report.List.Reads <<- c(Report.List.Reads,pheatmap_List)  
 #           do.call(grid.arrange,c(pheatmap_List,ncol=3, nrow=ceiling(length(pheatmap_List)/3)))
 
 
             par(mfrow=c(1,1))

	  Heatmaply_TEXT = c()
	  for(i in 1:length(GO.exmp.IDs))
	  {
	  	if(length(Heatmap_gene.inx_List[[i]])>=3)
	  	{
				ROW.INDEX = as.numeric(unlist(Heatmap_gene.inx_List[[i]]))
						
				Heatmaply_TEXT = c(Heatmaply_TEXT,paste("heatmaply(Fpkm.table.New.txt[", paste("c(",paste(ROW.INDEX,collapse = ","),")",sep=""),",combined.fpkm.inx],xlab = GO.exmp[",i,"],colors = colorRampPalette(colors = c(\"blue\",\"white\",\"red\")))",sep=""))
				
		}#if(length(Heatmap_gene.inx_List[[i]])>=3)
	  }#	for(i in 1:length(GO.exmp.IDs)) 

	    
	  if(length(Heatmaply_TEXT) > 1 & length(unique(combined.fpkm.inx)) >= 2 )
	  {
	  	Final_PLOT_Text = paste("subplot(",paste(Heatmaply_TEXT,collapse = ","),",nrows = ceiling(length(Heatmaply_TEXT)/3), titleX = T, shareY = F, shareX = F)")
	  	#eval(parse(text=Final_PLOT_Text))
	  }#if(length(Heatmaply_TEXT) > 1)
	  if(length(Heatmaply_TEXT) == 1 & length(unique(combined.fpkm.inx)) >= 2 )
	  {
	    Final_PLOT_Text = Heatmaply_TEXT 
	    #eval(parse(text=Final_PLOT_Text))

	  }#if(length(Heatmaply_TEXT) == 1)

 	  if(length(Heatmaply_TEXT) == 0 | length(unique(combined.fpkm.inx)) <= 1 )     
	  {
	  	Final_PLOT_Text = "heatmaply(matrix(c(0,0,0,0),nrow = 2))"  	
	  }#if(length(Heatmaply_TEXT) == 0)


	  GO_Heatmap_Final_PLOT_Text = Final_PLOT_Text

          save(list = c("GO_Heatmap_Final_PLOT_Text"), file=paste(Backup_Session_Folder,"/GO_Heatmap_FP.rdata",sep=""))


          eval(parse(text= GO_Heatmap_Final_PLOT_Text))

	
 	 #eval(parse(text=Final_PLOT_Text))
 
 
          })#output$GO_Heatmap <- renderPlot({
          ###########

       
          
          

        ##########################
     }#if(length(Group_GO_Values)>0)   
    }#if(length(GO.exmp)>0 & length(Test.fpkm)>0 & length(Control.fpkm)>0 & nchar(RPKM.threshold)>0))
      
    progress$inc(1, detail = paste("Progress: ",99,"%",sep=""))
    Sys.sleep(0.001)
    ###############################################  
  }) #observeEvent(input$GO_Analysis,{
  #####################################
    #####################################
  #####################################
  #####################################
#######################################
#########################################
###########################################

  ###########################  
  #####################################
  #####################################
  #####################################
  #input$GO_GeneList_Button
  #####################################
  #####################################  
  observeEvent(input$GO_GeneList_Button,ignoreInit = TRUE,{
    ########################################    
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Determining Gene Ontology", value = 0)
    # Increment the progress bar, and update the detail text.
    ###############################################         
    progress$inc(0.1, detail = paste("Progress: ",10,"%",sep=""))
    Sys.sleep(0.001)
    ###############################################  
   
    if(!exists("GO.info"))
    {
           Existing_Objects = ls()
           load("Data/ROGUE_GO_Workspace.RData")
           New_Objects = setdiff(ls(), Existing_Objects)
           Make_Global_Object_Command = paste(New_Objects,"<<-", New_Objects)

           for(Command in Make_Global_Object_Command)
           {
                eval(expr = parse(text = Command))
           }#for(Command in Make_Global_Object_Command)

    }#if(!exists("GO.info"))
    
    
    GO.info.count <<- GO.info
    
    Chosen.organism = c()
    if(length(input$Organism_Checkbox)==0)
    {
      Chosen.organism = c(9606,10090)
    }
    if(length(input$Organism_Checkbox)>0)
    {
      Chosen.organism = c()
      if(any(as.character(input$Organism_Checkbox)=="Human")) {Chosen.organism = c(Chosen.organism,9606)}
      if(any(as.character(input$Organism_Checkbox)=="Mouse")) {Chosen.organism = c(Chosen.organism,10090)}
    }
    
    
    Chosen.curation <- substr(input$Ontology_Method,1,3)
    Chosen.curation = paste("\t",Chosen.curation,"\t",sep="")
    Human_GO_select = Human_GO[grep(paste(Chosen.curation,collapse="|"),Human_GO)]
    Human_GO_select = Human_GO_select[grep(paste(Chosen.organism,collapse="|"),Human_GO_select)]
    
    
    isolate(Test.Gene.List <- input$GO_GeneList)
    if (nchar(Test.Gene.List) > 0)
    {
      
      Test.Gene.List = gsub("\"","",gsub(","," ",gsub("\t"," ",gsub("\n", " ", Test.Gene.List))))
      Test.Gene.List = unlist(strsplit(Test.Gene.List,split = " "))
      Test.Gene.List = unique(Test.Gene.List)
      
      ###############################################         
      progress$inc(0.2, detail = paste("Progress: ",20,"%",sep=""))
      Sys.sleep(0.001)
      ###############################################  
     
      {
        #Target.GO.Lines = Human_GO_select[grep(paste(Test.Gene.List,collapse = "|"),Human_GO_select)]
	Target.GO.Lines = Human_GO_select[grep(paste("\t",Test.Gene.List,"\t",sep="",collapse = "|"),Human_GO_select)]
        Target.GO.table = c()
	
	Target.GO.table = t(sapply(Target.GO.Lines, FUN=function(x) {
        #for(j in 1:length(Target.GO.Lines)) {

          #temp.line = unlist(strsplit(Target.GO.Lines[j],split="\t"))
          #Target.GO.table = rbind(Target.GO.table,temp.line)
	  temp.line = unlist(strsplit(x,split="\t"))
          return(temp.line)
        } #for(j in 1:length(Target.GO.Lines))
       ))
        


      ###############################################     
      progress$inc(0.25, detail = paste("Progress: ",25,"%",sep=""))
      Sys.sleep(0.001)
      ###############################################  



        if(ncol(Target.GO.table)>1)
        {
          if(length(Target.GO.table[which(Target.GO.table[,3] %in% Test.Gene.List),3])>0)
          {
            #Target.GO.IDs = unique(unlist(Target.GO.table[which(Target.GO.table[,3]==Test.Gene.List[i]),5]))
            #for(k in 1:length(unique(Target.GO.table[which(Target.GO.table[,3] %in% Test.Gene.List),3])))
            {
              K = length(unique(Target.GO.table[which(Target.GO.table[,3] %in% Test.Gene.List),3]))
	      GO.info.count <<- data.frame(GO.info.count,t(rep(0,K)))
              #GO.info.count <<- data.frame(cbind(GO.info.count,0))
            }
            colnames(GO.info.count)[3:ncol(GO.info.count)] <<- unique(Target.GO.table[which(Target.GO.table[,3] %in% Test.Gene.List),3])
          }#if(length(Target.GO.table[which(Target.GO.table[,3] %in% Test.Gene.List),3])>0)
        } #if(ncol(Target.GO.table)>1)
      } #for(i in 1:length(Test.Gene.List))
      
      ##########
      ##########STEP
      ##########
      
      if(ncol(GO.info.count)>2)
      {
        for(i in 3:ncol(GO.info.count))
        {
          Target.GO.ID.List = unique(subset(Target.GO.table,Target.GO.table[,3]==colnames(GO.info.count)[i])[,5])
          GO.info.count[which(as.character(GO.info.count$GO.IDs) %in% Target.GO.ID.List),i] <<- 1
        } #for(i in 3:ncol(GO.info.count))
      } #if(ncol(GO.info.count)>2)
      
      ##########
      ##########STEP
      ##########
      ###############################################         
      progress$inc(0.3, detail = paste("Progress: ",30,"%",sep=""))
      Sys.sleep(0.001)
      ###############################################  
      
      
      #GO.count.table = data.frame(GO_Top_Level,0)



       isolate(GO_Selection <- as.character(input$Select_Initial_GO))
 
 
       GO_Prev_Step <<- c(GO_Prev_Step,GO_Selection)
       #Select_GO_Button
 
 
       Selected.GO.sub = GO.info$GO.IDs[match(GO_Selection,GO.info$GO.Names)]
       Selected.GO.sub.inx = which(substr(GO.hierarchy,1,10) == Selected.GO.sub)

 
       if(length(Selected.GO.sub.inx) > 0)
       {
 
         GO.sub.list = unlist(strsplit(GO.hierarchy[Selected.GO.sub.inx],split=" "))
 
         GO_Main = GO.sub.list[1]
         Sub_GO_List = GO.sub.list[-1]
 
         Sub_GO_List.names = as.character(GO.info$GO.Names[match(Sub_GO_List,GO.info$GO.IDs)])
 
       }#if(length(Selected.GO.sub.inx) > 0)
 
       if(length(Selected.GO.sub.inx) == 0)
       {
         Sub_GO_List.names = GO_Selection
       }#if(length(Selected.GO.sub.inx) == 0)



      GO.count.table = data.frame(Sub_GO_List.names,0)



      GO.info.count.check = c()
      #for(i in 1:length(GO_Top_Level))
      for(i in 1:length(Sub_GO_List.names))
      {
        
        #length(as.character(GO.info.count$GO.IDs[which(GO.info.count$GO.Names == GO_Top_Level[i])]))
        
        #Selected.GO = GO.info$GO.IDs[match(GO_Top_Level[i],GO.info$GO.Names)]
        Selected.GO = GO.info$GO.IDs[match(Sub_GO_List.names[i],GO.info$GO.Names)]
        Selected.GO.inx = which(substr(GO.EXT.hierarchy,1,10) == Selected.GO)
        #GO.sub.list = unlist(strsplit(GO.EXT.hierarchy[Selected.GO.inx],split=" "))
        
        if(length(Selected.GO.inx) == 0)
        {
          GO.sub.list = Selected.GO
        }
        if(length(Selected.GO.inx) > 0)
        {
          GO.sub.list = unlist(strsplit(GO.EXT.hierarchy[Selected.GO.inx],split=" "))
        } 
        
        GO.info.count.Subset = GO.info.count[which(GO.info.count$GO.IDs %in% GO.sub.list),]
        GO.info.count.check = rbind(GO.info.count.check,GO.info.count.Subset)
        
        if(ncol(GO.info.count.Subset)==3){ Total_genes = max(GO.info.count.Subset[,3]) }
        if(ncol(GO.info.count.Subset)>3){ Total_genes = sum(apply(GO.info.count.Subset[,-1:-2], 2, function(x) max(x, na.rm = TRUE))) }
        if(ncol(GO.info.count.Subset)<3){ Total_genes = 0}
        GO.count.table[i,2] = as.numeric(Total_genes)
        
      }#for(i in 1:length(GO_Top_Level))
      
      ###############################################         
      progress$inc(0.6, detail = paste("Progress: ",60,"% (moving forward gracefully)",sep=""))
      Sys.sleep(0.001)
      ###############################################  
      
      GO.info.count.check = GO.info.count.check[,c(1,2,(which(!colSums(GO.info.count.check[c(-1:-2)]) == 0)+2))]
      if(ncol(GO.info.count.check)==3){ Represented.gene.list = colnames(GO.info.count.check)[3] }
      if(ncol(GO.info.count.check)>3)
      {
        names.indx = which(apply(GO.info.count.check[,-1:-2], 2, function(x) max(x, na.rm = TRUE)) > 0)
        Represented.gene.list = colnames(GO.info.count.check)[(2+names.indx)] 
      }
      
      GO.count.table = GO.count.table[GO.count.table[,2]!=0,]
      
      if(nrow(GO.count.table) > 0)
      {
        
        shinyjs::show(id = "Ontology_Barplot_div")
        output$Ontology_Barplot <- renderPlot({
            bar_labels = as.character(GO.count.table[,1])
            bar_labels = gsub("_"," ",bar_labels)
            bar_labels = gsub(" ","\n",bar_labels)
            bar_labels = gsub("to\n","to ",bar_labels)
            bar_labels = gsub("and\n","and ",bar_labels)
            bar_labels = gsub("of\n","of ",bar_labels)
            bar_labels = gsub("cell\n","cell ",bar_labels)
            bar_labels = gsub("or\n","or ",bar_labels)
            bar_labels = gsub("in\n","in ",bar_labels)
            bar_labels = gsub("DNA\n","DNA ",bar_labels)
            bar_labels = gsub("RNA\n","RNA ",bar_labels)
            bar_labels = gsub("\nI"," I",bar_labels)
            bar_labels = gsub("\ntag"," tag",bar_labels)
            bar_labels = gsub("\nacid"," acid",bar_labels)
            bar_labels = gsub("mast\n","mast ",bar_labels)
            bar_labels = gsub("T\n","T ",bar_labels)
            bar_labels = gsub("\nion"," ion",bar_labels)
            bar_labels = gsub("\nion"," ion",bar_labels)
     
            barplot.order = order(GO.count.table[,2],decreasing=T) 

            barplot(GO.count.table[barplot.order,2],names=bar_labels[barplot.order],las=2,cex.names=0.5,col=c(1:nrow(GO.count.table)))
          })#output$Ontology_Barplot
     
          barplot.order = order(GO.count.table[,2],decreasing=T)
          updateRadioButtons(session,inputId ="Select_GO",choices = GO.count.table[barplot.order,1] )


	GENE.Ont.table.subset = GO_Gene_Table[which(GO_Gene_Table[,1] %in% Represented.gene.list & grepl(paste(Chosen.organism,collapse="|"),as.character(GO_Gene_Table[,5]))),]
	GO_List.subset = as.character(unique(GENE.Ont.table.subset[,2]))
	GO_PVAL.temp=c()

	GO_Organism.Select = grepl(paste(Chosen.organism,collapse="|"),as.character(GO_Gene_Table[,5]))
	TOTAL_GO_GENES = length(unique(GO_Gene_Table[which(GO_Organism.Select),1]))

        GO_PVAL.temp = sapply(GO_List.subset, FUN=function(X)
#	for(i in 1:length(GO_List.subset))
	{
#	  ONT_Hits = length(unique(GENE.Ont.table.subset[which(as.character(GENE.Ont.table.subset[,2]) == GO_List.subset[i]),1]))
#	  ONT_Misses = length(unique(GO_Gene_Table[which(as.character(GO_Gene_Table[,2]) == GO_List.subset[i] & GO_Organism.Select),1])) - ONT_Hits
#	  Other_Hits = length(unique(GENE.Ont.table.subset[,1])) - ONT_Hits
#	  Other_Misses = TOTAL_GO_GENES - Other_Hits
	  
#	  GO_PVAL.temp[i] = fisher.test(matrix(c(ONT_Hits,ONT_Misses,Other_Hits,Other_Misses),nrow=2,ncol=2),alternative="greater")$p.value

          ONT_Hits = length(unique(GENE.Ont.table.subset[which(as.character(GENE.Ont.table.subset[,2]) == X),1]))
          ONT_Misses = length(unique(GO_Gene_Table[which(as.character(GO_Gene_Table[,2]) == X & GO_Organism.Select),1])) - ONT_Hits
          Other_Hits = length(unique(GENE.Ont.table.subset[,1])) - ONT_Hits
          Other_Misses = TOTAL_GO_GENES - Other_Hits
     
          GO_PVAL.temp = fisher.test(matrix(c(ONT_Hits,ONT_Misses,Other_Hits,Other_Misses),nrow=2,ncol=2),alternative="greater")$p.value
	  return(GO_PVAL.temp)

	}
	)#GO_PVAL.temp = sapply(GO_List.subset, FUN=function(X)

	GO_PVAL = p.adjust(GO_PVAL.temp,method = input$GO_Pval_Correction)
		
	log10_GO_Pvals = -log10(GO_PVAL)
	
	MIN.SELECT = min(10,length(GO_PVAL))
	MAX.SELECT = max(MIN.SELECT,length(which(GO_PVAL <= input$GO_PvalThreshold)))	


	log10_GO_Pvals.ordered = log10_GO_Pvals[order(GO_PVAL,decreasing=F)]
	GO_List.subset.ordered = GO_List.subset[order(GO_PVAL,decreasing=F)]

	GO_List.subset.ordered.Names = GO.info[match(GO_List.subset.ordered,GO.info[,1]),2]


	if(input$GO_Labels.x == "GO ID")
	{
		x <- GO_List.subset.ordered[1:MAX.SELECT]
		text <- GO_List.subset.ordered.Names[1:MAX.SELECT]
	}#if(input$GO_Labels.x == "GO ID")

	if(input$GO_Labels.x == "GO Name")
	{
		x <- GO_List.subset.ordered.Names[1:MAX.SELECT]
		text <- GO_List.subset.ordered[1:MAX.SELECT]
	}

        #x <- GO_List.subset.ordered[1:MAX.SELECT]
        y <- log10_GO_Pvals.ordered[1:MAX.SELECT]
        #text <- GO_List.subset.ordered.Names[1:MAX.SELECT]
	data <- data.frame(x, y, text,stringsAsFactors=F)
	data$x <- factor(data$x, levels = unique(data$x)[order(data$y, decreasing = TRUE)])


	GO_Barplot_data = data
	GO_Barplot_text = text

	shinyjs::show(id = "Ontology_Barplot_Pvalues_div")
  output$Ontology_Barplot_Pvalues <- renderPlotly({

		plot_ly(GO_Barplot_data, x = ~x, y = ~y, type = 'bar', text = GO_Barplot_text,
        	     marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
		  layout(title = "Enriched GO's",
        	  xaxis = list(title = "GO"),
        	  yaxis = list(title = "-log10 pvalue"))

        })#output$Ontology_Barplot_Pvalues
        

	save(list = c("GO_Barplot_data","GO_Barplot_text"), file=paste(Backup_Session_Folder,"/GO_BarPlot.rdata",sep=""))	
 

        #updateRadioButtons(session,inputId ="Select_GO",choices = GO.count.table[,1])
        Represented.gene.list = paste(Represented.gene.list,collapse = "\n")
        updateTextAreaInput(session,inputId ="Represented_Genes",label = paste("Represented Genes",sep=""), value=Represented.gene.list)
        
        GO_Prev_Step <<- "TOP"
      }#if(nrow(GO.count.table) > 0)
      
    }#if (nchar(Test.Gene.List) > 0)
    
    
    ###############################################         
    progress$inc(0.9, detail = paste("Progress: ",99,"%",sep=""))
    Sys.sleep(0.001)
    ###############################################  
  })#observeEvent(input$GO_GeneList_Button,{
  
    #####################################
  #####################################
  #####################################
#######################################
#########################################
###########################################

  ###########################  
  #####################################
  #####################################
  #####################################
  #####################################
  #input$Select_GO_Button
  #####################################
  #####################################  
  observeEvent(input$Select_GO_Button,ignoreInit = TRUE,{
    ###########################    
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "In Progress:Exploring GO", value = 0)
    # Increment the progress bar, and update the detail text.
    ###############################################         
    progress$inc(0.1, detail = paste("Progress: ",10,"%",sep=""))
    Sys.sleep(0.001)
    ############################################### 
    

    if(!exists("GO.info"))
    {
           Existing_Objects = ls()
           load("Data/ROGUE_GO_Workspace.RData")
           New_Objects = setdiff(ls(), Existing_Objects)
           Make_Global_Object_Command = paste(New_Objects,"<<-", New_Objects)

           for(Command in Make_Global_Object_Command)
           {
                eval(expr = parse(text = Command))
           }#for(Command in Make_Global_Object_Command)

    }#if(!exists("GO.info"))   

 
    if(length(GO_Prev_Step) > 0)
    {
      
      isolate(GO_Selection <- as.character(input$Select_GO))
      
      if(GO_Selection != GO_Prev_Step[length(GO_Prev_Step)])
      {
        
        GO_Prev_Step <<- c(GO_Prev_Step,GO_Selection)
        #Select_GO_Button
        
        
        Selected.GO.sub = GO.info$GO.IDs[match(GO_Selection,GO.info$GO.Names)]
        Selected.GO.sub.inx = which(substr(GO.hierarchy,1,10) == Selected.GO.sub)
        
        if(length(Selected.GO.sub.inx) > 0)
        {
          
          GO.sub.list = unlist(strsplit(GO.hierarchy[Selected.GO.sub.inx],split=" "))
          
          GO_Main = GO.sub.list[1]
          Sub_GO_List = GO.sub.list[-1]
          
          Sub_GO_List.names = as.character(GO.info$GO.Names[match(Sub_GO_List,GO.info$GO.IDs)])
          
        }#if(length(Selected.GO.sub.inx) > 0)
        
        if(length(Selected.GO.sub.inx) == 0)
        {
          Sub_GO_List.names = GO_Selection
        }#if(length(Selected.GO.sub.inx) == 0)
        
        GO.count.table = data.frame(Sub_GO_List.names,0)
        GO.info.count.check = c()
        
        ###############################################         
        progress$inc(0.2, detail ="Progress: 20%")
        Sys.sleep(0.001)
        ############################################### 
        for(i in 1:length(Sub_GO_List.names))
        {
          
          #length(as.character(GO.info.count$GO.IDs[which(GO.info.count$GO.Names == GO_Top_Level[i])]))
          
          Selected.GO = GO.info$GO.IDs[match(Sub_GO_List.names[i],GO.info$GO.Names)]
          Selected.GO.inx = which(substr(GO.EXT.hierarchy,1,10) == Selected.GO)
          
          if(length(Selected.GO.inx) == 0)
          {
            GO.sub.list = Selected.GO
          }
          if(length(Selected.GO.inx) > 0)
          {
            GO.sub.list = unlist(strsplit(GO.EXT.hierarchy[Selected.GO.inx],split=" "))
          } 
          
          GO.info.count.Subset = GO.info.count[which(GO.info.count$GO.IDs %in% GO.sub.list),]
          
          GO.info.count.check = rbind(GO.info.count.check,GO.info.count.Subset)
          
          if(ncol(GO.info.count.Subset)==3){ Total_genes = max(GO.info.count.Subset[,3]) }
          if(ncol(GO.info.count.Subset)>3){ Total_genes = sum(apply(GO.info.count.Subset[,-1:-2], 2, function(x) max(x, na.rm = TRUE))) }
          if(ncol(GO.info.count.Subset)<3){ Total_genes = 0}
          GO.count.table[i,2] = as.numeric(Total_genes)
          
        }#for(i in 1:length(Sub_GO_List.names))
        ###############################################         
        progress$inc(0.4, detail = "Progress: 40%")
        Sys.sleep(0.001)
        ############################################### 
        
        GO.count.table = GO.count.table[GO.count.table[,2]!=0,]
        if(nrow(GO.count.table) < 1)
        {
          Sub_GO_List.names = GO_Selection
          GO.count.table = data.frame(Sub_GO_List.names,0)
          GO.info.count.check = c()
          
          GO.sub.list = GO.info$GO.IDs[match(Sub_GO_List.names,GO.info$GO.Names)]
          GO.info.count.Subset = GO.info.count[which(GO.info.count$GO.IDs %in% GO.sub.list),]
          GO.info.count.check = rbind(GO.info.count.check,GO.info.count.Subset)
          if(ncol(GO.info.count.Subset)==3){ Total_genes = max(GO.info.count.Subset[,3]) }
          if(ncol(GO.info.count.Subset)>3){ Total_genes = sum(apply(GO.info.count.Subset[,-1:-2], 2, function(x) max(x, na.rm = TRUE))) }
          if(ncol(GO.info.count.Subset)<3){ Total_genes = 0}
          GO.count.table[1,2] = as.numeric(Total_genes)
        }#if(nrow(GO.count.table) == 0)  
        
        
        GO.info.count.check = GO.info.count.check[,c(1,2,(which(!colSums(GO.info.count.check[c(-1:-2)]) == 0)+2))]
        if(ncol(GO.info.count.check)==3){ Represented.gene.list = colnames(GO.info.count.check)[3] }
        if(ncol(GO.info.count.check)>3)
        {
          names.indx = which(apply(GO.info.count.check[,-1:-2], 2, function(x) max(x, na.rm = TRUE)) > 0)
          Represented.gene.list = colnames(GO.info.count.check)[(2+names.indx)] 
        }
        
        GO.count.table = GO.count.table[GO.count.table[,2]!=0,]
        
        if(nrow(GO.count.table) > 0)
        {
          shinyjs::show(id = "Ontology_Barplot_div")
          output$Ontology_Barplot <- renderPlot({
            bar_labels = as.character(GO.count.table[,1])
            bar_labels = gsub("_"," ",bar_labels)
            bar_labels = gsub(" ","\n",bar_labels)
            bar_labels = gsub("to\n","to ",bar_labels)
            bar_labels = gsub("and\n","and ",bar_labels)
            bar_labels = gsub("of\n","of ",bar_labels)
            bar_labels = gsub("cell\n","cell ",bar_labels)
            bar_labels = gsub("or\n","or ",bar_labels)
            bar_labels = gsub("in\n","in ",bar_labels)
            bar_labels = gsub("DNA\n","DNA ",bar_labels)
            bar_labels = gsub("RNA\n","RNA ",bar_labels)
            bar_labels = gsub("\nI"," I",bar_labels)
            bar_labels = gsub("\ntag"," tag",bar_labels)
            bar_labels = gsub("\nacid"," acid",bar_labels)
            bar_labels = gsub("mast\n","mast ",bar_labels)
            bar_labels = gsub("T\n","T ",bar_labels)
            bar_labels = gsub("\nion"," ion",bar_labels)
            bar_labels = gsub("\nion"," ion",bar_labels)
           
	    barplot.order = order(GO.count.table[,2],decreasing=T) 

            barplot(GO.count.table[barplot.order,2],names=bar_labels[barplot.order],las=2,cex.names=0.5,col=c(1:nrow(GO.count.table)))
          })#output$Ontology_Barplot
          
          barplot.order = order(GO.count.table[,2],decreasing=T) 
          updateRadioButtons(session,inputId ="Select_GO",choices = GO.count.table[barplot.order,1] )
         
	  Chosen.organism = c()
          if(length(input$Organism_Checkbox)==0)
          {
            Chosen.organism = c(9606,10090)
          }
          if(length(input$Organism_Checkbox)>0)
          {
            Chosen.organism = c()
            if(any(input$Organism_Checkbox=="Human")) Chosen.organism = c(Chosen.organism,9606)
            if(any(input$Organism_Checkbox=="Mouse")) Chosen.organism = c(Chosen.organism,10090)
          }



          
          Represented.gene.list = paste(Represented.gene.list,collapse = "\n")
          updateTextAreaInput(session,inputId ="Represented_Genes",label = paste(GO_Selection,":Represented Genes",sep=""), value=Represented.gene.list)
        }#if(nrow(GO.count.table) > 0)
      }#if(GO_Selection != GO_Prev_Step[length(GO_Prev_Step)])
    }#if(GO_Prev_Step > 1)
    
    ###############################################         
    progress$inc(0.9, detail ="Progress: 95%")
    Sys.sleep(0.001)
    ############################################### 
  })#observeEvent(input$Select_GO_Button,{
  
  #####################################
  #####################################
  #####################################
#######################################
#########################################
###########################################

  ###########################  
  #####################################
  #####################################
  #####################################
  #input$Previous_GO_Button
  #####################################
  #####################################  
  observeEvent(input$Previous_GO_Button,ignoreInit = TRUE,{
    ###########################      
    ###########################  
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Going to Previous Level", value = 0)
    # Increment the progress bar, and update the detail text.
    ###############################################         
    progress$inc(0.1, detail = paste("Progress: ",10,"%",sep=""))
    Sys.sleep(0.001)
    ############################################### 
    
    if(length(GO_Prev_Step)>1)
    {
      GO_Prev_Step <<- GO_Prev_Step[-length(GO_Prev_Step)]
      
      if(GO_Prev_Step[length(GO_Prev_Step)]!="TOP")
      {
        GO_Selection = GO_Prev_Step[length(GO_Prev_Step)]
        Selected.GO.sub = GO.info$GO.IDs[match(GO_Selection,GO.info$GO.Names)]
        Selected.GO.sub.inx = which(substr(GO.hierarchy,1,10) == Selected.GO.sub)
        
        if(length(Selected.GO.sub.inx) > 0)
        {
          
          GO.sub.list = unlist(strsplit(GO.hierarchy[Selected.GO.sub.inx],split=" "))
          
          GO_Main = GO.sub.list[1]
          Sub_GO_List = GO.sub.list[-1]
          
          Sub_GO_List.names = as.character(GO.info$GO.Names[match(Sub_GO_List,GO.info$GO.IDs)])
          
          
          
        }
        if(length(Selected.GO.sub.inx) == 0)
        {
          Sub_GO_List.names = GO_Selection
        }
        
        ###############################################         
        progress$inc(0.6, detail ="Progress: 60%")
        Sys.sleep(0.001)
        ############################################### 
        
        GO.count.table = data.frame(Sub_GO_List.names,0)
        GO.info.count.check = c()
        for(i in 1:length(Sub_GO_List.names))
        {
          
          #length(as.character(GO.info.count$GO.IDs[which(GO.info.count$GO.Names == GO_Top_Level[i])]))
          
          Selected.GO = GO.info$GO.IDs[match(Sub_GO_List.names[i],GO.info$GO.Names)]
          Selected.GO.inx = which(substr(GO.EXT.hierarchy,1,10) == Selected.GO)
          
          if(length(Selected.GO.inx) == 0)
          {
            GO.sub.list = Selected.GO
          }
          if(length(Selected.GO.inx) > 0)
          {
            GO.sub.list = unlist(strsplit(GO.EXT.hierarchy[Selected.GO.inx],split=" "))
          } 
          
          GO.info.count.Subset = GO.info.count[which(GO.info.count$GO.IDs %in% GO.sub.list),]
          GO.info.count.check = rbind(GO.info.count.check,GO.info.count.Subset)
          
          if(ncol(GO.info.count.Subset)==3){ Total_genes = max(GO.info.count.Subset[,3]) }
          if(ncol(GO.info.count.Subset)>3){ Total_genes = sum(apply(GO.info.count.Subset[,-1:-2], 2, function(x) max(x, na.rm = TRUE))) }
          if(ncol(GO.info.count.Subset)<3){ Total_genes = 0}
          GO.count.table[i,2] = as.numeric(Total_genes)
          
        }#for(i in 1:length(Sub_GO_List.names))
        
        GO.info.count.check = GO.info.count.check[,c(1,2,(which(!colSums(GO.info.count.check[c(-1:-2)]) == 0)+2))]
        if(ncol(GO.info.count.check)==3){ Represented.gene.list = colnames(GO.info.count.check)[3] }
        if(ncol(GO.info.count.check)>3)
        {
          names.indx = which(apply(GO.info.count.check[,-1:-2], 2, function(x) max(x, na.rm = TRUE)) > 0)
          Represented.gene.list = colnames(GO.info.count.check)[(2+names.indx)] 
        }
        
        GO.count.table = GO.count.table[GO.count.table[,2]!=0,]
        
        if(nrow(GO.count.table) > 0)
        {
          shinyjs::show(id = "Ontology_Barplot_div")
          output$Ontology_Barplot <- renderPlot({
            bar_labels = as.character(GO.count.table[,1])
            bar_labels = gsub("_"," ",bar_labels)
            bar_labels = gsub(" ","\n",bar_labels)
            bar_labels = gsub("to\n","to ",bar_labels)
            bar_labels = gsub("and\n","and ",bar_labels)
            bar_labels = gsub("of\n","of ",bar_labels)
            bar_labels = gsub("cell\n","cell ",bar_labels)
            bar_labels = gsub("or\n","or ",bar_labels)
            bar_labels = gsub("in\n","in ",bar_labels)
            bar_labels = gsub("DNA\n","DNA ",bar_labels)
            bar_labels = gsub("RNA\n","RNA ",bar_labels)
            bar_labels = gsub("\nI"," I",bar_labels)
            bar_labels = gsub("\ntag"," tag",bar_labels)
            bar_labels = gsub("\nacid"," acid",bar_labels)
            bar_labels = gsub("mast\n","mast ",bar_labels)
            bar_labels = gsub("T\n","T ",bar_labels)
            bar_labels = gsub("ion\n","ion ",bar_labels)
            bar_labels = gsub("ion\n","ion ",bar_labels)
          
	    barplot.order = order(GO.count.table[,2],decreasing=T)
  
            barplot(GO.count.table[barplot.order,2],names=bar_labels[barplot.order],las=2,cex.names=0.5,col=c(1:nrow(GO.count.table)))
          })#output$Ontology_Barplot
          
	  barplot.order = order(GO.count.table[,2],decreasing=T)
          updateRadioButtons(session,inputId ="Select_GO",choices = GO.count.table[barplot.order,1] )
          
          Represented.gene.list = paste(Represented.gene.list,collapse = "\n")
          updateTextAreaInput(session,inputId ="Represented_Genes",label = paste(GO_Selection,":Represented Genes",sep=""), value=Represented.gene.list)
        }#if(nrow(GO.count.table) > 0)
      }#if(GO_Prev_Step[-length(GO_Prev_Step)]!="TOP")
      if(GO_Prev_Step[length(GO_Prev_Step)]=="TOP")
      {
        
        ########################################  
        GO.info.count <<- GO.info
        
        Chosen.organism = c()
        if(length(input$Organism_Checkbox)==0)
        {
          Chosen.organism = c(9606,10090)
        }
        if(length(input$Organism_Checkbox)>0)
        {
          Chosen.organism = c()
          if(any(input$Organism_Checkbox=="Human")) Chosen.organism = c(Chosen.organism,9606)
          if(any(input$Organism_Checkbox=="Mouse")) Chosen.organism = c(Chosen.organism,10090)
        }
        
        
        Chosen.curation <- substr(input$Ontology_Method,1,3)
        Chosen.curation = paste("\t",Chosen.curation,"\t",sep="")
        Human_GO_select = Human_GO[grep(paste(Chosen.curation,collapse="|"),Human_GO)]
        Human_GO_select = Human_GO_select[grep(paste(Chosen.organism,collapse="|"),Human_GO_select)]
        
        
        isolate(Test.Gene.List <- input$GO_GeneList)
        if (nchar(Test.Gene.List) > 0)
        {
          
          Test.Gene.List = gsub(","," ",gsub("\t"," ",gsub("\n", " ", Test.Gene.List)))
          Test.Gene.List = unlist(strsplit(Test.Gene.List,split = " "))
          
          #Target.GO.Lines = Human_GO_select[grep(paste(Test.Gene.List,collapse = "|"),Human_GO_select)]
	  Target.GO.Lines = Human_GO_select[grep(paste("\t",Test.Gene.List,"\t",sep="",collapse = "|"),Human_GO_select)]
          Target.GO.table = c()
          for(j in 1:length(Target.GO.Lines))
          {
            temp.line = unlist(strsplit(Target.GO.Lines[j],split="\t"))
            Target.GO.table = rbind(Target.GO.table,temp.line)
          } #for(j in 1:length(Target.GO.Lines))
          if(ncol(Target.GO.table)>1)
          {
            if(length(Target.GO.table[which(Target.GO.table[,3] %in% Test.Gene.List),3])>0)
            {
              #Target.GO.IDs = unique(unlist(Target.GO.table[which(Target.GO.table[,3]==Test.Gene.List[i]),5]))
              for(k in 1:length(unique(Target.GO.table[which(Target.GO.table[,3] %in% Test.Gene.List),3])))
              {
                GO.info.count <<- data.frame(cbind(GO.info.count,0))
              }
              colnames(GO.info.count)[3:ncol(GO.info.count)] <<- unique(Target.GO.table[which(Target.GO.table[,3] %in% Test.Gene.List),3])
            }#if(length(Target.GO.table[which(Target.GO.table[,3] %in% Test.Gene.List),3])>0)
          } #if(ncol(Target.GO.table)>1)
          
          
          ##########
          ##########STEP
          ##########
          
          if(ncol(GO.info.count)>2)
          {
            for(i in 3:ncol(GO.info.count))
            {
              Target.GO.ID.List = unique(subset(Target.GO.table,Target.GO.table[,3]==colnames(GO.info.count)[i])[,5])
              GO.info.count[which(as.character(GO.info.count$GO.IDs) %in% Target.GO.ID.List),i] <<- 1
            } #for(i in 3:ncol(GO.info.count))
          } #if(ncol(GO.info.count)>2)
          
          ##########
          ##########STEP
          ##########
          
          ###############################################         
          progress$inc(0.6, detail ="Progress: 60%")
          Sys.sleep(0.001)
          ############################################### 
          
          ##########
          ##########STEP
          ##########
          
          
          GO.count.table = data.frame(GO_Top_Level,0)
          GO.info.count.check = c()
          for(i in 1:length(GO_Top_Level))
          {
            
            #length(as.character(GO.info.count$GO.IDs[which(GO.info.count$GO.Names == GO_Top_Level[i])]))
            
            Selected.GO = GO.info$GO.IDs[match(GO_Top_Level[i],GO.info$GO.Names)]
            Selected.GO.inx = which(substr(GO.EXT.hierarchy,1,10) == Selected.GO)
            #GO.sub.list = unlist(strsplit(GO.EXT.hierarchy[Selected.GO.inx],split=" "))
            
            if(length(Selected.GO.inx) == 0)
            {
              GO.sub.list = Selected.GO
            }
            if(length(Selected.GO.inx) > 0)
            {
              GO.sub.list = unlist(strsplit(GO.EXT.hierarchy[Selected.GO.inx],split=" "))
            } 
            
            GO.info.count.Subset = GO.info.count[which(GO.info.count$GO.IDs %in% GO.sub.list),]
            GO.info.count.check = rbind(GO.info.count.check,GO.info.count.Subset)
            
            if(ncol(GO.info.count.Subset)==3){ Total_genes = max(GO.info.count.Subset[,3]) }
            if(ncol(GO.info.count.Subset)>3){ Total_genes = sum(apply(GO.info.count.Subset[,-1:-2], 2, function(x) max(x, na.rm = TRUE))) }
            if(ncol(GO.info.count.Subset)<3){ Total_genes = 0}
            GO.count.table[i,2] = as.numeric(Total_genes)
            
          }#for(i in 1:length(GO_Top_Level))
          
          GO.info.count.check = GO.info.count.check[,c(1,2,(which(!colSums(GO.info.count.check[c(-1:-2)]) == 0)+2))]
          if(ncol(GO.info.count.check)==3){ Represented.gene.list = colnames(GO.info.count.check)[3] }
          if(ncol(GO.info.count.check)>3)
          {
            names.indx = which(apply(GO.info.count.check[,-1:-2], 2, function(x) max(x, na.rm = TRUE)) > 0)
            Represented.gene.list = colnames(GO.info.count.check)[(2+names.indx)] 
          }
          
          GO.count.table = GO.count.table[GO.count.table[,2]!=0,]
          
          if(nrow(GO.count.table) > 0)
          {
            shinyjs::show(id = "Ontology_Barplot_div")
            output$Ontology_Barplot <- renderPlot({
              bar_labels = as.character(GO.count.table[,1])
              bar_labels = gsub("_"," ",bar_labels)
              bar_labels = gsub(" ","\n",bar_labels)
              
              barplot(GO.count.table[,2],names=bar_labels,las=2,cex.names=0.6,col=c(1:nrow(GO.count.table)))
            })# output$Ontology_Barplot
            
            updateRadioButtons(session,inputId ="Select_GO",choices = GO.count.table[,1])
            Represented.gene.list = paste(Represented.gene.list,collapse = "\n")
            updateTextAreaInput(session,inputId ="Represented_Genes",label = paste("Represented Genes",sep=""), value=Represented.gene.list)
            
            GO_Prev_Step <<- "TOP"
          }#if(nrow(GO.count.table) > 0)
          
        }#if (nchar(Test.Gene.List) > 0)
        ###########################################
      }#if(GO_Prev_Step[length(GO_Prev_Step)]=="TOP")
      
    }#if(length(GO_Prev_Step)>0)
    
    ###############################################         
    progress$inc(0.9, detail ="Progress: 99%")
    Sys.sleep(0.001)
    ############################################### 
  })#observeEvent(input$Previous_GO_Button,{

  #####################################
  #####################################  
  observeEvent(input$Group_Stats_Run_tSNE,ignoreInit = TRUE,{
    ######################################
    ######################################
   ######################
   
   	if(length(DATA.Values.5min) <= 1)
	{	
		shinyalert(title = "Data Error",text = "Please load data into the tool", type = "warning")
		return(NULL)
	}
   
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Comparing Gene Expressions", value = 0)
    # Increment the progress bar, and update the detail text.
    progress$inc(0.1, detail = "Something is Happening!")
    Sys.sleep(0.001)
   ######################
    
    
    Group_Stats.tSNE.Gene.List.inx = c()

    isolate(Group_Stats.tSNE.Gene.List <- input$Group_Stats_tSNE_Gene_List)
    if (nchar(Group_Stats.tSNE.Gene.List) > 0)
    {
      
      Group_Stats.tSNE.Gene.List = gsub("\"","",gsub(","," ",gsub("\t"," ",gsub("\n", " ", Group_Stats.tSNE.Gene.List))))
      Group_Stats.tSNE.Gene.List = unlist(strsplit(Group_Stats.tSNE.Gene.List,split = " "))
      Group_Stats.tSNE.Gene.List = unique(Group_Stats.tSNE.Gene.List)
      
      Group_Stats.tSNE.Gene.List.inx = which(rownames(DATA.Values.5min) %in% Group_Stats.tSNE.Gene.List)
    }#if (nchar(Test.Gene.List) > 0

    if ( (nchar(Group_Stats.tSNE.Gene.List) < 2) | length(Group_Stats.tSNE.Gene.List.inx) < 2)
    {
	Group_Stats.tSNE.Gene.List = unique(rownames(DATA.Values.5min))
	Group_Stats.tSNE.Gene.List.inx = which(rownames(DATA.Values.5min) %in% Group_Stats.tSNE.Gene.List)	
	Group_Stats.tSNE.Gene.List.inx = unique(Group_Stats.tSNE.Gene.List.inx)
    }
      
    updateTextAreaInput(session, inputId = "Group_Stats_t_SNE_2DGeneList",value = "")
    updateTextAreaInput(session, inputId = "Group_Stats_t_SNE_3DGeneList",value = "")
    output$Group_Stats_t_SNE_3Dplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
    output$Group_Stats_t_SNE_plotly <- renderPlotly(plot_ly())
    output$Group_Stats_t_SNE_3Dplotly <- renderPlotly(plot_ly())
    
    Group_Stat.Compare.1 <- isolate(input$Group1_Stat_compare)
    Group_Stat.Compare.2 <- isolate(input$Group2_Stat_compare)
    Group_Stat.RPKM.Threshold <- isolate(input$Group_Stat_RPKM_Thresh)
    Group_Stat.Pval <- isolate(input$Group_Stat_PVal_Thresh)
    Group_Stat.tSNE.Gene <- isolate(input$Group_Stats_tSNE_Gene)
    Group_Stats.tSNE.max.iter <- isolate(input$Group_Stats_tSNE_max_iter)
    
    if(!is.null(nrow(DATA.Values.5min)))
    if(nrow(DATA.Values.5min)>10)
    {
      Group.Index=c()
      Gene.Index=c()
      
      Group.Index[1] = which(Groups == Group_Stat.Compare.1)
      Group.Index[2] = which(Groups == Group_Stat.Compare.2)
      
      
      if(nchar(Group_Stat.Compare.1)>=1 & nchar(Group_Stat.Compare.2)>=1)
      {
       Group.Member.index.1 = match(unlist(strsplit(Group.Members[Group.Index[1]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))
       Group.Member.index.2 = match(unlist(strsplit(Group.Members[Group.Index[2]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))
      }#if(nchar(Group_Stat.Compare.1)>=1 & nchar(Group_Stat.Compare.2)>=1)
      if(nchar(Group_Stat.Compare.1)<1 & nchar(Group_Stat.Compare.2)<1)
      {
       Group.Member.index.1 = 1:ncol(DATA.Values.5min)
       Group.Member.index.2 = 1:ncol(DATA.Values.5min)
      }#f(nchar(Group_Stat.Compare.1)<1 & nchar(Group_Stat.Compare.2)<1)
      
      if(length(Group.Member.index.1) <= 1 | length(Group.Member.index.2) <= 1)
      {
       shinyalert(title = "Small group size", text="If groups have less than two members, then all samples will be used in this analysis", type = "warning")

       Group.Member.index.1 = 1:ncol(DATA.Values.5min)
       Group.Member.index.2 = 1:ncol(DATA.Values.5min)
      }#if(length(Group.Member.index.1) <= 1 | length(Group.Member.index.2) <= 1)

      
      
      Control.Treatment.data = DATA.Values.5min[,c(Group.Member.index.1,Group.Member.index.2)]
      Group.1.data = DATA.Values.5min[,Group.Member.index.1]
      Group.2.data = DATA.Values.5min[,Group.Member.index.2]
      #Treatment.data = DATA.Values.5min[,Group.Member.index.2]
      
      #Control.Treatment.data.5.inx = which(apply(Control.Treatment.data, MARGIN = 1, function(x) all(x) >= Group_Stat.RPKM.Threshold))
      Control.Treatment.data.5.inx = which(apply(Group.1.data, MARGIN = 1, function(x) median(x) >= Group_Stat.RPKM.Threshold) & apply(Group.2.data, MARGIN = 1, function(x) median(x) >= Group_Stat.RPKM.Threshold))
      Control.Treatment.data.009.inx = which(apply(Group.1.data, MARGIN = 1, function(x) any(x > 0.009)) & apply(Group.2.data, MARGIN = 1, function(x) any(x > 0.009)))
      
      
      if (nchar(Group_Stats.tSNE.Gene.List) > 0)
      {
        Control.Treatment.data.5.inx = intersect(Group_Stats.tSNE.Gene.List.inx,Control.Treatment.data.5.inx)
      }#if (nchar(Group_Stats.tSNE.Gene.List) > 0)
      
      Control.Treatment.data.5.inx = intersect(Control.Treatment.data.009.inx,Control.Treatment.data.5.inx)
      

      if(length(Control.Treatment.data.5.inx)>=2)
      {
      
        #Treatment.data.5.inx = which(apply(Treatment.data, MARGIN = 1, function(x) mean(x) >= Group_Stat.RPKM.Threshold))
        Combined.inx = unique(c(Control.Treatment.data.5.inx))
        
	TSNE_TYPE <- isolate(input$Select_tSNE_Object)
	if(TSNE_TYPE == "GENES")
	{
        	New.Data.Subset = DATA.Values.5min[Combined.inx,c(Group.Member.index.1,Group.Member.index.2)]
        
		rownames(New.Data.Subset) = rownames(DATA.Values.5min[Combined.inx,])

		New.Data.Subset.filter0.inx = apply(New.Data.Subset,MARGIN = 1, function(X) any(X!=min(New.Data.Subset)))

		New.Data.Subset = New.Data.Subset[New.Data.Subset.filter0.inx,]

	}#if(TSNE_TYPE == "GENES")

	if(TSNE_TYPE == "SAMPLES") 
	{
		New.Data.Subset = t(DATA.Values.5min[Combined.inx,])
		
		rownames(New.Data.Subset) = colnames(DATA.Values.5min)

		New.Data.Subset.filter1.inx = apply(New.Data.Subset,MARGIN = 2, function(X) any(X!=min(New.Data.Subset)))
		New.Data.Subset = New.Data.Subset[,New.Data.Subset.filter1.inx]

	}#if(TSNE_TYPE == "SAMPLES")

	if(any(duplicated(New.Data.Subset)))
	{
		
		DUP.Rows = which(duplicated(New.Data.Subset))
		All.Cols = ncol(New.Data.Subset)

		rand.val.mat = matrix(runif((length(DUP.Rows)*All.Cols),0.00005,0.00007),nrow=length(DUP.Rows),ncol=All.Cols)

		New.Data.Subset[DUP.Rows,] = New.Data.Subset[DUP.Rows,] + rand.val.mat

	}#if(any(duplicated(New.Data.Subset)))


        Control.Label = Group_Stat.Compare.1
        Treatment.Label = Group_Stat.Compare.2
       
     
        if(nrow(New.Data.Subset) >= 10) PERPLEX = 2
	if(nrow(New.Data.Subset) >= 4 & nrow(New.Data.Subset) < 10) PERPLEX = 1
	if(nrow(New.Data.Subset) >= 2 & nrow(New.Data.Subset) < 4) PERPLEX = 0.2
    ##########################################################
        progress$inc(0.2, detail = "Calculating 3D t-SNE")
        Sys.sleep(0.001)
    #########################################################    
        
        if(input$Group_Stats_Select_Method == "tSNE")
        {
          #New.Data.Subset.tsne.3 = tsne(New.Data.Subset,k=3, max_iter = Group_Stats.tSNE.max.iter)
          New.Data.Subset.tsne.3 = Rtsne(X= (New.Data.Subset),dim=3,perplexity=PERPLEX, max_iter= Group_Stats.tSNE.max.iter)$Y
          
             ##########################################################
            progress$inc(0.25, detail = "Calculating 2D t-SNE")
            Sys.sleep(0.001)
            ######################################################### 
          
          New.Data.Subset.tsne.2 = Rtsne(X= (New.Data.Subset),dim=2,perplexity=PERPLEX, max_iter= Group_Stats.tSNE.max.iter)$Y
        }#if(input$Group_Stats_Select_Method == "tSNE")

        if(input$Group_Stats_Select_Method == "PCA")
        {
          New.Data.Subset.tsne.3 = prcomp(New.Data.Subset)$x[,c(1:3)]
          
             ##########################################################
            progress$inc(0.25, detail = "Calculating 2D PCA")
            Sys.sleep(0.001)
            ######################################################### 
          
           New.Data.Subset.tsne.2 = prcomp(New.Data.Subset)$x[,c(1:2)]
           
        }#if(input$Group_Stats_Select_Method == "tSNE")
        if(input$Group_Stats_Select_Method == "UMAP")
        {
          New.Data.Subset.tsne.3 = umap(New.Data.Subset,n_components = 3,n_neighbors = (floor(nrow(New.Data.Subset)/2)+1))
          
             ##########################################################
            progress$inc(0.25, detail = "Calculating 2D UMAP")
            Sys.sleep(0.001)
            ######################################################### 
          
           New.Data.Subset.tsne.2 = prcomp(New.Data.Subset)$x[,c(1:2)]
        }#if(input$Group_Stats_Select_Method == "UMAP")
        
        #New.Data.Subset.tsne.2 = tsne(New.Data.Subset,k=2, max_iter = Group_Stats.tSNE.max.iter)
        New.Data.Subset.tsne.2 = umap(New.Data.Subset,n_components = 2,n_neighbors = (floor(nrow(New.Data.Subset)/2)+1))

    ##########################################################
        progress$inc(0.275, detail = "Something is Happening! Do you feel lucky?")
        Sys.sleep(0.001)
    #########################################################
        
        Expr_Levels.1 = c()
        Expr_Levels.2 = c()
        #for(k in 1:length(Combined.inx))
        {
          #Expr_Levels.1[k] = paste(round(DATA.Values.5min[Combined.inx[k],Group.Member.index.1],1),sep="",collapse=",")
          #Expr_Levels.2[k] = paste(round(DATA.Values.5min[Combined.inx[k],Group.Member.index.2],1),sep="",collapse=",")

           Expr_Levels.1 = sapply(Combined.inx, function(x) paste(round(DATA.Values.5min[x,Group.Member.index.1],1),sep="",collapse=","))
           Expr_Levels.2 = sapply(Combined.inx, function(x) paste(round(DATA.Values.5min[x,Group.Member.index.2],1),sep="",collapse=","))

        }#for(k in 1:length(Combined.inx))
       
        
        D1.matrix3D = matrix(nrow=nrow(New.Data.Subset.tsne.3),ncol=nrow(New.Data.Subset.tsne.3))
        D1.matrix2D = matrix(nrow=nrow(New.Data.Subset.tsne.2),ncol=nrow(New.Data.Subset.tsne.2))
        #for(i in 1:nrow(D1.matrix3D))
        #{
        #  for(j in 1:ncol(D1.matrix3D))
        #  {
        #    D1.matrix3D[i,j] = sqrt(sum((New.Data.Subset.tsne.3[i,]-New.Data.Subset.tsne.3[j,])^2))
        #    D1.matrix2D[i,j] = sqrt(sum((New.Data.Subset.tsne.2[i,]-New.Data.Subset.tsne.2[j,])^2))
        #  }#for(i in 1:nrow(D1.matrix3D))
        #}#for(j in 1:ncol(D1.matrix3D))
        
		D1.matrix3D = apply(New.Data.Subset.tsne.3, MARGIN = 1, function(X) apply(New.Data.Subset.tsne.3, MARGIN=1, function(Y) sqrt(sum((Y - X)^2))))
        D1.matrix2D = apply(New.Data.Subset.tsne.2, MARGIN = 1, function(X) apply(New.Data.Subset.tsne.2, MARGIN=1, function(Y) sqrt(sum((Y - X)^2))))
        
      ##########################################################
        progress$inc(0.3, detail = "Determining p-values")
        Sys.sleep(0.001)
      #########################################################
        D1.matrix.3Dpvals = matrix(nrow=nrow(New.Data.Subset.tsne.3),ncol=nrow(New.Data.Subset.tsne.3))
        D1.matrix.2Dpvals = matrix(nrow=nrow(New.Data.Subset.tsne.2),ncol=nrow(New.Data.Subset.tsne.2))
        
        D1.matrix3D.density = density(D1.matrix3D)
        D1.matrix2D.density = density(D1.matrix2D)
        
        #for(i in 1:nrow(D1.matrix.2Dpvals))
        #{
        #  for(j in 1:ncol(D1.matrix.2Dpvals))
        #  {
        #    D1.matrix.3Dpvals[i,j] = sum(D1.matrix3D.density$y[D1.matrix3D.density$x <= D1.matrix3D[i,j]])/sum(D1.matrix3D.density$y)
        #    D1.matrix.2Dpvals[i,j] = sum(D1.matrix2D.density$y[D1.matrix2D.density$x <= D1.matrix2D[i,j]])/sum(D1.matrix2D.density$y)
        #    
        #  }#for(i in 1:nrow(D1.matrix.2Dpvals))
        #}#for(j in 1:ncol(D1.matrix.2Dpvals))
      
        D1.matrix3D.density.pvals = sapply(D1.matrix3D, function(X) sum(D1.matrix3D.density$y[D1.matrix3D.density$x <= X])/sum(D1.matrix3D.density$y))
        D1.matrix3D.density = matrix( D1.matrix3D.density.pvals, nrow=nrow(New.Data.Subset.tsne.3), ncol=nrow(New.Data.Subset.tsne.3))
		D1.matrix2D.density.pvals = sapply(D1.matrix2D, function(X) sum(D1.matrix2D.density$y[D1.matrix2D.density$x <= X])/sum(D1.matrix2D.density$y))
        D1.matrix2D.density = matrix( D1.matrix2D.density.pvals, nrow=nrow(New.Data.Subset.tsne.2), ncol=nrow(New.Data.Subset.tsne.2))

      ##########################################################
        progress$inc(0.33, detail = "Something Mysterious is Happening!")
        Sys.sleep(0.001)
      #########################################################  
        D2.3Dval=0
        h2=0
        while(D2.3Dval <= sum(density(D1.matrix3D)$y)*Group_Stat.Pval)
        {
          h2=h2+1
          D2.3Dval = sum(density(D1.matrix3D)$y[1:h2])
        }#while(D2.3Dval <= sum(density(D1.matrix3D)$y)*Group_Stat.Pval)
        alfa3D = density(D1.matrix3D)$x[(h2-1)]
       
        D2.2Dval=0
        h2=0
        while(D2.2Dval <= sum(density(D1.matrix2D)$y)*Group_Stat.Pval)
        {
          h2=h2+1
          D2.2Dval = sum(density(D1.matrix2D)$y[1:h2])
        }#while(D2.2Dval <= sum(density(D1.matrix2D)$y)*Group_Stat.Pval)
        alfa2D = density(D1.matrix2D)$x[(h2-1)]
        
            
         
        sig.val.count=c()
        #for(k in 1:nrow(D1.matrix3D))
        #{
        #  sig.val.count[k] = length(which(D1.matrix3D[k,] <= alfa3D))
        #}#for(k in 1:nrow(D1.matrix3D))

	sig.val.count = apply(D1.matrix3D, MARGIN=1, FUN=function(x) length(which(x <= alfa3D)))
        
      ##########################################################
        progress$inc(0.35, detail = "Generating Plots!")
        Sys.sleep(0.001)
      ######################################################### 
      
        rownames(New.Data.Subset.tsne.3) = rownames(New.Data.Subset)
        colnames(New.Data.Subset.tsne.3) = c("X","Y","Z")
        New.Data.Subset.tsne.3 = data.frame(New.Data.Subset.tsne.3)
        
        rownames(New.Data.Subset.tsne.2) = rownames(New.Data.Subset)
        colnames(New.Data.Subset.tsne.2) = c("X","Y")
        New.Data.Subset.tsne.2 = data.frame(New.Data.Subset.tsne.2)
        
        
        sig.val.count.norm = (sig.val.count+1)/(max(sig.val.count)+1)
        R.scale = (New.Data.Subset.tsne.3[,1]-min(New.Data.Subset.tsne.3[,1]))/(max(New.Data.Subset.tsne.3[,1])-min(New.Data.Subset.tsne.3[,1]))
        G.scale = (New.Data.Subset.tsne.3[,2]-min(New.Data.Subset.tsne.3[,2]))/(max(New.Data.Subset.tsne.3[,2])-min(New.Data.Subset.tsne.3[,2]))*0.75
        B.scale = (New.Data.Subset.tsne.3[,3]-max(New.Data.Subset.tsne.3[,3]))/(min(New.Data.Subset.tsne.3[,3])-max(New.Data.Subset.tsne.3[,3]))
      
        
        shinyjs::show(id = "Group_Stats_t_SNE_3Dplot_div")
        output$Group_Stats_t_SNE_3Dplot <- renderPlot({
  
             
          s3d = scatterplot3d(New.Data.Subset.tsne.3,pch=16,cex.symbols=0.75,color=rgb(R.scale,G.scale,B.scale,sig.val.count.norm),xlab = "X",ylab = "Y",zlab = "Z")
          
          if(length(Group_Stat.tSNE.Gene>0))
          {
            
            Group_Stat.Gene.Matches = match(Group_Stat.tSNE.Gene,rownames(New.Data.Subset.tsne.3))

	    if(length(Group_Stat.Gene.Matches[!is.na(Group_Stat.Gene.Matches)]) >= 1 )
	    {
            	scatter3d.labels = rownames(New.Data.Subset.tsne.3)[Group_Stat.Gene.Matches[!is.na(Group_Stat.Gene.Matches)]]
            	scatter3d.labels.coords = New.Data.Subset.tsne.3[Group_Stat.Gene.Matches[!is.na(Group_Stat.Gene.Matches)],]
            	if(length(scatter3d.labels>0))
            	{
            	  text(s3d$xyz.convert(scatter3d.labels.coords), labels=scatter3d.labels, pos=1) 
            	}#if(length(scatter3d.labels>0))
	    }#if(length(Group_Stat.Gene.Matches) >= 1 & length(which(is.na(Group_Stat.Gene.Matches))))	
          }#if(length(Group_Stat.tSNE.Gene>0))
        })#output$Group_Stats_t_SNE_3Dplot <- renderPlot
       
        plotlyMargins <- list(
          l = 50,
          r = 50,
          b = 100,
          t = 100,
          pad = 4
        )
        
        #####################
        #output$Group_Stats_t_SNE_3Dplotly <- renderPlotly({
          ###########
          ###########

 	if(TSNE_TYPE == "GENES")
  	{
	
	  #####################
 	  shinyjs::show(id = "Group_Stats_t_SNE_3Dplotly_div")
	  output$Group_Stats_t_SNE_3Dplotly <- renderPlotly({
       	  ###########
       	  ###########

			set.seed(as.numeric(input$Group_Stats_tSNE_Seed))
            plot_ly(data=New.Data.Subset.tsne.3,x=~X, y=~Y,z=~Z,text=paste(rownames(New.Data.Subset.tsne.3),paste(Control.Label,Expr_Levels.1,sep=": "),paste(Treatment.Label,Expr_Levels.2,sep=": "),sep="\n"),
                    marker = list(color = rgb(R.scale,G.scale,B.scale,sig.val.count.norm),
                                  symbol='circle',
                                  sizemode = 'diameter',size=as.numeric(input$Group_Stats_tSNE_pointSize))) %>%
              layout(autosize = F, width = 600, height = 600, margin = plotlyMargins)
          
	   })#output$Group_Stats_t_SNE_3Dplotly <- renderPlotly({
           #####################

	  }#if(TSNE_TYPE == "GENES")         
 
	  if(TSNE_TYPE == "SAMPLES")
          {
          	
          	
           if(length(input$Group_Stats_tSNE_Group ) > 0)  
           {
            Group.Select = input$Group_Stats_tSNE_Group 
                           
			GROUPINGS = rep("Ungrouped",nrow(New.Data.Subset.tsne.3))

			for(A in 1:length(Group.Select))
			{
 				GROUP_Selection = Group.Members[which(Groups == Group.Select[A])]
  				GROUP_Selection_MEMBERS = as.character(unlist(strsplit(as.character(GROUP_Selection),split=";")))
  				GROUPINGS[which(rownames(New.Data.Subset.tsne.3) %in% GROUP_Selection_MEMBERS)] = Group.Select[A]
			}#for(A in 1:length(Group.Select))
			
			New.Data.Subset.tsne.3.Groups = data.frame(New.Data.Subset.tsne.3, GROUPINGS)
			rownames(New.Data.Subset.tsne.3.Groups) = rownames(New.Data.Subset.tsne.3)
			New.Data.Subset.tsne.3.Groups = New.Data.Subset.tsne.3.Groups[which(GROUPINGS!="Ungrouped"),]	
			GROUPINGS = GROUPINGS[which(GROUPINGS!="Ungrouped")]
            #####################
			 shinyjs::show(id = "Group_Stats_t_SNE_3Dplotly_div")
        output$Group_Stats_t_SNE_3Dplotly <- renderPlotly({
            		###########
            		###########
				  set.seed(as.numeric(input$Group_Stats_tSNE_Seed))
	     		plot_ly(data=New.Data.Subset.tsne.3.Groups,x=~X, y=~Y,z=~Z,text=paste(rownames(New.Data.Subset.tsne.3.Groups)),color=~GROUPINGS,
             	       marker = list(symbol='circle',
                                  sizemode = 'diameter',size=as.numeric(input$Group_Stats_tSNE_pointSize))) %>%
             	 layout(autosize = F, width = 600, height = 600, margin = plotlyMargins)
           })#output$Group_Stats_t_SNE_3Dplotly <- renderPlotly
					
		 }#if(length(input$Group_Stats_tSNE_Group ) > 0)  
         
         if(length(input$Group_Stats_tSNE_Group ) == 0)
         {  
         	#####################
           shinyjs::show(id = "Group_Stats_t_SNE_3Dplotly_div")
          output$Group_Stats_t_SNE_3Dplotly <- renderPlotly({
            
            set.seed(as.numeric(input$Group_Stats_tSNE_Seed))
	     		  plot_ly(data=New.Data.Subset.tsne.3,x=~X, y=~Y,z=~Z,text=paste(rownames(New.Data.Subset.tsne.3)),
                		    marker = list(color = rgb(R.scale,G.scale,B.scale),
                                  symbol='circle',
                                  sizemode = 'diameter',size=as.numeric(input$Group_Stats_tSNE_pointSize))) %>%
              	layout(autosize = F, width = 600, height = 600, margin = plotlyMargins)
                         
	    		 })#output$Group_Stats_t_SNE_3Dplotly <- renderPlotly
            #####################
         }#if(length(input$Group_Stats_tSNE_Group ) == 0)

	  }#if(TSNE_TYPE == "SAMPLES")

        #####################
        
        if(length(Group_Stat.tSNE.Gene>0))
        {
     
     
          Group_Stat.Gene.Matches = match(Group_Stat.tSNE.Gene,rownames(New.Data.Subset.tsne.3))
          scatter3d.labels = rownames(New.Data.Subset.tsne.3)[Group_Stat.Gene.Matches[!is.na(Group_Stat.Gene.Matches)]]
          scatter3d.labels.coords = New.Data.Subset.tsne.3[Group_Stat.Gene.Matches[!is.na(Group_Stat.Gene.Matches)],]
          if(length(Group_Stat.Gene.Matches[!is.na(Group_Stat.Gene.Matches)]) >= 1) 
          {
            #########################

  	    if(TSNE_TYPE == "GENES")
            {
  	          shinyjs::show(id = "Group_Stats_t_SNE_3Dplotly_div")
              output$Group_Stats_t_SNE_3Dplotly <- renderPlotly({
              ##########
              ##########
				set.seed(as.numeric(input$Group_Stats_tSNE_Seed))
                plot_ly(data=New.Data.Subset.tsne.3,x=~X, y=~Y,z=~Z,text=paste(rownames(New.Data.Subset.tsne.3),paste(Control.Label,Expr_Levels.1,sep=": "),paste(Treatment.Label,Expr_Levels.2,sep=": "),sep="\n"),
                      marker = list(color = rgb(R.scale,G.scale,B.scale,sig.val.count.norm),
                                    symbol='circle',
                                    sizemode = 'diameter',size=2))  %>%
                  layout(autosize = F, width = 600, height = 600, margin = plotlyMargins,showlegend = FALSE) %>% 
                  add_text(x = scatter3d.labels.coords[,1],y = scatter3d.labels.coords[,2],z = scatter3d.labels.coords[,3],text = scatter3d.labels, textposition = "bottom",inherit = T) %>%
                  add_markers() 
		
		
                })#output$Group_Stats_t_SNE_3Dplotly
              }#if(TSNE_TYPE == "GENES")

	       if(TSNE_TYPE == "SAMPLES")
	       {

	         shinyjs::show(id = "Group_Stats_t_SNE_3Dplotly_div")
	         output$Group_Stats_t_SNE_3Dplotly <- renderPlotly({
                ##########
                ##########
	           set.seed(as.numeric(input$Group_Stats_tSNE_Seed))
	           plot_ly(data=New.Data.Subset.tsne.3,x=~X, y=~Y,z=~Z,text=paste(rownames(New.Data.Subset.tsne.3)),
                      marker = list(color = rgb(R.scale,G.scale,B.scale),
                                    symbol='circle',
                                    sizemode = 'diameter',size=2))  %>%
                  layout(autosize = F, width = 600, height = 600, margin = plotlyMargins,showlegend = FALSE) 

		})#output$Group_Stats_t_SNE_3Dplotly 
	       }#if(TSNE_TYPE == "SAMPLES")

            
          }#if(length(scatter2d.labels>0))
        }#if(length(Group_Stat.tSNE.Gene>0))
        
        #if(length(Group_Stat.tSNE.Gene==0))
        {

  	  if(TSNE_TYPE == "GENES")
	  {

		####################
  	    shinyjs::show(id = "Group_Stats_t_SNE_plotly_div")
  	    output$Group_Stats_t_SNE_plotly <- renderPlotly({
            	############
            	############
            		set.seed(as.numeric(input$Group_Stats_tSNE_Seed))
	            plot_ly(data=New.Data.Subset.tsne.2,x=~X, y=~Y,text=paste(rownames(New.Data.Subset.tsne.2),paste(Control.Label,Expr_Levels.1,sep=": "),paste(Treatment.Label,Expr_Levels.2,sep=": "),sep="\n"),marker = list(color = rgb(R.scale,G.scale,B.scale,sig.val.count.norm))) %>%
              layout(autosize = F, width = 600, height = 600, margin = plotlyMargins)
	
		})#output$Group_Stats_t_SNE_plotly 
	   }#if(TSNE_TYPE == "GENES")


	   if(TSNE_TYPE == "SAMPLES")
	   {
		####################
	     shinyjs::show(id = "Group_Stats_t_SNE_plotly_div")
	     output$Group_Stats_t_SNE_plotly <- renderPlotly({
		############
			 set.seed(as.numeric(input$Group_Stats_tSNE_Seed))
		     plot_ly(data=New.Data.Subset.tsne.2,x=~X, y=~Y,text=paste(rownames(New.Data.Subset.tsne.2)),marker = list(color = rgb(R.scale,G.scale,B.scale))) %>%
        	      layout(autosize = F, width = 600, height = 600, margin = plotlyMargins)

		})#output$Group_Stats_t_SNE_plotly 
	   }#if(TSNE_TYPE == "SAMPLES")

        }#if(length(Group_Stat.tSNE.Gene==0))
        
        if(length(Group_Stat.tSNE.Gene>0))
        {
          
          Group_Stat.Gene.Matches = match(Group_Stat.tSNE.Gene,rownames(New.Data.Subset.tsne.2))
          scatter2d.labels = rownames(New.Data.Subset.tsne.2)[Group_Stat.Gene.Matches[!is.na(Group_Stat.Gene.Matches)]]
          scatter2d.labels.coords = New.Data.Subset.tsne.2[Group_Stat.Gene.Matches[!is.na(Group_Stat.Gene.Matches)],]
          if(length(Group_Stat.Gene.Matches[!is.na(Group_Stat.Gene.Matches)]) >= 1)
          {
	      if(TSNE_TYPE == "GENES")
	      {
		#########################
	        shinyjs::show(id = "Group_Stats_t_SNE_plotly_div")
	        output$Group_Stats_t_SNE_plotly <- renderPlotly({
		##########
				set.seed(as.numeric(input$Group_Stats_tSNE_Seed))
              	plot_ly(data=New.Data.Subset.tsne.2,x=~X, y=~Y,text=paste(rownames(New.Data.Subset.tsne.2),paste(Control.Label,Expr_Levels.1,sep=": "),paste(Treatment.Label,Expr_Levels.2,sep=": "),sep="\n"),marker = list(color = rgb(R.scale,G.scale,B.scale,sig.val.count.norm)))  %>%
                   layout(autosize = F, width = 600, height = 600, margin = plotlyMargins ,showlegend = FALSE) %>% 
                   add_text(scatter2d.labels.coords[,1],scatter2d.labels.coords[,2],text = scatter2d.labels, textposition = "bottom") %>%
                   add_markers() 

		})#output$Group_Stats_t_SNE_plotly 
	       }#if(TSNE_TYPE == "GENES")

              if(TSNE_TYPE == "SAMPLES")
              {
                #########################
                shinyjs::show(id = "Group_Stats_t_SNE_plotly_div")
                output$Group_Stats_t_SNE_plotly <- renderPlotly({
                ##########
				set.seed(as.numeric(input$Group_Stats_tSNE_Seed))
                plot_ly(data=New.Data.Subset.tsne.2,x=~X, y=~Y,text=paste(rownames(New.Data.Subset.tsne.2)),marker = list(color = rgb(R.scale,G.scale,B.scale)))  %>%
                   layout(autosize = F, width = 600, height = 600, margin = plotlyMargins ,showlegend = FALSE)
		})#output$Group_Stats_t_SNE_plotly

               }#if(TSNE_TYPE == "SAMPLES")


            
          }#if(length(scatter2d.labels>0))
        }#if(length(Group_Stat.tSNE.Gene>0))
        
  
      ##########################################################
        progress$inc(0.40, detail = "Generating Lists!")
        Sys.sleep(0.001)
      #########################################################
      
        List1_2D=c()
        List2_2D=c()
        pair.pval_2D=c()
        List1_3D=c()
        List2_3D=c()
        pair.pval_3D=c()
      

       if(input$Generate_tSNE_List == TRUE)
       { 
        New.Data.Subset.tsne.2.names = rownames(New.Data.Subset.tsne.2)
        New.Data.Subset.tsne.3.names = rownames(New.Data.Subset.tsne.3)
        for(k3 in 1: nrow(D1.matrix2D))
        {
      
          SIG.count2D = length(which(D1.matrix2D[k3,-c(1:k3)] <= alfa2D))
          SIG.count3D = length(which(D1.matrix3D[k3,-c(1:k3)] <= alfa3D))
    
          if(SIG.count2D > 0)
          {
    
            sig.inx = which(D1.matrix2D[k3,] <= alfa2D)
            sig.inx = sig.inx[sig.inx>k3]
            List1_2D = c(List1_2D,rep(New.Data.Subset.tsne.2.names[k3],SIG.count2D))
            List2_2D = c(List2_2D,New.Data.Subset.tsne.2.names[sig.inx])
                 
            pair.pval_2D = c(pair.pval_2D,D1.matrix.2Dpvals[k3,sig.inx])
          }#if(SIG.count2D > 0)
          
          if(SIG.count3D > 0)
          {

            sig.inx = which(D1.matrix3D[k3,] <= alfa3D)
            sig.inx = sig.inx[sig.inx>k3]
           
            List1_3D = c(List1_3D,rep(New.Data.Subset.tsne.3.names[k3],SIG.count3D))
            List2_3D = c(List2_3D,New.Data.Subset.tsne.3.names[sig.inx])

            pair.pval_3D = c(pair.pval_3D,D1.matrix.3Dpvals[k3,sig.inx])
          }#if(SIG.count3D > 0)
          
        }#for(k3 in 1: nrow(D1.matrix2D))
        
        
        if(length(List1_2D)>0)
        {
          t_SNE_List2D = data.frame(List1_2D,List2_2D,pair.pval_2D)
          t_SNE_List.sorted2D = t_SNE_List2D[order(t_SNE_List2D$pair.pval_2D,decreasing=F),]
          t_SNE_2DdataContent = "t-SNE 2D Gene List\n"

	  t_SNE_2DdataContent = apply(t_SNE_3DdataContent,MARGIN=1, FUN=function(X)
          #for(k4 in 1:nrow(t_SNE_List.sorted2D))
          {
            #t_SNE_2DdataContent = paste(t_SNE_2DdataContent,paste(unlist(as.matrix(t_SNE_List.sorted2D[k4,])),collapse="\t"),sep = "\n")
	     paste(unlist(as.matrix(X)),collapse="\t")
          }#for(k4 in 1:nrow(t_SNE_List.sorted2D))
	  ); t_SNE_2DdataContent = paste(t_SNE_2DdataContent,collapse="\n")

          updateTextAreaInput(session, inputId = "Group_Stats_t_SNE_2DGeneList",value = t_SNE_2DdataContent)
        }#if(length(List1_2D)>0)
        
        if(length(List1_3D)>0)
        {
          t_SNE_List3D = data.frame(List1_3D,List2_3D,pair.pval_3D)
          t_SNE_List.sorted3D = t_SNE_List3D[order(t_SNE_List3D$pair.pval_3D,decreasing=F),]
          t_SNE_3DdataContent = "t-SNE 3D Gene List\n"

	  t_SNE_3DdataContent = apply(t_SNE_3DdataContent,MARGIN=1, FUN=function(X)
          #for(k4 in 1:nrow(t_SNE_List.sorted3D))
          {
            #t_SNE_3DdataContent = paste(t_SNE_3DdataContent,paste(unlist(as.matrix(t_SNE_List.sorted3D[k4,])),collapse="\t"),sep = "\n")
	     paste(unlist(as.matrix(X)),collapse="\t")
          }#for(k4 in 1:nrow(t_SNE_List.sorted3D))
	  ); t_SNE_3DdataContent = paste(t_SNE_3DdataContent,collapse="\n")

          updateTextAreaInput(session, inputId = "Group_Stats_t_SNE_3DGeneList",value = t_SNE_3DdataContent)
        }#if(length(List1_3D)>0)
  
        
        input_Group_Stats_tSNE_pointSize = input$Group_Stats_tSNE_pointSize

	if(exists("New.Data.Subset.tsne.3") & exists(R.scale))
	{
		save(list = c("New.Data.Subset.tsne","R.scale","G.scale","B.scale","plotlyMargins","sig.val.count.norm","input_Group_Stats_tSNE_pointSize"), file=paste(Backup_Session_Folder,"/tsne.3d.rdata",sep=""))  
	}## if(exists("New.Data.Subset.tsne.3") & exists(R.scale))

        if(exists("New.Data.Subset.tsne.2") & exists(R.scale))
        {
                save(list = c("New.Data.Subset.tsne","R.scale","G.scale","B.scale","plotlyMargins","input_Group_Stats_tSNE_pointSize"), file=paste(Backup_Session_Folder,"/tsne.2d.rdata",sep=""))
        }## if(exists("New.Data.Subset.tsne.3") & exists(R.scale))


	if(exists("New.Data.Subset.tsne.3.Groups"))
        {
                save(list = c("New.Data.Subset.tsne.3.Groups","plotlyMargins","input_Group_Stats_tSNE_pointSize"), file=paste(Backup_Session_Folder,"/tsne.3d.rdata",sep="")) 
        }## if(exists("New.Data.Subset.tsne.3.Groups"))

       }#if(GENERATE_LIST_Check)

 
      }#if(length(Control.Treatment.data.5.inx)>=2)
    }##if(nrow(DATA.Values.5min)>10)
  ##########################################################
    progress$inc(0.999, detail = "Something Happened!")
    Sys.sleep(0.001)
  #########################################################
    
  })#observeEvent(input$Group_Stats_Run_tSNE
  ######################################
  ######################################
  
  #####################################
  #####################################  
  observeEvent(input$Group_Stat_analysis_button,ignoreInit = TRUE,{
    #####################################
    #####################################  
    #####################
    
    if(length(DATA.Values.5min) <= 1)
	{	
		shinyalert(title = "Data Error",text = "Please load data into the tool and create groups", type = "warning")
		return(NULL)
	}
    
    ######################
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Comparing Gene Expressions", value = 0)
    # Increment the progress bar, and update the detail text.
    progress$inc(0.3, detail = "Something is Happening!")
    Sys.sleep(0.001)
    ######################
    
    Group_Stat.Compare.1 <- isolate(input$Group1_Stat_compare)
    Group_Stat.Compare.2 <- isolate(input$Group2_Stat_compare)
    Group_Stat.RPKM.Threshold <- isolate(input$Group_Stat_RPKM_Thresh)
    Group_Stat.RPKM.FC <- isolate(input$Group_Stat_FC_Thresh)
    Group_Stat.Top.Graphs <- isolate(input$Group_Stat_Display_Top_Graphs)
    Group_Stat.Pval <- isolate(input$Group_Stat_PVal_Thresh)
    
  
    if(nchar(Group_Stat.Compare.1)>=1 & nchar(Group_Stat.Compare.2)>=1)
    {
      Group.Index=c()
      Gene.Index=c()

      Group.Index[1] = which(Groups == Group_Stat.Compare.1)
      Group.Index[2] = which(Groups == Group_Stat.Compare.2)
      
      Group.Member.index.1 = match(unlist(strsplit(Group.Members[Group.Index[1]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))
      Group.Member.index.2 = match(unlist(strsplit(Group.Members[Group.Index[2]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))
      
      
      if(length(Group.Member.index.1) <= 1 | length(Group.Member.index.2) <= 1 )
      {
        shinyalert(title = "Group Size Error", text="Select groups with at least 2 samples", type = "warning")
      	return(NULL)
      }#if(length(Group.Member.index.1) <= 1 | Group.Member.index.2 <=1 )

      Control.data = DATA.Values.5min[,Group.Member.index.1]
      Treatment.data = DATA.Values.5min[,Group.Member.index.2]
      
      Control.Label = Group_Stat.Compare.1
      Control.Label.bp = Group_Stat.Compare.1
      
      Treatment.Label.bp = Group_Stat.Compare.2
      Treatment.Label = Group_Stat.Compare.2
      
      Control.data.5.inx = which(apply(Control.data, MARGIN = 1, function(x) median(x) >= Group_Stat.RPKM.Threshold))
      Treatment.data.5.inx = which(apply(Treatment.data, MARGIN = 1, function(x) median(x) >= Group_Stat.RPKM.Threshold))
      Combined.inx = unique(c(Control.data.5.inx,Treatment.data.5.inx))
      
      Gene_Names = rownames(DATA.Values.5min)[Combined.inx]
      Control.data = Control.data[Combined.inx,]
      Treatment.data = Treatment.data[Combined.inx,]
      
      
      
      plotlyMargins <- list(
        l = 50,
        r = 50,
        b = 100,
        t = 100,
        pad = 4
      )
      MIN.dim = min(log2(apply(Control.data, MARGIN = 1, function(x) median(x))),log2(apply(Treatment.data, MARGIN = 1, function(x) median(x))))
      MAX.dim = max(log2(apply(Control.data, MARGIN = 1, function(x) median(x))),log2(apply(Treatment.data, MARGIN = 1, function(x) median(x))))
     
      Group_Stats_X = log2(apply(Control.data, MARGIN = 1, function(x) median(x)))
      Group_Stats_Y = log2(apply(Treatment.data, MARGIN = 1, function(x) median(x))) 
      Group_Stats_Text = Gene_Names
      Group_Stats_Margins = plotlyMargins
      Group_Stats_xTitle = paste(Control.Label,"log2(Expression)")
      Group_Stats_yTitle = paste(Treatment.Label,"log2(Expression)")
      Group_Stats_MIN.dim = MIN.dim
      Group_Stats_MAX.dim = MAX.dim

      shinyjs::show(id = "Group_Stats_log2RPKM_div")
      output$Group_Stats_log2RPKM <- renderPlotly({
        plot_ly(x=Group_Stats_X,y=Group_Stats_Y,text = Group_Stats_Text) %>%
          layout(autosize = F, width = 600, height = 600, margin = Group_Stats_Margins ,showlegend = FALSE,xaxis = list(title = Group_Stats_xTitle, range=c(Group_Stats_MIN.dim,Group_Stats_MAX.dim)),yaxis = list(title = Group_Stats_yTitle,range=c(Group_Stats_MIN.dim,Group_Stats_MAX.dim)))
      
      })#output$Group_Stats_log2RPKM <- renderPlotly({
      
      save(list = c("Group_Stats_X","Group_Stats_Y","Group_Stats_Text","Group_Stats_Margins","Group_Stats_xTitle","Group_Stats_yTitle","Group_Stats_MIN.dim","Group_Stats_MAX.dim"), file=paste(Backup_Session_Folder,"/Group_Stats_log2RPKM.rdata",sep=""))  

       
      Pval.Set.Scores = matrix(nrow=nrow(Control.data),ncol=4)
      NORM.score = c()
      Med.fc.score = c()
      for(g in 1:nrow(Control.data))
      {
        plot.coords = matrix(ncol = 2, nrow=(ncol(Control.data) *ncol(Treatment.data)))
        h=0
        for(j in 1:ncol(Control.data))
        {
          for(k in 1:ncol(Treatment.data))
          {
            h=h+1
            plot.coords[h,1] = Treatment.data[g,k]
            plot.coords[h,2] = Control.data[g,j]
          }#for(k in 1:ncol(Treatment.data))
        }#for(j in 1:ncol(Control.data))
           
        plot.coords.norm = (plot.coords-min(plot.coords))/(max(plot.coords)-min(plot.coords))
        
        
        NORM.score[g] = abs((mean(plot.coords.norm[,1]-plot.coords.norm[,2])+.1)/(sd(plot.coords.norm[,1]-plot.coords.norm[,2])+.1))
        Med.fc.score[g] = abs(log2((median(unlist(Treatment.data[g,]))+1)/(median(unlist(Control.data[g,]))+1)))
        
        Pval.Set.Scores[g,3] = wilcox.test(unlist(Treatment.data[g,]),unlist(Control.data[g,]))$p.val
        Pval.Set.Scores[g,4] = t.test(unlist(Treatment.data[g,]),unlist(Control.data[g,]))$p.val
        
      }#for(g in 1:nrow(Control.data))
      

      NORM.score.zval = (NORM.score-mean(NORM.score))/sd(NORM.score)
      Med.fc.score.zval = (Med.fc.score-mean(Med.fc.score))/sd(Med.fc.score)

      
      NORM.score.pval <- pnorm(NORM.score.zval)
  
      Med.fc.score.pval <- pnorm(Med.fc.score.zval)
      
      Med.fc.score.thresh.inx = which(Med.fc.score < as.numeric(as.matrix(Group_Stat.RPKM.FC)))
      
      Med.fc.score.pval[Med.fc.score.thresh.inx] = 0    #Set Fold changes below FCthreshold to 0 pval  
      
      Pval.Set.Scores[,1] = 1-Med.fc.score.pval  #Right tail pvalue
      Pval.Set.Scores[,2] = 1-NORM.score.pval    #Right tail pvalue
      
      
        
      Pval.inx = c()
      if(any(input$Stat_Test_Select=="Fold Change")){Pval.inx = c(Pval.inx,1)}
      if(any(input$Stat_Test_Select=="Mead/SD")){Pval.inx = c(Pval.inx,2)}
      if(any(input$Stat_Test_Select=="Wilcox")){Pval.inx = c(Pval.inx,3)}
      if(any(input$Stat_Test_Select=="Ttest")){Pval.inx = c(Pval.inx,4)}
      
  
      if(length(Pval.inx) >= 2)
      {
        g.indx = which(apply(Pval.Set.Scores[,Pval.inx],MARGIN = 1,function(x) all(x<=Group_Stat.Pval)))
        g.indx = g.indx[order(apply(Pval.Set.Scores[g.indx,Pval.inx],MARGIN=1,function(x) sum(x)),decreasing = F)]
      }
      if(length(Pval.inx) == 1)
      {
        g.indx = which(Pval.Set.Scores[,Pval.inx] <= Group_Stat.Pval)
        g.indx = g.indx[order(Pval.Set.Scores[g.indx,Pval.inx],decreasing = F)]
      }
      if(length(Pval.inx) == 0)
      {
        g.indx = c()
      }
       
      if(length(g.indx)>=1)
      {
        
        #########################
        ########################
        shinyjs::show(id = "Group_Stats_Scatter_Boxplot_div")
        output$Group_Stats_Scatter_Boxplot <- renderPlot({
          ###################
          PLot_Count = min(as.numeric(as.matrix(Group_Stat.Top.Graphs)),length(g.indx),20)
          par(mfrow = c(PLot_Count,2))
          if(PLot_Count >= 10){par(mfrow = c(ceiling(PLot_Count/2),4))}
          #for(k in 1:length(g.indx))
          for(k in 1:PLot_Count)
          {
            g=g.indx[k]
            plot.coords = matrix(ncol = 2, nrow=(ncol(Control.data) *ncol(Treatment.data)))
            h=0
            for(j in 1:ncol(Control.data))
            {
              for(k in 1:ncol(Treatment.data))
              {
                h=h+1
                plot.coords[h,1] = Control.data[g,j]
                plot.coords[h,2] = Treatment.data[g,k]
              }#for(k in 1:ncol(Treatment.data))
            }#for(j in 1:ncol(Control.data))
            
            GSSP_Gene_Names = Gene_Names[g]
            GSSP_Control.Label = Control.Label
            GSSP_Treatment.Label = Treatment.Label
            GSSP_Control.data = Control.data[g,]
            GSSP_Treatment.data = Treatment.data[g,]
            GSSP_Control.Label.bp = Control.Label.bp
            GSSP_Treatment.Label.bp = Treatment.Label.bp
            GSSP_PLot_Count = PLot_Count 
            GSSP_plot.coords = plot.coords
            
            save(list = c("GSSP_plot.coords","GSSP_Gene_Names","GSSP_Control.Label","GSSP_Treatment.Label","GSSP_Control.data","GSSP_Treatment.data","GSSP_Control.Label.bp","GSSP_Treatment.Label.bp","GSSP_PLot_Count"), file=paste(Backup_Session_Folder,"/Group_Stats_Scatter_Boxplot.rdata",sep=""))  
            
            plot(plot.coords,xlim=c(min(plot.coords),max(plot.coords)),ylim=c(min(plot.coords),max(plot.coords)),pch=16,cex=0.75,main=Gene_Names[g],xlab = Control.Label,ylab = Treatment.Label,cex.lab=0.7)
            lines(rbind(c((min(plot.coords)-5),(min(plot.coords)-5)),c((max(plot.coords)+5),(max(plot.coords)+5))),lty=2)
            boxplot(unlist(Control.data[g,]),unlist(Treatment.data[g,]), main=Gene_Names[g],names=c(Control.Label.bp,Treatment.Label.bp),las=2,ylab="Expression",cex.axis=0.7)
          
            plot.coords.norm = (plot.coords-min(plot.coords))/(max(plot.coords)-min(plot.coords))
            
          }#for(k in 1:length(g.indx))
          par(mfrow = c(1,1))
        })#output$Group_Stats_Scatter_Boxplot <- renderPlot({
        #########################
        ########################
        
        ######################
        progress$inc(0.7, detail = "Something is STILL Happening!")
        Sys.sleep(0.001)
        ######################
        
        Gene_Names.table = data.frame(as.character(Gene_Names[g.indx]),log2(as.numeric(apply(Treatment.data[g.indx,], MARGIN = 1, function(x) median(x))+1)/as.numeric(apply(Control.data[g.indx,], MARGIN = 1, function(x) median(x))+1)))
        colnames(Gene_Names.table) = c("Gene_Symbol","Log2FC")
        
        #########################
        ########################
        if(input$Group_Stat_List_FC==TRUE)
        {
          GeneList.Table.content = paste(Gene_Names.table[,1],"\t\t",Gene_Names.table[,2],"\n",sep="",collapse="")
          updateTextAreaInput(session, inputId = "Group_Stats_Gene_FC_List",label = "Top scoring genes with corresponding fold change",value = GeneList.Table.content)
        }
        if(input$Group_Stat_List_FC==FALSE)
        {
          GeneList.Table.content = paste(Gene_Names.table[,1],"\n",sep="",collapse="")
          updateTextAreaInput(session, inputId = "Group_Stats_Gene_FC_List",label = "Top scoring genes with corresponding fold change",value = GeneList.Table.content)
        }
        #########################
        ########################
        
 
        if(nrow(Gene_Names.table)>0) lab.size = 0.5
        if(nrow(Gene_Names.table)>50) lab.size = 0.35
        if(nrow(Gene_Names.table)>100) lab.size = 0.25
        if(nrow(Gene_Names.table)>150) lab.size = 0.2
        
        
        #########################
        ########################
        shinyjs::show(id = "Group_Stat_Gene_FC_div")
        output$Group_Stat_Gene_FC <- renderPlot({
          ###################
          barplot(abs(Gene_Names.table[,2]),names=Gene_Names.table[,1],las=2,cex.names=lab.size,ylab="abs(Log2 FC)",col=c("grey","blue"))
        })#output$Group_Stat_Gene_FC <- renderPlot({
        #########################
        ########################
        
        Group_Stat_Gene_FC_table = Gene_Names.table
        GSFC_lab.size = lab.size
        
        save(list = c("Group_Stat_Gene_FC_table","GSFC_lab.size"), file=paste(Backup_Session_Folder,"/Group_Stat_Gene_FC.rdata",sep=""))  
        
        #dev.off()
      }#if(length(g.indx)>=1)
      
      if(length(g.indx)==0)
      {
        output$Group_Stats_Scatter_Boxplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
        output$Group_Stat_Gene_FC <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
        updateTextAreaInput(session, inputId = "Group_Stats_Gene_FC_List",label = "Top scoring genes with corresponding fold change",value = "",placeholder = "No Genes Met the Criteria")
      
      }#if(length(g.indx)==0)
      
      #####################################
      #####################################
      ############################
      output$Group_Stat_Download_Graphs <- downloadHandler(
        ###################
        filename = function() {
          paste("Data_Download", ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file)

          if(length(g.indx)>=1)
          {
            par(mfrow = c(3,2))
            for(k in 1:length(g.indx))
            {
              g=g.indx[k]
              plot.coords = matrix(ncol = 2, nrow=(ncol(Control.data) *ncol(Treatment.data)))
              h=0
              for(j in 1:ncol(Control.data))
              {
                for(k in 1:ncol(Treatment.data))
                {
                  h=h+1
                  plot.coords[h,1] = Control.data[g,j]
                  plot.coords[h,2] = Treatment.data[g,k]
                }#for(k in 1:ncol(Treatment.data))
              }#for(j in 1:ncol(Control.data))
              plot(plot.coords,xlim=c(min(plot.coords),max(plot.coords)),ylim=c(min(plot.coords),max(plot.coords)),pch=16,cex=0.75,main=Gene_Names[g],xlab = Control.Label,ylab = Treatment.Label,cex.lab=0.7)
              lines(rbind(c((min(plot.coords)-5),(min(plot.coords)-5)),c((max(plot.coords)+5),(max(plot.coords)+5))),lty=2)
              boxplot(unlist(Control.data[g,]),unlist(Treatment.data[g,]), main=Gene_Names[g],names=c(Control.Label.bp,Treatment.Label.bp),las=2,ylab="Expression",cex.axis=0.7)
        
             }#for(k in 1:length(g.indx))
            
            par(mfrow = c(1,1))
            
            Gene_Names.table = data.frame(as.character(Gene_Names[g.indx]),log2(as.numeric(apply(Treatment.data[g.indx,], MARGIN = 1, function(x) median(x))+1)/as.numeric(apply(Control.data[g.indx,], MARGIN = 1, function(x) median(x))+1)))
            colnames(Gene_Names.table) = c("Gene_Symbol","Log2FC")
            
            page.count = ceiling(length(g.indx)/30)
            for(P in 0:(page.count-1))
            {
              plot(NA, xlim=c(0,2), ylim=c(0,3), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
              mytheme <- gridExtra::ttheme_default(
                core = list(fg_params=list(cex = .4)),
                colhead = list(fg_params=list(cex = .4)),
                rowhead = list(fg_params=list(cex = .4)))
              
              grid.table(Gene_Names.table[(P*30+1):min(nrow(Gene_Names.table),((P+1)*30)),],theme = mytheme)
            }
            if(nrow(Gene_Names.table)>0) lab.size = 0.5
            if(nrow(Gene_Names.table)>50) lab.size = 0.35
            if(nrow(Gene_Names.table)>100) lab.size = 0.25
            if(nrow(Gene_Names.table)>150) lab.size = 0.2
            barplot(abs(Gene_Names.table[,2]),names=Gene_Names.table[,1],las=2,cex.names=lab.size,ylab="abs(Log2 FC)",col=c("grey","blue"))
            
            
          }#if(length(g.indx)>=1)
          
          if(length(g.indx)==0)
          {
            plot(0,cex=0,axes=F,xlab="",ylab="")
            plot(0,cex=0,axes=F,xlab="",ylab="")
            
          }#if(length(g.indx)==0)
          dev.off()
        })#output$Group_Stat_Download_Graphs <- downloadHandler(
      #########################
      #########################
      ########################
      
      
      
      
      
      
      
    }#if(length(Group_Stat.Compare.1)>=1 & length(Group_Stat.Compare.2)>=1)     
    
    
    ######################
    progress$inc(0.99, detail = "Something Happened!")
    Sys.sleep(0.1)
    ######################
  })#observeEvent(input$Group_Stat_analysis_button,{
  
  #####################################
  #####################################   
   
   
   
   ##################################################### NEW  Stuffff ##################################
   
   
     ###########################  
  #####################################
  #####################################
  observeEvent(input$DEO_GO_Group_v_Sample,ignoreInit = TRUE,{
    
    #"Groups","Sample"
    if(input$DEO_GO_Group_v_Sample == "Samples" & length(DATA.Values.5min)>1)
    {
      updateSelectizeInput(session, inputId = "DEO_Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
      updateSelectizeInput(session, inputId = "DEO_Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = NULL)
    }#if(input$DEO_GO_Group_v_Sample == "Samples")
    if(input$DEO_GO_Group_v_Sample == "Groups" & exists("Groups"))
    {
      
      if(Group_Count>0)
      {
        updateSelectizeInput(session, inputId = "DEO_Control_GO_Test",label = "Select Control",choices = Groups, selected = NULL)
        updateSelectizeInput(session, inputId = "DEO_Subjects_GO_Test",label = "Select Subjects",choices = Groups, selected = NULL)
      }#if(Group_Count>0)
      if(Group_Count==0)
      {
        updateSelectizeInput(session, inputId = "DEO_Control_GO_Test",label = "Select Control",choices = NULL, selected = NULL)
        updateSelectizeInput(session, inputId = "DEO_Subjects_GO_Test",label = "Select Subjects",choices = NULL, selected = NULL)
      }#if(Group_Count==0)
      
    }#if(input$DEO_GO_Group_v_Sample == "Groups" & exists("Groups"))
    
  })#observeEvent(input$DEO_GO_Group_v_Sample,{
  #####################################
  #####################################

   
   #input$DEO_GO_Analysis
  #####################################
  observeEvent(input$DEO_GO_Analysis,ignoreInit = TRUE,{  
    #updateSliderInput(session = session,inputId = "GO_Text_FC_limit",label = "Label absFC Threshold",min = 0,max = 0,value = 0)
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Determining Gene Ontology", value = 0)
    # Increment the progress bar, and update the detail text.
    ###############################################         
    progress$inc(0.1, detail = paste("Progress: ",10,"%",sep=""))
    Sys.sleep(0.001)
    ###############################################  
######
    #######
    ########

    if(!exists("GO.info"))
    {
           Existing_Objects = ls()
           load("Data/ROGUE_GO_Workspace.RData")
           New_Objects = setdiff(ls(), Existing_Objects)
           Make_Global_Object_Command = paste(New_Objects,"<<-", New_Objects)

           for(Command in Make_Global_Object_Command)
           {
                eval(expr = parse(text = Command))
           }#for(Command in Make_Global_Object_Command)

    }#if(!exists("GO.info"))



Fpkm.table.New.txt = c()

	if(length(DATA.Values.5min) <= 1)
	{
		shinyalert(title = "Data Error",text = "Please load data into the tool", type = "warning")

		return(NULL)
	}


    if(exists("DATA.Values.5min"))
    { 
	if(!is.null(DATA.Values.5min))
	{    
 	   if(input$DEO_GO_Group_v_Sample == "Samples"){Fpkm.table.New.txt = DATA.Values.5min}
	}#if(!missing("DATA.Values.5min"))
    }#if(exists(DATA.Values.5min))


     if(exists("Groups.Medians.Matrix"))
     { 
         if(!is.null(Groups.Medians.Matrix))
         { 
	    if(input$DEO_GO_Group_v_Sample == "Groups"){Fpkm.table.New.txt = Groups.Medians.Matrix}
	 }#if(!is.null(Groups.Medians.Matrix))
     }#if(exists(Groups.Medians.Matrix))


    
    Control.fpkm = input$DEO_Control_GO_Test
    Control.fpkm.inx = which(colnames(Fpkm.table.New.txt)==Control.fpkm)
    Test.fpkm = input$DEO_Subjects_GO_Test
    Test.fpkm.inx = which(colnames(Fpkm.table.New.txt) %in% Test.fpkm)
    GO_Source = substr(input$Ontology_Method_2Compare,1,3)
    GO_Gene_Lists = c()
    RPKM.threshold = input$DEO_GO_Min_RPKM
########
    #######
    #######
 
	
        DEO_GO_ID_info = GO.info[which(GO.info[,2] == input$DEO_GO_CLASS),1]
        DEO_GO_ID_List = as.character(GO.info[which(GO.info[,1] %in% unlist(strsplit(GO.EXT.hierarchy[grep(paste("^",DEO_GO_ID_info,sep=""),GO.EXT.hierarchy)]," "))),1])
   
  
        Keywords_part1 = as.character(input$DEO_Ontology_Keyword_Select)
   	#Keywords_part2 = unlist(strsplit(as.character(input$DEO_Keyword_Text), split = ";"))
   	
   	#Keywords = c(Keywords_part1,Keywords_part2)
	Keywords = Keywords_part1
   	Keywords = Keywords[which(nchar(Keywords) >= 1)]
  
  	GO_Shortlist = unlist(sapply( Keywords, function(X) as.character(unique(GO.info$GO.IDs[grep(X,GO.info$GO.Names)]))))
	  
	GO_Shortlist = c(GO_Shortlist,DEO_GO_ID_List)

progress$inc(0.2, detail = paste("Progress: ",20,"%",sep=""))
    Sys.sleep(0.001)

  
  	if(length(Test.fpkm)==0 | length(Control.fpkm)==0 | length(GO_Shortlist)==0)
  	{
  		shinyalert(title = "No Hits",text = "Please Try different groups or a broader range/class of Gene Ontologies", type = "warning")
		return(NULL)
  		
  	}
  
  	if(length(Test.fpkm)>0 & length(Control.fpkm)>0 & length(GO_Shortlist)>0)
    {
    
  
  		Gene_Set_Compare  = data.frame(rownames(Fpkm.table.New.txt),Fpkm.table.New.txt[,c(Control.fpkm.inx, Test.fpkm.inx)])
  		colnames(Gene_Set_Compare) = c("Gene", Control.fpkm, Test.fpkm)
  		
  		

  
  		GO.Pval.analysis.Pathway = sapply(GO_Shortlist, function(X){
	
  			#Gene_Set_Compare = Immune_Cells.RPKM.CD4.HCvSepsis
  			Pvals = 1
  			log2FC = 0
  			Mean_Base = 0
  			if(length(which(Gene_Set_Compare[,1] %in% GO_Gene_Table$V1[which(as.character(GO_Gene_Table$V2)==as.character(X))]))>=1)
  			{
    			Pvals = wilcox.test(
    			Gene_Set_Compare[which(Gene_Set_Compare[,1] %in% GO_Gene_Table$V1[which(as.character(GO_Gene_Table$V2)==as.character(X))]),2],
    			Gene_Set_Compare[which(Gene_Set_Compare[,1] %in% GO_Gene_Table$V1[which(as.character(GO_Gene_Table$V2)==as.character(X))]),3]
   		 	)$p.value 
   		 	log2FC = log2((median(unlist(Gene_Set_Compare[which(Gene_Set_Compare[,1] %in% GO_Gene_Table$V1[which(as.character(GO_Gene_Table$V2)==as.character(X))]),3]))+1)/(median(unlist(Gene_Set_Compare[which(Gene_Set_Compare[,1] %in% GO_Gene_Table$V1[which(as.character(GO_Gene_Table$V2)==as.character(X))]),2]))+1))
    	 	Mean_Base = mean(c(median(unlist(Gene_Set_Compare[which(Gene_Set_Compare[,1] %in% GO_Gene_Table$V1[which(as.character(GO_Gene_Table$V2)==as.character(X))]),3])),median(unlist(Gene_Set_Compare[which(Gene_Set_Compare[,1] %in% GO_Gene_Table$V1[which(as.character(GO_Gene_Table$V2)==as.character(X))]),2]))))
  			}
  			return(c(Pvals,log2FC,Mean_Base))
  		
		})# GO.Pval.analysis.Pathway = sapply(GO_Shortlist, function(X){



		progress$inc(0.5, detail = paste("Progress: ",30,"%",sep=""))
    	Sys.sleep(0.001)


    	GO.Pval.analysis.Pathway[,which(GO.Pval.analysis.Pathway[1,]<=0.2)]


		Pval_Thresh = input$DEO_PvalThresh
		absFC_Thresh = input$DEO_absFCThresh
		Base_Thresh = input$DEO_BaseThresh
		
		GO.Pval.analysis.Pathway.Check = which(apply(GO.Pval.analysis.Pathway, MARGIN=2, function(X) {X[1]<= Pval_Thresh & abs(X[2])>= absFC_Thresh & X[3]>= Base_Thresh}))
		HITS = GO.info$GO.Names[which(GO.info$GO.IDs %in% GO_Shortlist[GO.Pval.analysis.Pathway.Check])]

    
    
      	if(length(HITS) == 0)
  		{
  			shinyalert(title = "No Hits within threshold criteria",text = "Please Try a lower 'Min log2 Fold Change' and a higher 'Max p-value'", type = "warning")
			return(NULL)		
  		}
    
    
    	GO.Pval.analysis.Pathway.Hits.GTable = c()
		if(length(HITS) > 0)
		{
			for(HIT in HITS)
			{
			  	GO.Pval.analysis.Pathway.Check = Gene_Set_Compare[which(Gene_Set_Compare[,1] %in% GO_Gene_Table$V1[which(GO_Gene_Table$V2==as.character(GO.info$GO.IDs[which(GO.info$GO.Names == HIT)]))]),1:3]
  				GO.Pval.analysis.Pathway.Hits.GTable = rbind(GO.Pval.analysis.Pathway.Hits.GTable,data.frame(gene_symbol = GO.Pval.analysis.Pathway.Check[,1],Sample = colnames(GO.Pval.analysis.Pathway.Check)[2], Expression = GO.Pval.analysis.Pathway.Check[,2],GO = HIT),
        	                           data.frame(gene_symbol = GO.Pval.analysis.Pathway.Check[,1],Sample = colnames(GO.Pval.analysis.Pathway.Check)[3], Expression = GO.Pval.analysis.Pathway.Check[,3],GO= HIT))                                             
			}#for(A in HITS[5])

	

				DEO_GO_Box_ggp = ggplot(GO.Pval.analysis.Pathway.Hits.GTable, aes(x=gsub(" ","\n",GO),y=Expression,fill=Sample)) + 
  				geom_boxplot()+
  				theme_classic()+
  				theme(axis.text.x = element_text(angle = 0))+
  				theme(text = element_text(size=as.numeric(input$DEO_GO.Font_Size)))+
  				theme(axis.title.x = element_blank())+
  				scale_fill_manual(values=c("firebrick2","dodgerblue4"))

				Report.List.Reads <<- c(Report.List.Reads,list(GGP))
  			
				shinyjs::show(id = "DEO_GO_Boxplot_div")
			  output$DEO_GO_Boxplot <- renderPlotly({
	
  				ggplotly(DEO_GO_Box_ggp) %>% layout(boxmode = "group")

			  })#output$DEO_GO_Boxplot
		
			save(list = c("DEO_GO_Box_ggp"), file=paste(Backup_Session_Folder,"/DEO_GO_Boxplot.rdata",sep=""))  
		
		progress$inc(0.8, detail = paste("Progress: ",30,"%",sep=""))
    	Sys.sleep(0.001)
		
    	shinyjs::show(id = "DEO_GO_Distribution_div")
			output$DEO_GO_Distribution <- renderPlot({
				
				unique.GOs = unique(GO.Pval.analysis.Pathway.Hits.GTable$GO)

				GList=list()
				for(GO.select in 1:length(unique.GOs))
				{
		  			GO.Pval.analysis.Pathway.Hits.GTable.sub = subset(GO.Pval.analysis.Pathway.Hits.GTable,( GO == unique.GOs[GO.select]))
  
  					Range.20pct = 0.8*(max(GO.Pval.analysis.Pathway.Hits.GTable.sub$Expression)-min(GO.Pval.analysis.Pathway.Hits.GTable.sub$Expression))

      				GO.plot = ggplot(GO.Pval.analysis.Pathway.Hits.GTable.sub, aes(x=Expression,fill=Sample)) + 
      				geom_density(alpha=0.7)+
      				theme_classic()+
      				theme(text = element_text(size=as.numeric(input$DEO_GO.Font_Size)))+
      				xlim(c(
        				min(GO.Pval.analysis.Pathway.Hits.GTable.sub$Expression) - Range.20pct,
        				max(GO.Pval.analysis.Pathway.Hits.GTable.sub$Expression) + Range.20pct
      				))+
      				scale_fill_manual(values=c("firebrick2","dodgerblue4")) + 
        			theme(legend.position = "none")
      
 
 				     GList = c(GList,list(GO.plot))
   				}#for(GO.select in 1:length(unique.GOs))
   				
				legend1 <- cowplot::get_legend(GO.plot+theme(legend.position = "right"))
				GList = c(GList,list(legend1))

				Report.List.Reads <<- c(Report.List.Reads,GList)
				
				DEO_GO_Distribution_GList = GList
				DEO_GO_Distribution_Graph_Width = input$DEO_GO.Graph_Width
				
				save(list = c("DEO_GO_Distribution_GList","DEO_GO_Distribution_Graph_Width"), file=paste(Backup_Session_Folder,"/DEO_GO_Distribution.rdata",sep=""))  
				
				
				do.call("grid.arrange",c(GList,ncol= 5-as.numeric(input$DEO_GO.Graph_Width)))
			
			
			})#output$DEO_GO_Distribution
			
			####output$DEO_GO_Heatmap_rowScale <- renderPlotly({

				
  				
  				DEO_GO_Heatmap_rows <- plot_ly(GO.Pval.analysis.Pathway.Hits.GTable, x = ~GO, y = ~Expression, color = ~Sample, type = "box", text = ~gene_symbol,boxpoints = "all", jitter = 0.3, pointpos = 0)
				
  				shinyjs::show(id = "DEO_GO_Heatmap_rowScale_div")
  				output$DEO_GO_Heatmap_rowScale <- renderPlotly({
  				  
  				  DEO_GO_Heatmap_rows %>% layout(boxmode = "group")
  				  
  				})#output$DEO_GO_Heatmap_rowScale

			save(list = c("DEO_GO_Heatmap_rows"), file=paste(Backup_Session_Folder,"/DEO_GO_Heatmap_rowScale.rdata",sep=""))  

			
			 TEXT_OUTPUT = paste(c(paste(colnames(GO.Pval.analysis.Pathway.Hits.GTable),collapse = "\t"),apply(GO.Pval.analysis.Pathway.Hits.GTable[order(GO.Pval.analysis.Pathway.Hits.GTable$gene_symbol),],MARGIN=1,function(X) paste(X,collapse = "\t"))),collapse = "\n")

			
			 updateTextAreaInput(session, inputId = "DEO_GENE_GO_FC_RPKM",label = "Genes Related to Differentially expressed Ontologies/Pathways",value = TEXT_OUTPUT)

		}#if(length(HITS) > 0)



                                     #textAreaInput(inputId = "DEO_GENE_GO_FC_RPKM",label = "GO RPKM Table",value="",width = '800px', height='300px')

									#sliderInput(inputId = "DEO_GO.Graph_Width",label = "Graph Width",min = 1,max = 4,value = 4),
                                     #sliderInput(inputId = "DEO_GO.Font_Size",label = "Font Size",min = 1,max = 4,value = 2),

                                     #plotlyOutput("DEO_GO_Heatmap_rowScale",height='600px',width = "1200px"),
                                     #plotOutput("DEO_GO_Heatmap",height='600px',width = "1600px"),  


    }#if(length(GO.exmp)>0 & length(Test.fpkm)>0 & length(Control.fpkm)>0 & nchar(RPKM.threshold)>0))
      
      
      
      
    progress$inc(1, detail = paste("Progress: ",99,"%",sep=""))
    Sys.sleep(0.001)
    ###############################################  
  }) #observeEvent(input$DEO_GO_Analysis,{
  #####################################
   
   
   
   
        ###########################  
  #####################################
  #####################################
  observeEvent(input$GSEA_Group_v_Sample,ignoreInit = TRUE,{
    
    #"Groups","Sample"
    if(input$GSEA_Group_v_Sample == "Sample" & length(DATA.Values.5min)>1)
    {
      updateSelectizeInput(session, inputId = "Control_GSEA_Test",label = "Select Control",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = colnames(DATA.Values)[1:ncol(DATA.Values)][1])
      updateSelectizeInput(session, inputId = "Query_GSEA_Test",label = "Select Query",choices = colnames(DATA.Values)[1:ncol(DATA.Values)], selected = colnames(DATA.Values)[1:ncol(DATA.Values)][ncol(DATA.Values)])
    }#if(input$DEO_GO_Group_v_Sample == "Samples")
    if(input$GSEA_Group_v_Sample == "Groups" & exists("Groups"))
    {
      
      if(Group_Count>0)
      {
        updateSelectizeInput(session, inputId = "Control_GSEA_Test",label = "Select Control",choices = Groups, selected = Groups[1])
        updateSelectizeInput(session, inputId = "Query_GSEA_Test",label = "Select Query",choices = Groups, selected = Groups[length(Groups)])
      }#if(Group_Count>0)
      if(Group_Count==0)
      {
        updateSelectizeInput(session, inputId = "Control_GSEA_Test",label = "Select Control",choices = NULL, selected = NULL)
        updateSelectizeInput(session, inputId = "Query_GSEA_Test",label = "Select Query",choices = NULL, selected = NULL)
      }#if(Group_Count==0)
      
    }#if(input$DEO_GO_Group_v_Sample == "Groups" & exists("Groups"))
    
  })#observeEvent(input$DEO_GO_Group_v_Sample,{
  #####################################
  #####################################

   
##################################################
######### GSEA Run_GSEA_GeneSets ##########
################################################

	observeEvent(input$Run_Enriched_Signatures,ignoreInit = TRUE,{

	
	suppressPackageStartupMessages(library(fgsea))

	if(!exists("GSET_HallMark"))
	{
		Existing_Objects = ls()
		load("Data/ROGUE_GSEA_Workspace.RData")
		New_Objects = setdiff(ls(), Existing_Objects)
		Make_Global_Object_Command = paste(New_Objects,"<<-", New_Objects)

		for(Command in Make_Global_Object_Command) 
		{
			eval(expr = parse(text = Command))
		}#for(Command in Make_Global_Object_Command)

	}#if(!exists("GSET_HallMark"))

	if(length(DATA.Values.5min) <= 1)
	{
		shinyalert(title = "Data Error",text = "Please load data into the tool", type = "warning")

		return(NULL)
	}


    	if(exists("DATA.Values.5min"))
    	{ 
			if(!is.null(DATA.Values.5min))
			{    
 			   if(input$GSEA_Group_v_Sample == "Sample"){Fpkm.table.New.txt = DATA.Values.5min}
			}#if(!missing("DATA.Values.5min"))
    	}#if(exists(DATA.Values.5min))


    	if(exists("Groups.Medians.Matrix"))
     	{ 
     	    if(!is.null(Groups.Medians.Matrix))
     	    { 
	 		   if(input$GSEA_Group_v_Sample == "Groups"){Fpkm.table.New.txt = Groups.Medians.Matrix}
	 		}#if(!is.null(Groups.Medians.Matrix))
     	}#if(exists(Groups.Medians.Matrix))
 
   		Control.fpkm = input$Control_GSEA_Test
    	Control.fpkm.inx = which(colnames(Fpkm.table.New.txt)==Control.fpkm)
    	Test.fpkm = input$Query_GSEA_Test
    	Test.fpkm.inx = which(colnames(Fpkm.table.New.txt) %in% Test.fpkm)   
    	 	
    	 	                            
        if(length(Test.fpkm)>0 & length(Control.fpkm)>0 & Test.fpkm != Control.fpkm)
    	{
    
  			Gene_Set_Compare  = data.frame(rownames(Fpkm.table.New.txt),Fpkm.table.New.txt[,c(Control.fpkm.inx, Test.fpkm.inx)])
  			colnames(Gene_Set_Compare) = c("Gene", "Subject", "Query")

			if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			{
				Subject = Gene_Set_Compare$Subject[match(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))]
				Query = Gene_Set_Compare$Query[match(GENE_Symbol_Ensembl$Gene.Symbol, Gene_Set_Compare$Gene)]
			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			
			
			if(length(intersect(as.character(substr(GENE_Symbol_Ensembl$Ensembl.Gene.ID,1,15)),as.character(Gene_Set_Compare$Gene))) > 2)
			{
				Subject = Gene_Set_Compare$Subject[match(substr(as.character(GENE_Symbol_Ensembl$Ensembl.Gene.ID),1,15),as.character(Gene_Set_Compare$Gene))]
				Query = Gene_Set_Compare$Query[match(substr(GENE_Symbol_Ensembl$Ensembl.Gene.ID,1,15), Gene_Set_Compare$Gene)]
			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)			
			
			names(Subject) = GENE_Symbol_Ensembl$Gene.ID
			names(Query) = GENE_Symbol_Ensembl$Gene.ID

			Subject = Subject[which(!is.na(Subject))]
			Query = Query[which(!is.na(Query))]

			EXPRESSION = apply(cbind(Subject,Query),MARGIN=1,max)
			FC = apply(cbind(Subject,Query),MARGIN=1,function(X) (log((X[2]+1)/(X[1]+1))))

			EXPRESSION.pval = p.adjust(pnorm(EXPRESSION,mean(Subject),sd(EXPRESSION),lower.tail = F),method = "none")
			FC.pval = p.adjust(pnorm(abs(FC),mean(abs(FC)),sd(abs(FC)),lower.tail = F),method = "none")
			
			Expr.FC.pval = apply(cbind(EXPRESSION.pval,FC.pval),MARGIN = 1, mean)

			RANK.vals = FC * -log10(Expr.FC.pval)
			
			#ALL_Lists = c(Hs.H,Hs.c1,Hs.c2,Hs.c3,Hs.c4,Hs.c5,Hs.c6,Hs.c7)		
		
			GSEA_LIST = c("All","hallmark gene sets","positional gene sets","curated gene sets","motif gene sets","computational gene sets","GO gene sets","oncogenic signatures","immunologic signatures")
			#ALL_Lists = c(Hs.H,Hs.c1,Hs.c2,Hs.c3,Hs.c4,Hs.c5,Hs.c6,Hs.c7)
			All_Collections = c("Hs.H,Hs.c1,Hs.c2,Hs.c3,Hs.c4,Hs.c5,Hs.c6,Hs.c7","Hs.H","Hs.c1","Hs.c2","Hs.c3","Hs.c4","Hs.c5","Hs.c6","Hs.c7")


			if(any(input$GSEA_Dataset_List == "All")) Collection_Select = All_Collections[which(GSEA_LIST %in% c("All") )]
			if(all(input$GSEA_Dataset_List != "All")) Collection_Select = All_Collections[which(GSEA_LIST %in% input$GSEA_Dataset_List )]	
			
			GET_GENESET_COMMAND = paste("ALL_Lists = c(",paste(Collection_Select,collapse = ","),")",sep="")

    		eval(expr = parse(text = GET_GENESET_COMMAND))
    		
    		
    		fgseaRes <- fgsea(pathways = ALL_Lists, 
            stats    = RANK.vals,
            minSize  = 10,
            maxSize  = 500,nperm = 1000)

			topPathwaysUp <- fgseaRes[ES > 0][head(order(pval), n=as.numeric(input$GSEA_TOP_HITS)), pathway]
			topPathwaysDown <- fgseaRes[ES < 0][head(order(pval), n=as.numeric(input$GSEA_BOTTOM_HITS)), pathway]
			topPathways <- c(topPathwaysUp, rev(topPathwaysDown))

			shinyjs::show(id = "GSEA_PLOT_1_div")
			output$GSEA_PLOT_1 <- renderPlot({
				plot.new()
				plotGseaTable(ALL_Lists[topPathways], RANK.vals, fgseaRes, gseaParam=0.5)
			})#renderPlot({

			GSEA_ALL_Lists_topPathways = ALL_Lists[topPathways]
			GSEA_RANK.vals = RANK.vals
			GSEA_fgseaRes = fgseaRes 			
			GSEA_ALL_Lists_topPathwaysUP = ALL_Lists[[topPathwaysUp[1]]]
			GSEA_topPathwaysUp = topPathwaysUp[1]
			GSEA_Test.fpkm = Test.fpkm			

			save(list = c("GSEA_ALL_Lists_topPathways","GSEA_RANK.vals","GSEA_fgseaRes","GSEA_ALL_Lists_topPathwaysUP","GSEA_topPathwaysUp","GSEA_Test.fpkm"), file=paste(Backup_Session_Folder,"/GSEA_PLOT_1.rdata",sep=""))
		
			updateSelectizeInput(session = session,inputId = "GSEA_GeneSet",label = "Select Gene Set",choices = names(ALL_Lists), selected = topPathwaysUp[1])	
			
			shinyjs::show(id = "GSEA_PLOT_2_div")
			output$GSEA_PLOT_2 <- renderPlot(
				plotEnrichment(ALL_Lists[[topPathwaysUp[1]]],RANK.vals) + labs(title = paste(gsub("_"," ",topPathwaysUp[1]), Test.fpkm,sep="\n"))
			)#renderPlot(

			
			
    		#Subject = Gene_Set_Compare$Subject[match(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))]
			#Query = Gene_Set_Compare$Query[match(GENE_Symbol_Ensembl$Gene.Symbol, Gene_Set_Compare$Gene)]
			
			
			
	        if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			{
					
				Subject_EXPR = Gene_Set_Compare$Subject[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Gene.Symbol[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				Query_EXPR = Gene_Set_Compare$Query[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Gene.Symbol[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				GENE_NAMES = Gene_Set_Compare$Gene[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Gene.Symbol[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]


			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			
			
			if(length(intersect(as.character(substr(GENE_Symbol_Ensembl$Ensembl.Gene.ID,1,15)),as.character(Gene_Set_Compare$Gene))) > 2)
			{				
			    Subject_EXPR = Gene_Set_Compare$Subject[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Ensembl.Gene.ID[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				Query_EXPR = Gene_Set_Compare$Query[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Ensembl.Gene.ID[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				GENE_NAMES = Gene_Set_Compare$Gene[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Ensembl.Gene.ID[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)			

			
			
		     			
    			EXPRESSION_sub = apply(cbind(Subject_EXPR,Query_EXPR),MARGIN=1,max)
				FC_sub = apply(cbind(Subject_EXPR, Query_EXPR),MARGIN=1,function(X) (log((X[2]+1)/(X[1]+1))))

    			EXPRESSION.pval_sub = p.adjust(pnorm(EXPRESSION_sub,mean(Subject),sd(EXPRESSION),lower.tail = F),method = "none")
				FC.pval_sub = p.adjust(pnorm(abs(FC_sub),mean(abs(FC)),sd(abs(FC)),lower.tail = F),method = "none")
			
				Expr.FC.pval_sub = apply(cbind(EXPRESSION.pval_sub, FC.pval_sub),MARGIN = 1, mean)

				RANK.vals_sub = FC_sub * -log10(Expr.FC.pval_sub)
    	
    	
    			
    			#MATRIX = cbind(Subject_EXPR, Query_EXPR) 
    			MATRIX_BarPlot = data.frame(Genes=GENE_NAMES, Subject = Subject_EXPR,Query = Query_EXPR)
    			
    			MATRIX_BarPlot$Genes <- factor(MATRIX_BarPlot$Genes, levels = MATRIX_BarPlot$Genes[order(RANK.vals_sub,decreasing = T)])


				MATRIX_BarPlot_ay <- list(
  					tickfont = list(color = "red"),
  					verlaying = "y",
  					side = "right",
  					title = "second y axis"
				)

				MATRIX_BarPlot_Test.fpkm = Test.fpkm
				MATRIX_BarPlot_Control.fpkm = Control.fpkm

				shinyjs::show(id = "GSEA_Barplot_div")
				output$GSEA_Barplot <- renderPlotly({
				  
				  plot_ly(MATRIX_BarPlot, x = ~Genes, y = ~Subject, type = 'bar', name = MATRIX_BarPlot_Control.fpkm, marker = list(color = 'rgb(49,130,189)')) %>%
				    add_trace(y = ~Query, name = MATRIX_BarPlot_Test.fpkm, marker = list(color = 'rgb(204,204,204)'), axis = "y2") %>%
  				  layout(yaxis2 = MATRIX_BarPlot_ay,
         		xaxis = list(title = "", tickangle = -45),
         			yaxis = list(title = ""),
         		margin = list(b = 100),
         		barmode = 'group')

    		})#output$GSEA_Barplot <- renderPlotly({
    
	        save(list = c("MATRIX_BarPlot","MATRIX_BarPlot_Control.fpkm","MATRIX_BarPlot_Test.fpkm","MATRIX_BarPlot_ay"), file=paste(Backup_Session_Folder,"/GSEA_Barplot.rdata",sep=""))

	

    			GSEA_MATRIX = cbind(Subject_EXPR, Query_EXPR) 
    			rownames(GSEA_MATRIX) = GENE_NAMES
    			colnames(GSEA_MATRIX) = c(Control.fpkm, Test.fpkm) 
    			GSEA_RANK.vals_sub = RANK.vals_sub    		

			shinyjs::show(id = "GSEA_Heatmap_div")
			output$GSEA_Heatmap <- renderPlotly({	

    			heatmaply(GSEA_MATRIX[order(GSEA_RANK.vals_sub,decreasing = T),],colors =  rev(brewer.pal(nrow(GSEA_MATRIX),"RdBu")),Rowv = F,Colv=F,scale="row")

    	})#output$GSEA_Heatmap <- renderPlotly({


    		####output$GSEA_Heatmap2 <- renderPlotly({
    			
    			####MATRIX = cbind(Subject_EXPR, Query_EXPR) 
    			####rownames(MATRIX) = GENE_NAMES
    			####colnames(MATRIX) = c(Control.fpkm, Test.fpkm) 
    			
			shinyjs::show(id = "GSEA_Heatmap2_div")
		output$GSEA_Heatmap2 <- renderPlotly({

    			heatmaply(GSEA_MATRIX[order(GSEA_RANK.vals_sub,decreasing = T),],colors =  rev(brewer.pal(nrow(GSEA_MATRIX),"RdBu")),Rowv = F,Colv=F)

    		})#output$GSEA_Heatmap <- renderPlotly({
	

		save(list = c("GSEA_MATRIX","GSEA_RANK.vals_sub"), file=paste(Backup_Session_Folder,"/GSEA_Heatmaps.rdata",sep=""))			

  		}#if(length(Test.fpkm)>0 & length(Control.fpkm)>0 & length(GO_Shortlist)>0)
 
})#observeEvent(input$Run_Enriched_Signatures,{

#############################################   
#############################################
#############################################                                              
   
   ##################################################
######### GSEA Run_Enriched_Signatures ##########
################################################

#observeEvent(input$Run_GSEA,{
	observeEvent(input$GSEA_GeneSet,ignoreInit = TRUE,{
		
		if(length(DATA.Values.5min) <= 1)
		{	
			shinyalert(title = "Data Error",text = "Please load data into the tool", type = "warning")
			return(NULL)
		}


	 if(length(input$GSEA_GeneSet)>0 & input$GSEA_GeneSet != "" & !is.na(input$GSEA_GeneSet) & !is.null(input$GSEA_GeneSet))
	 {
    	if(exists("DATA.Values.5min"))
    	{ 
			if(!is.null(DATA.Values.5min))
			{    
 			   if(input$GSEA_Group_v_Sample == "Sample"){Fpkm.table.New.txt = DATA.Values.5min}
			}#if(!missing("DATA.Values.5min"))
    	}#if(exists(DATA.Values.5min))


    	if(exists("Groups.Medians.Matrix"))
     	{ 
     	    if(!is.null(Groups.Medians.Matrix))
     	    { 
	 		   if(input$GSEA_Group_v_Sample == "Groups"){Fpkm.table.New.txt = Groups.Medians.Matrix}
	 		}#if(!is.null(Groups.Medians.Matrix))
     	}#if(exists(Groups.Medians.Matrix))
 
   		Control.fpkm = input$Control_GSEA_Test
    	Control.fpkm.inx = which(colnames(Fpkm.table.New.txt)==Control.fpkm)
    	Test.fpkm = input$Query_GSEA_Test
    	Test.fpkm.inx = which(colnames(Fpkm.table.New.txt) %in% Test.fpkm)   
    	 	
    	 	                            
        if(length(Test.fpkm)>0 & length(Control.fpkm)>0 & Test.fpkm != Control.fpkm)
    	{
    
  			Gene_Set_Compare  = data.frame(rownames(Fpkm.table.New.txt),Fpkm.table.New.txt[,c(Control.fpkm.inx, Test.fpkm.inx)])
  			colnames(Gene_Set_Compare) = c("Gene", "Subject", "Query")

			if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			{
				Subject = Gene_Set_Compare$Subject[match(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))]
				Query = Gene_Set_Compare$Query[match(GENE_Symbol_Ensembl$Gene.Symbol, Gene_Set_Compare$Gene)]
			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			
			
			if(length(intersect(as.character(substr(GENE_Symbol_Ensembl$Ensembl.Gene.ID,1,15)),as.character(Gene_Set_Compare$Gene))) > 2)
			{
				Subject = Gene_Set_Compare$Subject[match(substr(as.character(GENE_Symbol_Ensembl$Ensembl.Gene.ID),1,15),as.character(Gene_Set_Compare$Gene))]
				Query = Gene_Set_Compare$Query[match(substr(GENE_Symbol_Ensembl$Ensembl.Gene.ID,1,15), Gene_Set_Compare$Gene)]
			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)			
			
			names(Subject) = GENE_Symbol_Ensembl$Gene.ID
			names(Query) = GENE_Symbol_Ensembl$Gene.ID

			Subject = Subject[which(!is.na(Subject))]
			Query = Query[which(!is.na(Query))]

			EXPRESSION = apply(cbind(Subject,Query),MARGIN=1,max)
			FC = apply(cbind(Subject,Query),MARGIN=1,function(X) (log((X[2]+1)/(X[1]+1))))

			EXPRESSION.pval = p.adjust(pnorm(EXPRESSION,mean(Subject),sd(EXPRESSION),lower.tail = F),method = "none")
			FC.pval = p.adjust(pnorm(abs(FC),mean(abs(FC)),sd(abs(FC)),lower.tail = F),method = "none")
			
			Expr.FC.pval = apply(cbind(EXPRESSION.pval,FC.pval),MARGIN = 1, mean)

			RANK.vals = FC * -log10(Expr.FC.pval)
			

			
			#GSEA_LIST = c("All","hallmark gene sets","positional gene sets","curated gene sets","motif gene sets","computational gene sets","GO gene sets","oncogenic signatures","immunologic signatures")
			#ALL_Lists = c(Hs.H,Hs.c1,Hs.c2,Hs.c3,Hs.c4,Hs.c5,Hs.c6,Hs.c7)		
		
			GSEA_LIST = c("All","hallmark gene sets","positional gene sets","curated gene sets","motif gene sets","computational gene sets","GO gene sets","oncogenic signatures","immunologic signatures")
			#ALL_Lists = c(Hs.H,Hs.c1,Hs.c2,Hs.c3,Hs.c4,Hs.c5,Hs.c6,Hs.c7)
			All_Collections = c("Hs.H,Hs.c1,Hs.c2,Hs.c3,Hs.c4,Hs.c5,Hs.c6,Hs.c7","Hs.H","Hs.c1","Hs.c2","Hs.c3","Hs.c4","Hs.c5","Hs.c6","Hs.c7")


			if(any(input$GSEA_Dataset_List == "All")) Collection_Select = All_Collections[which(GSEA_LIST %in% c("All") )]
			if(all(input$GSEA_Dataset_List != "All")) Collection_Select = All_Collections[which(GSEA_LIST %in% input$GSEA_Dataset_List )]	
			
			GET_GENESET_COMMAND = paste("ALL_Lists = c(",paste(Collection_Select,collapse = ","),")",sep="")

			eval(expr = parse(text = GET_GENESET_COMMAND))
					
		
			updateSliderInput(session = session,inputId = "GSEA_Gene_limit_slider",label = "Select Gene Limit",min = 2,max = length(ALL_Lists[[as.character(input$GSEA_GeneSet)]]) ,value = length(ALL_Lists[[as.character(input$GSEA_GeneSet)]]),step = 1)
			
			shinyjs::show(id = "GSEA_PLOT_2_div")
		  output$GSEA_PLOT_2 <- renderPlot(
		   		plotEnrichment(ALL_Lists[[as.character(input$GSEA_GeneSet)]],RANK.vals) + labs(title = paste(gsub("_"," ",as.character(input$GSEA_GeneSet)), Test.fpkm,sep="\n"))
		  )#renderPlot(
			
			
			if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			{
					
				Subject_EXPR = Gene_Set_Compare$Subject[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Gene.Symbol[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				Query_EXPR = Gene_Set_Compare$Query[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Gene.Symbol[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				GENE_NAMES = Gene_Set_Compare$Gene[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Gene.Symbol[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]


			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			
			
			if(length(intersect(as.character(substr(GENE_Symbol_Ensembl$Ensembl.Gene.ID,1,15)),as.character(Gene_Set_Compare$Gene))) > 2)
			{				
			    Subject_EXPR = Gene_Set_Compare$Subject[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Ensembl.Gene.ID[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				Query_EXPR = Gene_Set_Compare$Query[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Ensembl.Gene.ID[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				GENE_NAMES = Gene_Set_Compare$Gene[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Ensembl.Gene.ID[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)			


     			
    			EXPRESSION_sub = apply(cbind(Subject_EXPR,Query_EXPR),MARGIN=1,max)
				FC_sub = apply(cbind(Subject_EXPR, Query_EXPR),MARGIN=1,function(X) (log((X[2]+1)/(X[1]+1))))

    			EXPRESSION.pval_sub = p.adjust(pnorm(EXPRESSION_sub,mean(Subject),sd(EXPRESSION),lower.tail = F),method = "none")
				FC.pval_sub = p.adjust(pnorm(abs(FC_sub),mean(abs(FC)),sd(abs(FC)),lower.tail = F),method = "none")
			
				Expr.FC.pval_sub = apply(cbind(EXPRESSION.pval_sub, FC.pval_sub),MARGIN = 1, mean)

				RANK.vals_sub = FC_sub * -log10(Expr.FC.pval_sub)
    	
				shinyjs::show(id = "GSEA_Barplot_div")
				output$GSEA_Barplot <- renderPlotly({

    			
    			#MATRIX = cbind(Subject_EXPR, Query_EXPR) 
    			MATRIX = data.frame(Genes=GENE_NAMES, Subject = Subject_EXPR,Query = Query_EXPR)
    			
    			MATRIX$Genes <- factor(MATRIX$Genes, levels = MATRIX$Genes[order(RANK.vals_sub,decreasing = T)])

				 if(as.numeric(input$GSEA_Gene_limit_slider) < nrow(MATRIX))
				 {
				 	TOP = ceiling(input$GSEA_Gene_limit_slider/2)
				 	BOTTOM = floor(input$GSEA_Gene_limit_slider/2)
				 	
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),][c(1:TOP,(nrow(MATRIX)-BOTTOM+1):nrow(MATRIX)),]
				 	RANK.vals_sub.TOP.Bottom = RANK.vals_sub[c(1:TOP,(nrow(MATRIX)-BOTTOM+1):nrow(MATRIX))]
				 	
				 	MATRIX$Genes <- factor(MATRIX$Genes, levels = MATRIX$Genes[order(RANK.vals_sub.TOP.Bottom,decreasing = T)])				 	
				 }

				ay <- list(
  					tickfont = list(color = "red"),
  					verlaying = "y",
  					side = "right",
  					title = "second y axis"
				)

				plot_ly(MATRIX, x = ~Genes, y = ~Subject, type = 'bar', name = Control.fpkm, marker = list(color = 'rgb(49,130,189)')) %>%
  				add_trace(y = ~Query, name = Test.fpkm, marker = list(color = 'rgb(204,204,204)'), axis = "y2") %>%
  				layout(yaxis2 = ay,
         		xaxis = list(title = "", tickangle = -45),
         			yaxis = list(title = ""),
         		margin = list(b = 100),
         		barmode = 'group')

    		})#output$GSEA_Barplot <- renderPlotly({
    
				
				shinyjs::show(id = "GSEA_Heatmap_div")
    		output$GSEA_Heatmap <- renderPlotly({
    			
    			MATRIX = cbind(Subject_EXPR, Query_EXPR) 
    			
    			rownames(MATRIX) = GENE_NAMES
    			colnames(MATRIX) = c(Control.fpkm, Test.fpkm) 
    			
    		     if(input$GSEA_Gene_limit_slider < nrow(MATRIX) & input$GSEA_Gene_limit_slider>2)
				 {
				 	TOP = ceiling(input$GSEA_Gene_limit_slider/2)
				 	BOTTOM = floor(input$GSEA_Gene_limit_slider/2)
				 	
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),][c(1:TOP,(nrow(MATRIX)-BOTTOM+1):nrow(MATRIX)),]
				 	RANK.vals_sub.TOP.Bottom = RANK.vals_sub[c(1:TOP,(nrow(MATRIX)-BOTTOM+1):nrow(MATRIX))]
				 					 	
				 }else{
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),]
				 }# if(input$GSEA_Gene_limit_slider < nrow(MATRIX) & input$GSEA_Gene_limit_slider>2)
    					

    			
    			heatmaply(MATRIX,colors =  rev(brewer.pal(20,"RdBu")),Rowv = F,Colv=F,scale="row")

    		})#output$GSEA_Heatmap <- renderPlotly({
    		
    		shinyjs::show(id = "GSEA_Heatmap2_div")
    		output$GSEA_Heatmap2 <- renderPlotly({
    			
    			MATRIX = cbind(Subject_EXPR, Query_EXPR) 
    			rownames(MATRIX) = GENE_NAMES
    			colnames(MATRIX) = c(Control.fpkm, Test.fpkm) 
    			
    			 if(input$GSEA_Gene_limit_slider < nrow(MATRIX) & input$GSEA_Gene_limit_slider>2)
				 {
				 	TOP = ceiling(input$GSEA_Gene_limit_slider/2)
				 	BOTTOM = floor(input$GSEA_Gene_limit_slider/2)
				 	
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),][c(1:TOP,(nrow(MATRIX)-BOTTOM+1):nrow(MATRIX)),]
				 					 	
				 }else{
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),]
				 }#if(input$GSEA_Gene_limit_slider < nrow(MATRIX) & input$GSEA_Gene_limit_slider>2)
    					
    			heatmaply(MATRIX,colors =  rev(brewer.pal(20,"RdBu")),Rowv = F,Colv=F)

    		})#output$GSEA_Heatmap <- renderPlotly({

  		}#if(length(Test.fpkm)>0 & length(Control.fpkm)>0 & length(GO_Shortlist)>0)
  		
  	}#if(length(input$GSEA_GeneSet)>0 & input$GSEA_GeneSet != "" & !is.na(input$GSEA_GeneSet)  !is.null(input$GSEA_GeneSet))
 
})#observeEvent(input$Run_Enriched_Signatures,{


#############################################   
#############################################
############################################# 
   
   	observeEvent(input$GSEA_Gene_limit_slider,ignoreInit = TRUE,{

	 if(length(input$GSEA_GeneSet)>0 & input$GSEA_GeneSet != "" & !is.na(input$GSEA_GeneSet) & !is.null(input$GSEA_GeneSet))
	 {
    	if(exists("DATA.Values.5min"))
    	{ 
			if(!is.null(DATA.Values.5min))
			{    
 			   if(input$GSEA_Group_v_Sample == "Sample"){Fpkm.table.New.txt = DATA.Values.5min}
			}#if(!missing("DATA.Values.5min"))
    	}#if(exists(DATA.Values.5min))


    	if(exists("Groups.Medians.Matrix"))
     	{ 
     	    if(!is.null(Groups.Medians.Matrix))
     	    { 
	 		   if(input$GSEA_Group_v_Sample == "Groups"){Fpkm.table.New.txt = Groups.Medians.Matrix}
	 		}#if(!is.null(Groups.Medians.Matrix))
     	}#if(exists(Groups.Medians.Matrix))
 
   		Control.fpkm = input$Control_GSEA_Test
    	Control.fpkm.inx = which(colnames(Fpkm.table.New.txt)==Control.fpkm)
    	Test.fpkm = input$Query_GSEA_Test
    	Test.fpkm.inx = which(colnames(Fpkm.table.New.txt) %in% Test.fpkm)   
    	 	
    	 	                            
        if(length(Test.fpkm)>0 & length(Control.fpkm)>0 & Test.fpkm != Control.fpkm)
    	{
    
  			Gene_Set_Compare  = data.frame(rownames(Fpkm.table.New.txt),Fpkm.table.New.txt[,c(Control.fpkm.inx, Test.fpkm.inx)])
  			colnames(Gene_Set_Compare) = c("Gene", "Subject", "Query")

			if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			{
				Subject = Gene_Set_Compare$Subject[match(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))]
				Query = Gene_Set_Compare$Query[match(GENE_Symbol_Ensembl$Gene.Symbol, Gene_Set_Compare$Gene)]
			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			
			
			if(length(intersect(as.character(substr(GENE_Symbol_Ensembl$Ensembl.Gene.ID,1,15)),as.character(Gene_Set_Compare$Gene))) > 2)
			{
				Subject = Gene_Set_Compare$Subject[match(substr(as.character(GENE_Symbol_Ensembl$Ensembl.Gene.ID),1,15),as.character(Gene_Set_Compare$Gene))]
				Query = Gene_Set_Compare$Query[match(substr(GENE_Symbol_Ensembl$Ensembl.Gene.ID,1,15), Gene_Set_Compare$Gene)]
			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)			
			
			names(Subject) = GENE_Symbol_Ensembl$Gene.ID
			names(Query) = GENE_Symbol_Ensembl$Gene.ID

			Subject = Subject[which(!is.na(Subject))]
			Query = Query[which(!is.na(Query))]

			EXPRESSION = apply(cbind(Subject,Query),MARGIN=1,max)
			FC = apply(cbind(Subject,Query),MARGIN=1,function(X) (log((X[2]+1)/(X[1]+1))))

			EXPRESSION.pval = p.adjust(pnorm(EXPRESSION,mean(Subject),sd(EXPRESSION),lower.tail = F),method = "none")
			FC.pval = p.adjust(pnorm(abs(FC),mean(abs(FC)),sd(abs(FC)),lower.tail = F),method = "none")
			
			Expr.FC.pval = apply(cbind(EXPRESSION.pval,FC.pval),MARGIN = 1, mean)

			RANK.vals = FC * -log10(Expr.FC.pval)
			

			
			#GSEA_LIST = c("All","hallmark gene sets","positional gene sets","curated gene sets","motif gene sets","computational gene sets","GO gene sets","oncogenic signatures","immunologic signatures")
			#ALL_Lists = c(Hs.H,Hs.c1,Hs.c2,Hs.c3,Hs.c4,Hs.c5,Hs.c6,Hs.c7)		
		
			GSEA_LIST = c("All","hallmark gene sets","positional gene sets","curated gene sets","motif gene sets","computational gene sets","GO gene sets","oncogenic signatures","immunologic signatures")
			#ALL_Lists = c(Hs.H,Hs.c1,Hs.c2,Hs.c3,Hs.c4,Hs.c5,Hs.c6,Hs.c7)
			All_Collections = c("Hs.H,Hs.c1,Hs.c2,Hs.c3,Hs.c4,Hs.c5,Hs.c6,Hs.c7","Hs.H","Hs.c1","Hs.c2","Hs.c3","Hs.c4","Hs.c5","Hs.c6","Hs.c7")


			if(any(input$GSEA_Dataset_List == "All")) Collection_Select = All_Collections[which(GSEA_LIST %in% c("All") )]
			if(all(input$GSEA_Dataset_List != "All")) Collection_Select = All_Collections[which(GSEA_LIST %in% input$GSEA_Dataset_List )]	
			
			GET_GENESET_COMMAND = paste("ALL_Lists = c(",paste(Collection_Select,collapse = ","),")",sep="")

			eval(expr = parse(text = GET_GENESET_COMMAND))
					
		
			
			
			if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			{
					
				Subject_EXPR = Gene_Set_Compare$Subject[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Gene.Symbol[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				Query_EXPR = Gene_Set_Compare$Query[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Gene.Symbol[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				GENE_NAMES = Gene_Set_Compare$Gene[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Gene.Symbol[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]


			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)
			
			
			if(length(intersect(as.character(substr(GENE_Symbol_Ensembl$Ensembl.Gene.ID,1,15)),as.character(Gene_Set_Compare$Gene))) > 2)
			{				
			    Subject_EXPR = Gene_Set_Compare$Subject[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Ensembl.Gene.ID[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				Query_EXPR = Gene_Set_Compare$Query[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Ensembl.Gene.ID[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
				GENE_NAMES = Gene_Set_Compare$Gene[which(Gene_Set_Compare$Gene %in%  GENE_Symbol_Ensembl$Ensembl.Gene.ID[which(as.character(GENE_Symbol_Ensembl$Gene.ID) %in% as.character(ALL_Lists[[as.character(input$GSEA_GeneSet)]]))])]
			}#if(length(intersect(as.character(GENE_Symbol_Ensembl$Gene.Symbol),as.character(Gene_Set_Compare$Gene))) > 2)			


     			
    			EXPRESSION_sub = apply(cbind(Subject_EXPR,Query_EXPR),MARGIN=1,max)
				FC_sub = apply(cbind(Subject_EXPR, Query_EXPR),MARGIN=1,function(X) (log((X[2]+1)/(X[1]+1))))

    			EXPRESSION.pval_sub = p.adjust(pnorm(EXPRESSION_sub,mean(Subject),sd(EXPRESSION),lower.tail = F),method = "none")
				FC.pval_sub = p.adjust(pnorm(abs(FC_sub),mean(abs(FC)),sd(abs(FC)),lower.tail = F),method = "none")
			
				Expr.FC.pval_sub = apply(cbind(EXPRESSION.pval_sub, FC.pval_sub),MARGIN = 1, mean)

				RANK.vals_sub = FC_sub * -log10(Expr.FC.pval_sub)
    	
				shinyjs::show(id = "GSEA_Barplot_div")
    	  	output$GSEA_Barplot <- renderPlotly({

    			
    			#MATRIX = cbind(Subject_EXPR, Query_EXPR) 
    			MATRIX = data.frame(Genes=GENE_NAMES, Subject = Subject_EXPR,Query = Query_EXPR)
    			
    			MATRIX$Genes <- factor(MATRIX$Genes, levels = MATRIX$Genes[order(RANK.vals_sub,decreasing = T)])

				 if(input$GSEA_Gene_limit_slider < nrow(MATRIX) & input$GSEA_Gene_limit_slider > 2)
				 {
				 	TOP = ceiling(input$GSEA_Gene_limit_slider/2)
				 	BOTTOM = floor(input$GSEA_Gene_limit_slider/2)
				 	
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),][c(1:TOP,(nrow(MATRIX)-BOTTOM+1):nrow(MATRIX)),]
				 	RANK.vals_sub.TOP.Bottom = RANK.vals_sub[c(1:TOP,(nrow(MATRIX)-BOTTOM+1):nrow(MATRIX))]
				 	
				 	MATRIX$Genes <- factor(MATRIX$Genes, levels = MATRIX$Genes[order(RANK.vals_sub.TOP.Bottom,decreasing = T)])				 	
				 }

				ay <- list(
  					tickfont = list(color = "red"),
  					verlaying = "y",
  					side = "right",
  					title = "second y axis"
				)

				plot_ly(MATRIX, x = ~Genes, y = ~Subject, type = 'bar', name = Control.fpkm, marker = list(color = 'rgb(49,130,189)')) %>%
  				add_trace(y = ~Query, name = Test.fpkm, marker = list(color = 'rgb(204,204,204)'), axis = "y2") %>%
  				layout(yaxis2 = ay,
         		xaxis = list(title = "", tickangle = -45),
         			yaxis = list(title = ""),
         		margin = list(b = 100),
         		barmode = 'group')

    		})#output$GSEA_Barplot <- renderPlotly({
    	
    	  shinyjs::show(id = "GSEA_Heatmap_div")
    		output$GSEA_Heatmap <- renderPlotly({
    			
    			MATRIX = cbind(Subject_EXPR, Query_EXPR) 
    			
    			rownames(MATRIX) = GENE_NAMES
    			colnames(MATRIX) = c(Control.fpkm, Test.fpkm)
    			
    			
    			
    		     if(as.numeric(input$GSEA_Gene_limit_slider) < nrow(MATRIX) & input$GSEA_Gene_limit_slider > 2)
				 {
				 	TOP = ceiling(as.numeric(input$GSEA_Gene_limit_slider)/2)
				 	BOTTOM = floor(as.numeric(input$GSEA_Gene_limit_slider)/2)
				 	
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),][c(1:TOP,((nrow(MATRIX)-BOTTOM+1):nrow(MATRIX))),]
				 					 	
				 }else{
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),]
				 }
    					
    			 
    			
    			heatmaply(MATRIX,colors =  rev(brewer.pal(20,"RdBu")),Rowv = F,Colv=F,scale="row")

    		})#output$GSEA_Heatmap <- renderPlotly({
    		
    		shinyjs::show(id = "GSEA_Heatmap2_div")
    		output$GSEA_Heatmap2 <- renderPlotly({
    			
    			MATRIX = cbind(Subject_EXPR, Query_EXPR) 
    			rownames(MATRIX) = GENE_NAMES
    			colnames(MATRIX) = c(Control.fpkm, Test.fpkm) 
    			
    			rownames(MATRIX) = GENE_NAMES
    			colnames(MATRIX) = c(Control.fpkm, Test.fpkm) 
    			    			
    			 if(as.numeric(input$GSEA_Gene_limit_slider) < nrow(MATRIX) & input$GSEA_Gene_limit_slider > 2)
				 {
				 	TOP = ceiling(input$GSEA_Gene_limit_slider/2)
				 	BOTTOM = floor(input$GSEA_Gene_limit_slider/2)

		
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),][c(1:TOP,(nrow(MATRIX)-BOTTOM+1):nrow(MATRIX)),]
				 					 	
				 }else{
				 	MATRIX = MATRIX[order(RANK.vals_sub,decreasing = T),]
				 }
    					

    			
    			heatmaply(MATRIX,colors =  rev(brewer.pal(20,"RdBu")),Rowv = F,Colv=F)

    		})#output$GSEA_Heatmap <- renderPlotly({
    	
  		}#if(length(Test.fpkm)>0 & length(Control.fpkm)>0 & length(GO_Shortlist)>0)
  		
  	}#if(length(input$GSEA_GeneSet)>0 & input$GSEA_GeneSet != "" & !is.na(input$GSEA_GeneSet)  !is.null(input$GSEA_GeneSet))
 
})#observeEvent(input$GSEA_Gene_limit_slider,{
  ##################
  ####################
  ###################################


             output$Download_Summary_Plots <- downloadHandler(

             filename =  "Report_Sample_DEG.pdf", 
             content = function(file) {
                pdf(file,fillOddEven=T, bg="white",fg="white")
                for(D in 1:length(Report.List.Reads))
                {      
                   plot(Report.List.Reads[[D]])
                }#for(D in 1:length(Report.List.Reads))
        	
                dev.off()
             },
                )#output

  
###############################
###############################
############################### 
   

#session$allowReconnect(TRUE)
     
}#server

# Run the application 
#shinyApp(ui = ui, server = server)


shinyApp(ui = dashboardPage(title= "ROGUE", dashboardHeader(tags$li(class = "dropdown",
      tags$script('$(document).on("shiny:sessioninitialized",function(){$.get("https://api.ipify.org", function(response) {Shiny.setInputValue("getIP", response);});})'),	
      tags$style(".main-header {max-height: 75px}"),
      tags$style(".main-header .logo {height: 75px}")
    ),
title = tags$a(tags$img(src='ROGUE.png',align="left", width= "250", height="75", padding="0px")),titleWidth=250),sidebar,body), server = server)

