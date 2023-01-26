



##grep "save(" ~/Projects/ROGUE/ROGUE-main_Jan_2023/app.R | sort | uniq | cut -d ")" -f"1"| cut -d"=" -f2 |sed -e s/$/\),/g

ALL_INPUT = unique(c(
  c("DEO_GO_Distribution_GList","DEO_GO_Distribution_Graph_Width"),
  c("DEO_GO_Box_ggp"),
  c("DEO_GO_Heatmap_rows"),
  c("GSEA_ALL_Lists_topPathways","GSEA_RANK.vals","GSEA_fgseaRes","GSEA_ALL_Lists_topPathwaysUP","GSEA_topPathwaysUp","GSEA_Test.fpkm"),
  c("HM.Groups.Reads.plot"),
  c("HM.Groups.Reads.plot.100"),
  c("HM.Groups.Reads.plot.50"),
  c("HM.Reads.100.plot"),
  c("HM.Reads.50.plot"),
  c("HM.Reads.plot_ComparePlot4"),
  c("Single.Sample.barplot_P","Single.Sample.barplot_Width"),
  c("GC3_comparison","GC3_topGenesTable","GC3_mainTitle","GC3_xLabel","GC3_yLabel","GC3_up","GC3_down"),
  c("GSEA_MATRIX","GSEA_RANK.vals_sub"),
  c("New.Data.Subset.tsne","R.scale","G.scale","B.scale","plotlyMargins","sig.val.count.norm","input_Group_Stats_tSNE_pointSize"),
  c("Volc.Groups.Reads.plot"),
  c("MATRIX_BarPlot","MATRIX_BarPlot_Control.fpkm","MATRIX_BarPlot_Test.fpkm","MATRIX_BarPlot_ay"),
  c("Volc.Reads.plot"),
  c("Plot_smear_Plot"),
  c("Volc.Reads.plot"),
  c("GO_Heatmap_Final_PLOT_Text_rowScale"),
  c("Group_heatmap_HM.GP"),
  c("Single.Sample.heatmap_BP8"),
  c("DATA.Values","DATA.Values.5min","GeneList","pengRPKMTable","Reads_Reset","DATA.Values_Flag","readData","file.datapath"),
  c("allData.tSNE","plotlyMargins"),
  c("ComparePlot1_Table","ComparePlot1_MIN.dim","ComparePlot1_MAX.dim","ComparePlot1_m","ComparePlot1_Point_Colors"),
  c("GO_Barplot_data","GO_Barplot_text"),
  c("GSP_Gene.Index","GSP_Groups.Medians","GSP_Groups.sem.max","GSP_Font_Adjust","GSP_Width_Adjust"),
  c("GSP_MM_Gene.Index","GSP_MM_Groups.Medians","GSP_MM_Groups.Max","GSP_MM_Groups.Min","GSP_MM_Font_Adjust","GSP_MM_Width_Adjust"),
  c("Group.MDS_Distance.plot"),
  c("Groups","Group.GTable","Group.Members","Groups.Medians.Matrix","PRE_GROUPS","Group_Count"),
  c("Groups","Group.GTable","Group.Members","Groups.Medians.Matrix","PRE_GROUPS","Group_Count"),
  c("MDS_Distance.plot_MDS_PLOT.Samples"),
  c("MDS_Distance_distPlot"),
  c("New.Data.Subset.tsne","R.scale","G.scale","B.scale","plotlyMargins","input_Group_Stats_tSNE_pointSize"),
  c("New.Data.Subset.tsne.3.Groups","plotlyMargins","input_Group_Stats_tSNE_pointSize"),
  c("GSSP_plot.coords","GSSP_Gene_Names","GSSP_Control.Label","GSSP_Treatment.Label","GSSP_Control.data","GSSP_Treatment.data","GSSP_Control.Label.bp","GSSP_Treatment.Label.bp","GSSP_PLot_Count"),
  c("Group.CompareTable1","Group.Compare_MIN.dim","Group.Compare_MIN.dim","Group.Compare_m","Group.Compare_Point_Colors"),
  c("Plot_smear_Plot"),
  c("GO_Heatmap_Final_PLOT_Text"),
  c("Group_heatmap_HM.Mem.GP"),
  c("Group_Stat_Gene_FC_table","GSFC_lab.size"),
  c("DATA.Values","DATA.Values.5min","GeneList","Data_File_path"),
  c("Group_Stats_X","Group_Stats_Y","Group_Stats_Text","Group_Stats_Margins","Group_Stats_xTitle","Group_Stats_yTitle","Group_Stats_MIN.dim","Group_Stats_MAX.dim"),
  c("Group.barplot.sem_plot_list","Group.barplot.sem_GRID.ROWS","Group.barplot.sem_GRID.COLS"),
  c("Group.barplot_plot_list","Group.barplot_GRID.ROWS","Group.barplot_GRID.COLS"),
  c("Group.boxplot_plot_list","Group.boxplot_GRID.ROWS","Group.boxplot_GRID.COLS"),
  c("Stacked_Plot_plot_list","Stacked_Plot_GRID.COLS")
))




Selected_DATA = ALL_INPUT[which(ALL_INPUT %in% ls())]









