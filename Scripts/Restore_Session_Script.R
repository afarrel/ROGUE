suppressPackageStartupMessages(library(fgsea))
suppressPackageStartupMessages(library(plotly))
suppressWarnings(suppressPackageStartupMessages(library(DESeq2)))

if(exists("allData.tSNE") & exists("plotlyMargins"))
{
  shinyjs::show(id = "MDSPlot_div")
  output$MDSPlot <- renderPlotly({
    
    plot_ly(x=allData.tSNE$X, y= allData.tSNE$Y,text=rownames(allData.tSNE)) %>%
      layout(title = "t-SNE Plot",autosize = F, width = 600, height = 600, margin = plotlyMargins)
    
  }) ## output$MDSPlot <- renderPlotly({
} ## if(exists("allData.tSNE") & exists("plotlyMargins"))




#####################
if(exists("ComparePlot1_Table") & exists("ComparePlot1_m")){
  shinyjs::show(id = "ComparePlot1_div")
  output$ComparePlot1 <- renderPlotly({
    
    plot_ly(x=log2(ComparePlot1_Table[,1] +1),y=log2(ComparePlot1_Table[,2]+1),
            text = paste(rownames(ComparePlot1_Table),paste(colnames(ComparePlot1_Table)[1],round(ComparePlot1_Table[,1],2),sep=": "),paste(colnames(ComparePlot1_Table)[2],round(ComparePlot1_Table[,2],2),sep=": "),sep="\n"),
            marker = list(color = ComparePlot1_Point_Colors,size=3)
    ) %>%
      layout(autosize = F, width = 500, height = 500, margin = ComparePlot1_m) %>%
      layout(xaxis = list(title = paste(colnames(ComparePlot1_Table)[1],"log2(RPKM + 1)"),range=c(ComparePlot1_MIN.dim,ComparePlot1_MAX.dim)),yaxis = list(title = paste(colnames(ComparePlot1_Table)[2],"log2(RPKM + 1)"), range=c(ComparePlot1_MIN.dim,ComparePlot1_MAX.dim)))
    
  })#output$ComparePlot1 <- renderPlot({
}#if(exists("ComparePlot1_Table") & exists("ComparePlot1_m")){
###################


if(exists("Group.CompareTable1") & exists("Group.Compare_m")){
  
  shinyjs::show(id = "Groups.ComparePlot1_div")
  output$Groups.ComparePlot1 = renderPlotly({
    plot_ly(x=log2(Group.CompareTable1[,1]+1),y=log2(Group.CompareTable1[,2]+1),
            text = paste(rownames(Group.CompareTable1),paste(colnames(Group.CompareTable1)[1],round(Group.CompareTable1[,1],2),sep=": "),paste(colnames(Group.CompareTable1)[2],round(Group.CompareTable1[,2],2),sep=": "),sep="\n"),
            marker = list(color = Group.Compare_Point_Colors,size=3)
    ) %>%
      layout(autosize = F, width = 500, height = 500, margin = Group.Compare_m) %>%
      layout(xaxis = list(title = paste(colnames(Group.CompareTable1)[1],"log2(RPKM)"),range=c(Group.Compare_MIN.dim,Group.Compare_MAX.dim)),yaxis = list(title = paste(colnames(Group.CompareTable1)[2],"log2(RPKM)"), range=c(Group.Compare_MIN.dim,Group.Compare_MAX.dim)))
    
  })#output$Groups.ComparePlot1 = renderPlotly({  })#output$Groups.ComparePlot1 = renderPlotly({
}#if(exists("Group.CompareTable1") & exists("Group.CompareTable1_m")){
###################

if(exists("MATRIX_BarPlot") & exists("MATRIX_BarPlot_ay")){
  
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
}## if(exists("MATRIX_BarPlot") & exists("MATRIX_BarPlot_ay")){



####output$GSEA_Heatmap <- renderPlotly({

if(exists("GSEA_MATRIX") & exists("GSEA_RANK.vals_sub")){
  
  shinyjs::show(id = "GSEA_Heatmap_div")
  output$GSEA_Heatmap <- renderPlotly({
    
    heatmaply(GSEA_MATRIX[order(GSEA_RANK.vals_sub,decreasing = T),],colors =  rev(brewer.pal(nrow(GSEA_MATRIX),"RdBu")),Rowv = F,Colv=F,scale="row")
    
  })#output$GSEA_Heatmap <- renderPlotly({
  
  shinyjs::show(id = "GSEA_Heatmap2_div")
  output$GSEA_Heatmap2 <- renderPlotly({
    
    heatmaply(GSEA_MATRIX[order(GSEA_RANK.vals_sub,decreasing = T),],colors =  rev(brewer.pal(nrow(GSEA_MATRIX),"RdBu")),Rowv = F,Colv=F)
    
  })#output$GSEA_Heatmap <- renderPlotly({
  
}## if(exists("GSEA_MATRIX") & exists("GSEA_RANK.vals_sub")){

###################


if(exists("GO_Barplot_data") & exists("GO_Barplot_text")){
  
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
  
}#if(exists("GO_Barplot_data") & exists("GO_Barplot_text")){


######################

if(exists("Group_Stats_X") & exists("Group_Stats_Y")){
  
  shinyjs::show(id = "Group_Stats_log2RPKM_div")
  output$Group_Stats_log2RPKM <- renderPlotly({
    plot_ly(x=Group_Stats_X,y=Group_Stats_Y,text = Group_Stats_Text) %>%
      layout(autosize = F, width = 600, height = 600, margin = Group_Stats_Margins ,showlegend = FALSE,xaxis = list(title = Group_Stats_xTitle, range=c(Group_Stats_MIN.dim,Group_Stats_MAX.dim)),yaxis = list(title = Group_Stats_yTitle,range=c(Group_Stats_MIN.dim,Group_Stats_MAX.dim)))
    
  })#output$Group_Stats_log2RPKM <- renderPlotly({
}#if(exists("Group_Stats_X") & exists("Group_Stats_Y")){

#######################


if(exists("New.Data.Subset.tsne.3") & exists("R.scale")){
  
  shinyjs::show(id = "Group_Stats_t_SNE_3Dplotly_div")
  output$Group_Stats_t_SNE_3Dplotly <- renderPlotly({
    
    plot_ly(data=New.Data.Subset.tsne.3,x=~X, y=~Y,z=~Z,text=paste(rownames(New.Data.Subset.tsne.3)),
            marker = list(color = rgb(R.scale,G.scale,B.scale),
                          symbol='circle',
                          sizemode = 'diameter',size=as.numeric(input_Group_Stats_tSNE_pointSize))) %>%
      layout(autosize = F, width = 600, height = 600, margin = plotlyMargins)
    
  })#output$Group_Stats_t_SNE_3Dplotly <- renderPlotly
}#if(exists("New.Data.Subset.tsne.3") & exists("R.scale"))

if(exists("New.Data.Subset.tsne.3.Groups")){
  
  shinyjs::show(id = "Group_Stats_t_SNE_3Dplotly_div")
  output$Group_Stats_t_SNE_3Dplotly <- renderPlotly({
    
    plot_ly(data=New.Data.Subset.tsne.3.Groups,x=~X, y=~Y,z=~Z,text=paste(rownames(New.Data.Subset.tsne.3.Groups)),color=~GROUPINGS,
            marker = list(symbol='circle',
                          sizemode = 'diameter',size=as.numeric(input_Group_Stats_tSNE_pointSize))) %>%
      layout(autosize = F, width = 600, height = 600, margin = plotlyMargins)
    
  })#output$Group_Stats_t_SNE_3Dplotly <- renderPlotly
  
}#if(exists("New.Data.Subset.tsne.3.Groups"))


if(exists("New.Data.Subset.tsne.2") & exists("R.scale")){
  
  #########################
  shinyjs::show(id = "Group_Stats_t_SNE_plotly_div")
  output$Group_Stats_t_SNE_plotly <- renderPlotly({
    ##########
    plot_ly(data=New.Data.Subset.tsne.2,x=~X, y=~Y,text=paste(rownames(New.Data.Subset.tsne.2)),marker = list(color = rgb(R.scale,G.scale,B.scale))) %>%
      layout(autosize = F, width = 600, height = 600, margin = plotlyMargins)
    
  })#output$Group_Stats_t_SNE_plotly <- renderPlotly({
}#if(exists("New.Data.Subset.tsne.2") & exists("R.scale"))


if(exists("New.Data.Subset.tsne.3") & exists("R.scale") & exists("Group_Stat.tSNE.Gene")) {

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

}#if(exists("New.Data.Subset.tsne.3") & exists("R.scale"))



#########################

if(exists("DEO_GO_Heatmap_rows")){
  shinyjs::show(id = "DEO_GO_Heatmap_rowScale_div")
  output$DEO_GO_Heatmap_rowScale <- renderPlotly({
    
    DEO_GO_Heatmap_rows %>% layout(boxmode = "group")
  })#output$DEO_GO_Heatmap_rowScale
}
#######################

if(exists("DEO_GO_Box_ggp"))
{
  shinyjs::show(id = "DEO_GO_Boxplot_div")
  output$DEO_GO_Boxplot <- renderPlotly({
    
    ggplotly(DEO_GO_Box_ggp) %>% layout(boxmode = "group")
    
  })#output$DEO_GO_Boxplot
}
##########################


if(exists("DEO_GO_Distribution_GList"))
{
  shinyjs::show(id = "DEO_GO_Distribution_div")
  output$DEO_GO_Distribution <- renderPlot({

    do.call("grid.arrange",c(DEO_GO_Distribution_GList,ncol= 5-as.numeric(DEO_GO_Distribution_Graph_Width)))
  
  })#output$DEO_GO_Distribution
}##if(exists("DEO_GO_Distribution_GList"))

##########################

if(exists("GO_Heatmap_Final_PLOT_Text_rowScale"))
{
  shinyjs::show(id = "GO_Heatmap_rowScale_div")
  output$GO_Heatmap_rowScale <- renderPlotly({
    
    eval(parse(text=GO_Heatmap_Final_PLOT_Text_rowScale))
    
  })#output$GO_Heatmap_rowScale <- renderPlotly({
}#if(exists(GO_Heatmap_Final_PLOT_Text_rowScale))
#########################

if(exists("GO_Heatmap_Final_PLOT_Text"))
{
  shinyjs::show(id = "GO_Heatmap_div")
  output$GO_Heatmap <- renderPlotly({
    eval(parse(text= GO_Heatmap_Final_PLOT_Text))
  })#output$GO_Heatmap <- renderPlot({
}#if(exists(GO_Heatmap_Final_PLOT_Text))


#########################

if(exists("Volc.Reads.plot"))
{
  shinyjs::show(id = "ComparePlot2_div")
  output$ComparePlot2 = renderPlot({
    #p1 #must be done to render the image
    Volc.Reads.plot
    #Volc.Reads
  })#output$ComparePlot2
}


if(exists("Plot_smear_Plot"))
{
  shinyjs::show(id = "ComparePlot3_div")
  output$ComparePlot3 = renderPlot({
    
    Plot_smear_Plot
  })#output$ComparePlot3
}#if(input$Reads.EdgeR_DEseq == "DESeq2")


if(exists("MDS_Distance.plot_MDS_PLOT.Samples")){
  
  shinyjs::show(id = "MDS_PLOT.Samples_div")
  output$MDS_PLOT.Samples <- renderPlot({
    MDS_Distance.plot_MDS_PLOT.Samples
  }) ## output$MDS_PLOT.Samples <- renderPlot({
  
}#if(eexists(MDS_Distance.plot_MDS_PLOT.Samples)){

if(exists("MDS_Distance_distPlot")){
  
  shinyjs::show(id = "distPlot_div")
  output$distPlot <- renderPlot({
    MDS_Distance_distPlot
  }) ## output$distPlot <- renderPlot({
  
}#if(eexists(MDS_Distance.plot_MDS_PLOT.Samples)){



if(exists("Group.MDS_Distance.plot")){
  
  shinyjs::show(id = "Group.MDS_PLOT.Samples_div")
  output$Group.MDS_PLOT.Samples <- renderPlot({
    Group.MDS_Distance.plot
  }) ## output$Group.MDS_PLOT.Samples <- renderPlot({
  
}#if(eexists("Group.MDS_Distance.plot")){

if(exists("HM.Reads.plot_ComparePlot4")){
  
  shinyjs::show(id = "ComparePlot4_div")
  output$ComparePlot4 <- renderPlot({
    HM.Reads.plot_ComparePlot4
  }) ## output$ComparePlot4 <- renderPlot({
  
}#if(exists("HM.Reads.plot_ComparePlot4")){


if(exists("HM.Reads.50.plot")){
  
  shinyjs::show(id = "ComparePlot5_div")
  output$ComparePlot5 <- renderPlot({
    HM.Reads.50.plot
    
  })#output$ComparePlot5
}


if(exists("HM.Reads.100.plot")){
  
  shinyjs::show(id = "ComparePlot6_div")
  output$ComparePlot6 <- renderPlot({
    HM.Reads.100.plot
    
  })#output$ComparePlot6
}




if(exists("HM.Groups.Reads.plot")){
  
  shinyjs::show(id = "Groups.ComparePlot4_div")
  output$Groups.ComparePlot4 <- renderPlot({
    HM.Groups.Reads.plot
    
  })#output$Groups.ComparePlot4
}## if(exists("HM.Groups.Reads.plot")){

if(exists("HM.Groups.Reads.plot.50")){
  
  shinyjs::show(id = "Groups.ComparePlot5_div")
  output$Groups.ComparePlot5 <- renderPlot({
    HM.Groups.Reads.plot.50
    
  })#output$Groups.ComparePlot5
}## if(exists("HM.Groups.Reads.plot.50")){


if(exists("HM.Groups.Reads.plot.100")){
  
  shinyjs::show(id = "HM.Groups.Reads.plot.100_div")
  output$Groups.ComparePlot6 <- renderPlot({
        
      HM.Groups.Reads.plot.100
    })# output$Groups.ComparePlot6 = renderPlot({
}#if(exists("HM.Groups.Reads.plot.100")){

if(exists("Volc.Groups.Reads.plot")){
  
  shinyjs::show(id = "Groups.ComparePlot2_div")
  output$Groups.ComparePlot2 <- renderPlot({
    Volc.Groups.Reads.plot
    
  })#output$Groups.ComparePlot2
}## if(exists("Volc.Groups.Reads.plot")){


if(exists("Stacked_Plot_plot_list") & exists("Stacked_Plot_GRID.COLS")  ){
  
  shinyjs::show(id = "Stacked_Plot_div")
  output$Stacked_Plot <- renderPlot({
    
    do.call("grid.arrange", c(Stacked_Plot_plot_list, ncol= Stacked_Plot_GRID.COLS))
    
  })#output$Stacked_Plot
}## if(exists("Stacked_Plot_plot_list") & exists("Stacked_Plot_GRID.COLS")  ){



if(exists("Single.Sample.heatmap_BP8")){
  
  shinyjs::show(id = "Single.Sample.heatmap_div")
  output$Single.Sample.heatmap <- renderPlot({
    
    Single.Sample.heatmap_BP8
    
  })#output$Single.Sample.heatmap
}## if(exists("Single.Sample.heatmap_BP8")){


if(exists("Single.Sample.barplot_P") & exists("Single.Sample.barplot_Width")  ){
  
  shinyjs::show(id = "Single.Sample.barplot_div")
  output$Single.Sample.barplot <- renderPlot({
    
    do.call("grid.arrange", c(list(Single.Sample.barplot_P), ncol= (6-Single.Sample.barplot_Width), nrow= 1))
    
  })#output$Single.Sample.barplot
}## if(exists("Single.Sample.barplot_P") & exists("Single.Sample.barplot_Width")  ){


if(exists("GSP_Groups.Medians") & exists("GSP_Groups.sem.max") & exists("GSP_Gene.Index")  ){  
  
  shinyjs::show(id = "Group.stackedPlot_div")
  output$Group.stackedPlot <- renderPlot({
    
      n.rows =  ceiling(length(GSP_Gene.Index)/3)
    
      par(mfrow=c(n.rows,3*4/GSP_Width_Adjust))
    
      for(xyz in 1:length(GSP_Gene.Index))
      {
        bpgp.stackSEM = barplot(as.numeric(as.matrix(GSP_Groups.Medians[xyz,])),ylim = c(0,max(GSP_Groups.sem.max[xyz,])),names.arg=colnames(GSP_Groups.Medians),main=rownames(GSP_Groups.Medians)[xyz],las=2,col=(xyz+1),cex.names=0.8*GSP_Font_Adjust,cex.main=1*GSP_Font_Adjust,cex.axis=1*GSP_Font_Adjust)
        arrows(bpgp.stackSEM,as.numeric(as.matrix(GSP_Groups.Medians[xyz,])),bpgp.stackSEM,as.numeric(as.matrix(GSP_Groups.sem.max[xyz,])),angle = 90,code = 2,length = 0.05)
      }#for(xyz in 1:length(Gene.Index))
      par(mfrow=c(1,1))
      
  })#output$Group.stackedPlot <- renderPlot({
}##if(exists("GSP_Groups.Medians") & exists("GSP_Groups.sem.max") & exists("GSP_Font_Adjust")  ){  

if(exists("GSP_MM_Groups.Medians") & exists("GSP_MM_Groups.Max") & exists("GSP_MM_Gene.Index")  ){  
  
  shinyjs::show(id = "Group.stackedPlot.maxmin_div")
  output$Group.stackedPlot.maxmin <- renderPlot({
    
    n.rows =  ceiling(length(GSP_MM_Gene.Index)/3)
    
    par(mfrow=c(n.rows,3*4/GSP_Width_Adjust))
    
    for(xyz in 1:length(GSP_MM_Gene.Index))
    {
      bpgp.stackMM = barplot(as.numeric(as.matrix(GSP_MM_Groups.Medians[xyz,])),names.arg=colnames(GSP_MM_Groups.Medians),main=rownames(GSP_MM_Groups.Medians)[xyz],las=2,col=(xyz+1),ylim=c(0,max(GSP_MM_Groups.Max[xyz,])),cex.names=0.8*GSP_MM_Font_Adjust,cex.main=1*GSP_MM_Font_Adjust,cex.axis=1*GSP_MM_Font_Adjust)
      segments(bpgp.stackMM,as.numeric(as.matrix(GSP_MM_Groups.Min[xyz,])),bpgp.stackMM,as.numeric(as.matrix(GSP_MM_Groups.Max[xyz,])))
      arrows(bpgp.stackMM,as.numeric(as.matrix(GSP_MM_Groups.Min[xyz,])),bpgp.stackMM,as.numeric(as.matrix(GSP_MM_Groups.Max[xyz,])),angle = 90,code = 3,length = 0.05)
    }#for(xyz in 1:length(Gene.Index))

  })#output$Group.stackedPlot.maxmin <- renderPlot({
}##if(exists("GSP_MM_Groups.Medians") & exists("GSP_MM_Groups.Max") & exists("GSP_MM_Gene.Index")  ){  



if(exists("Group.boxplot_plot_list") & exists("Group.boxplot_GRID.ROWS") & exists("Group.boxplot_GRID.COLS")  ){
  
  shinyjs::show(id = "Group.boxplot_div")
  output$Group.boxplot <- renderPlot({
    
    do.call("grid.arrange", c(Group.boxplot_plot_list, ncol= Group.boxplot_GRID.COLS, nrow= Group.boxplot_GRID.ROWS))
    
  })#output$Group.boxplot
}## if(exists("Single.Sample.barplot_P") & exists("Single.Sample.barplot_Width")  ){


if(exists("Group.barplot_plot_list") & exists("Group.barplot_GRID.ROWS") & exists("Group.barplot_GRID.COLS")  ){
  
  shinyjs::show(id = "Group.barplot.label_div")
  output$Group.barplot.label <- renderPlot({
    
    do.call("grid.arrange", c(Group.barplot_plot_list, ncol= Group.barplot_GRID.COLS, nrow= Group.barplot_GRID.ROWS))
    
  })#output$Group.barplot.label
}## if(exists("Group.barplot_plot_list") & exists("Group.barplot_GRID.ROWS") & exists("Group.barplot_GRID.COLS")  ){

if(exists("Group.barplot.sem_plot_list") & exists("Group.barplot.sem_GRID.ROWS") & exists("Group.barplot.sem_GRID.COLS")  ){
  
  shinyjs::show(id = "Group.barplot.sem_div")
  output$Group.barplot.sem <- renderPlot({
    
    do.call("grid.arrange", c(Group.barplot.sem_plot_list, ncol= Group.barplot.sem_GRID.COLS, nrow= Group.barplot.sem_GRID.ROWS))
    
  })#output$Group.barplot.sem
}## if(exists("Group.barplot.sem_plot_list") & exists("Group.barplot.sem_GRID.ROWS") & exists("Group.barplot.sem_GRID.COLS")  ){


if(exists("Group_heatmap_HM.GP")){
  
  shinyjs::show(id = "Group.heatmap_div")
  output$Group.heatmap <- renderPlot({
    
    Group_heatmap_HM.GP
    
  })#output$Group.heatmap
}## if(exists("Group_heatmap_HM.GP")){


if(exists("Group_heatmap_HM.Mem.GP")){
  
  shinyjs::show(id = "Group.members.heatmap_div")
  output$Group.members.heatmap <- renderPlot({
    
    Group_heatmap_HM.Mem.GP
    
  })#output$Group.members.heatmap
}## if(exists("Group_heatmap_HM.Mem.GP"))

if(exists("GSEA_ALL_Lists_topPathways") & exists("GSEA_RANK.vals") & exists("GSEA_fgseaRes")  ){
  
  shinyjs::show(id = "GSEA_PLOT_1_div")
  output$GSEA_PLOT_1 <- renderPlot({
    plot.new()
    plotGseaTable(GSEA_ALL_Lists_topPathways, GSEA_RANK.vals, GSEA_fgseaRes, gseaParam=0.5)
  })# output$GSEA_PLOT_1
  
  shinyjs::show(id = "GSEA_PLOT_2_div")
  output$GSEA_PLOT_2 <- renderPlot(
    plotEnrichment(GSEA_ALL_Lists_topPathwaysUP,GSEA_RANK.vals) + labs(title = paste(gsub("_"," ",GSEA_topPathwaysUp), GSEA_Test.fpkm,sep="\n"))
  )#  output$GSEA_PLOT_2
  
  rm(list = c("GSEA_ALL_Lists_topPathways","GSEA_RANK.vals","GSEA_fgseaRes","GSEA_ALL_Lists_topPathwaysUP","GSEA_topPathwaysUp","GSEA_Test.fpkm"))
}## if(exists("GSEA_ALL_Lists_topPathways") & exists("GSEA_RANK.vals") & exists("GSEA_fgseaRes")  )
  

if(exists("GSSP_plot.coords") & exists("GSSP_Control.data") & exists("GSSP_Treatment.data")) {
  
  shinyjs::show(id = "Group_Stats_Scatter_Boxplot_div")
  output$Group_Stats_Scatter_Boxplot <- renderPlot({
    
    par(mfrow = c(GSSP_PLot_Count,2))
    plot(GSSP_plot.coords,xlim=c(min(GSSP_plot.coords),max(GSSP_plot.coords)),ylim=c(min(GSSP_plot.coords),max(GSSP_plot.coords)),pch=16,cex=0.75,main=GSSP_Gene_Names,xlab = GSSP_Control.Label,ylab = GSSP_Treatment.Label,cex.lab=0.7)
    lines(rbind(c((min(GSSP_plot.coords)-5),(min(GSSP_plot.coords)-5)),c((max(GSSP_plot.coords)+5),(max(GSSP_plot.coords)+5))),lty=2)
    boxplot(unlist(GSSP_Control.data),unlist(GSSP_Treatment.data), main=GSSP_Gene_Names,names=c(GSSP_Control.Label.bp,GSSP_Treatment.Label.bp),las=2,ylab="Expression",cex.axis=0.7)
    
    
  })#output$Group_Stats_Scatter_Boxplot
} ## if(exists("GSSP_plot.coords") & exists("GSSP_Control.data") & exists("GSSP_Treatment.data")) 


if(exists("Group_Stat_Gene_FC_table") & exists("GSFC_lab.size")) {
  
  shinyjs::show(id = "Group_Stat_Gene_FC_div")
  output$Group_Stat_Gene_FC <- renderPlot({
    
    barplot(abs(Group_Stat_Gene_FC_table[,2]),names=Group_Stat_Gene_FC_table[,1],las=2,cex.names=GSFC_lab.size,ylab="abs(Log2 FC)",col=c("grey","blue"))
    
  })#output$Group_Stat_Gene_FC
} ## if(exists("Group_Stat_Gene_FC_table")) 






if(exists("DATA.Values"))
{
  if(!is.null(DATA.Values))
  if(nrow(DATA.Values) >= 3)
  {
    updateSelectizeInput(session, inputId="All_Conditions", label = "Select Dataset", choices = colnames(DATA.Values),selected = NULL)
    updateSelectizeInput(session, inputId="Single.Compare.Conditions", label = "Select Samples", choices = c("ALL",colnames(DATA.Values)),selected = NULL)
    updateSelectizeInput(session, inputId="Single.Control", label = "Select Control", choices = colnames(DATA.Values),selected = NULL)
    updateSelectizeInput(session, inputId="Single.Treatment", label = "Select Treatment", choices = colnames(DATA.Values),selected = NULL)
    updateSelectizeInput(session, inputId = "Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values), selected = NULL)
    updateSelectizeInput(session, inputId = "Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values), selected = NULL)
    updateSelectizeInput(session, inputId = "DEO_Control_GO_Test",label = "Select Control",choices = colnames(DATA.Values), selected = NULL)
    updateSelectizeInput(session, inputId = "DEO_Subjects_GO_Test",label = "Select Subjects",choices = colnames(DATA.Values), selected = NULL)
    updateSelectizeInput(session, inputId = "Control_GSEA_Test",label = "Select Control",choices = colnames(DATA.Values), selected = colnames(DATA.Values)[1])
    updateSelectizeInput(session, inputId = "Query_GSEA_Test",label = "Select Query",choices = colnames(DATA.Values), selected = colnames(DATA.Values)[ncol(DATA.Values)])
  }#if(nrow(DATA.Values) >= 3)
}#if(exists("DATA.Values"))


if(exists("readData") & exists("DATA.Values.5min"))
{
  if(!is.null(DATA.Values.5min))
  {
    shinyjs::show(id = "Comparison_Box_wrapper")
    updateSelectizeInput(session, inputId="Comparison", label = "Select Library", choices = colnames(DATA.Values.5min), selected = colnames(DATA.Values.5min)[1:2])
  }
}


if(exists("Groups") & exists("Group.Members"))
{
  if(length(Groups) >= 1 & length(Group.Members) >= 1)
  {
    updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=paste(paste(Groups,":",Group.Members,sep="",collaspe="\n"),collapse=""))
    updateSelectizeInput(session, inputId="All_Conditions",selected = "")
    updateTextInput(session,inputId="Group_Name", value="")
    updateSelectizeInput(session, inputId="Group.Compare.Condition", label = "Select Groups", choices = c("ALL",Groups),selected = NULL)
    updateSelectInput(session, inputId = "Group1_Stat_compare",label = "Select Group1",choices = Groups, selected = NULL)
    updateSelectInput(session, inputId = "Group2_Stat_compare",label = "Select Group2",choices = Groups, selected = NULL)
    updateSelectInput(session, inputId = "Group_Stats_tSNE_Group",label = "Highlight Group (Optional)",choices = Groups, selected = NULL)
    
    if(length(readData)>1)
    {
      updateSelectizeInput(session,inputId="Groups.Control", label="Select Group 1",choices = Groups, selected = NULL)
      updateSelectizeInput(session,inputId="Groups.Treatment", label="Select Group 2",choices = Groups, selected = NULL)
    }#if(length(readData)>1)
    
  }#if(length(Groups) >= 1 & length(Group.Members) >= 1)
}#if(exists("Groups") & exists("Group.Members"))
 
if(exists("GC3_comparison") & exists("GC3_topGenesTable") & exists("GC3_mainTitle"))
{
  shinyjs::show(id = "Groups.ComparePlot3_div")
  output$Groups.ComparePlot3 = renderPlot({
    {plotSmear(GC3_comparison, de.tags = rownames(GC3_topGenesTable), main = GC3_mainTitle, xlab = GC3_xLabel, ylab = GC3_yLabel, ylim = c(-5,5), col = "grey")
      abline(h=c(-1, 1), col = rep("orange", 2))
      abline(v=1, col = "grey")
      text(8, 3, paste("upregulated=",GC3_up))
      text(8, -3, paste("downregulated=",GC3_down))
    }
  })#output$ComparePlot3
}#if(exists("GC3_comparison") & exists("GC3_topGenesTable") & exists("GC3_mainTitle"))






ALL_Plots_INPUT = unique(c(
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
  c("GO_Heatmap_Final_PLOT_Text_rowScale"),
  c("Group_heatmap_HM.GP"),
  c("Single.Sample.heatmap_BP8"),
  c("allData.tSNE","plotlyMargins"),
  c("ComparePlot1_Table","ComparePlot1_MIN.dim","ComparePlot1_MAX.dim","ComparePlot1_m","ComparePlot1_Point_Colors"),
  c("GO_Barplot_data","GO_Barplot_text"),
  c("GSP_Gene.Index","GSP_Groups.Medians","GSP_Groups.sem.max","GSP_Font_Adjust","GSP_Width_Adjust"),
  c("GSP_MM_Gene.Index","GSP_MM_Groups.Medians","GSP_MM_Groups.Max","GSP_MM_Groups.Min","GSP_MM_Font_Adjust","GSP_MM_Width_Adjust"),
  c("Group.MDS_Distance.plot"),
  c("MDS_Distance.plot_MDS_PLOT.Samples"),
  c("MDS_Distance_distPlot"),
  c("New.Data.Subset.tsne","R.scale","G.scale","B.scale","plotlyMargins","input_Group_Stats_tSNE_pointSize"),
  c("New.Data.Subset.tsne.3.Groups","plotlyMargins","input_Group_Stats_tSNE_pointSize"),
  c("GSSP_plot.coords","GSSP_Gene_Names","GSSP_Control.Label","GSSP_Treatment.Label","GSSP_Control.data","GSSP_Treatment.data","GSSP_Control.Label.bp","GSSP_Treatment.Label.bp","GSSP_PLot_Count"),
  c("Group.CompareTable1","Group.Compare_MIN.dim","Group.Compare_MIN.dim","Group.Compare_m","Group.Compare_Point_Colors"),
  c("GO_Heatmap_Final_PLOT_Text"),
  c("Group_heatmap_HM.Mem.GP"),
  c("Group_Stat_Gene_FC_table","GSFC_lab.size"),
  c("Group_Stats_X","Group_Stats_Y","Group_Stats_Text","Group_Stats_Margins","Group_Stats_xTitle","Group_Stats_yTitle","Group_Stats_MIN.dim","Group_Stats_MAX.dim"),
  c("Group.barplot.sem_plot_list","Group.barplot.sem_GRID.ROWS","Group.barplot.sem_GRID.COLS"),
  c("Group.barplot_plot_list","Group.barplot_GRID.ROWS","Group.barplot_GRID.COLS"),
  c("Stacked_Plot_plot_list","Stacked_Plot_GRID.COLS")
))

#rm(list = ls()[which(ls() %in% ALL_Plots_INPUT)])












