#This is an R script, that given a folder with the results of banzai, 
#will create some plots and dfs with the missing and unexpected sampe barcodes found
#As it is now, you have to input here the directory
banzai.output="/Users/moncho/Documents/troubleshooting_eDNA/Aquaculture_diversity/output/banzai_out_20170317_1709"#Your directory here

#And we need to load some R functions, and libraries
library(gridExtra)
library(stringr)
library (ggplot2)
library (reshape2)
library(Biostrings)
source('./Scripts/remap_banzai_output.r')
source('./Scripts/string.diff.ex.r')
#source('./Scripts/per_lib.r')
source('./Scripts/fv.r')
source('./Scripts/distance_to_known.r')
source('./Scripts/Mon_rev_com.r')
source("./Scripts/multiplot.r")

#banzai_output_function<-function(banzai.output){
#If we want to ake this a shiny app, then we could use the out folder as teh input variable

### specify otu mapfile
otu_mapfile <- paste0(banzai.output,"/all_lib/derep.map")
# read otu map
otu_map <- read.table(otu_mapfile, stringsAsFactors = FALSE, header = FALSE)
otu_map[,4:6]<-with (otu_map, colsplit(V2,";",c("library","Fwd","Rev")))
otu_map[,5:6]<-apply(otu_map[,5:6], 2, gsubseq)
otu_map_no_singletons<-subset(otu_map, V3>1)
#return(write.csv(head(otu_map_no_singletons, file=paste0(banzai.output,"trial_output.csv"))))
#}
### specify sample mapfile
sample_mapfile <- paste0(banzai.output, "/sample_trans.tmp")
# read sample map
sample_map <- read.table(sample_mapfile, stringsAsFactors = FALSE, header = FALSE)

sample_map[,4:6]<-with (sample_map, colsplit(V1,";",c("library","Fwd","Rev")))
sample_map[,5:6]<-apply(sample_map[,5:6], 2, gsubseq)
#Now run all functions

#otu_map <- rename_samples(otu_map, sample_map)

otu_map_no_singletons<-subset(otu_map, V3>1)
otu_map_no_singletons$library<-as.factor(otu_map_no_singletons$library)
  a<-levels(as.factor(otu_map_no_singletons$library))
   test<-lapply(a,per_lib,otu=otu_map_no_singletons, map=sample_map )
  names(test)<-a
    #The output is in a list, so we have to unlist it and work on each output
  tett2<-unlist(test, recursive = F);rm(test)
  names(tett2)
  #Plots are .plot, missing and unexpectedF and R
  #Plots: showd them on screen and make a pdf
    b<-grep(pattern = ".plot", x = names(tett2))
    library("gridExtra")
    heatmaps<-marrangeGrob(tett2[b],ncol = 1, nrow = 2)
    heatmaps
   t<-multiplot(tett2[b], cols=1)
   
   png(paste0(banzai.output,"/A.png"))
   tett2[b][1]
   dev.off()
   png(paste0(banzai.output,"/all.png"))
   heatmaps
   dev.off()
#Seems like there is something really wrong. Do a similar plot only including the intended barcodes
   
   
  #Missing samples
    c<-grep(pattern = ".missing", x = names(tett2))
    c1<-do.call("rbind", tett2[c])
    c1$lib_fwd<-with(c1,interaction(library,Fwd, drop = T))
    c1$sample<-sample_map[,3][match(c1$lib_fwd,sample_map$lib_fwd)]
    c1
    write.csv(c1, file=paste0(banzai.output,"/all_missing_samples.csv"))
  #Unexpected samples - We analize each set of unexpected barcodes, on both the Fwd and rev end of the amplicon
    d<-grep(pattern = ".unexpectedF", x= names(tett2))
    d1<-do.call("rbind", tett2[d])
    e<-grep(pattern = ".unexpectedR", x= names(tett2))
    e1<-do.call("rbind", tett2[e])
    write.csv(rbind(d1,e1), file=paste0(banzai.output,"/all_unexpected_barcodes.csv"))
    p<-ggplot(data=d1, aes(x=Mapped,y=Barcode, alpha=N_Reads))
    p<- p+geom_raster()+labs(x="")+theme_bw()+theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
                                                    axis.ticks.y = element_blank(),axis.text.y = element_blank())
    fwd<-p+facet_grid(Barcode~lib, scales="free_y", switch = "y")+theme(strip.text.y = element_text(angle = 180))
    ggsave(fwd,"unexpected_Fwd_barcodes.png", dpi=600)
    q<- ggplot(data=e1, aes(x=lib,y=Barcode, alpha=N_Reads, fill=as.factor(Diffs)))
    q<- q+geom_raster()+labs(x="")+theme_bw()+theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(),
                                                    axis.ticks.y = element_blank(),axis.text.y = element_blank(),
                                                    strip.text.y = element_text(angle = 180))
    rev<-q +facet_grid(Barcode~lib, scales="free", switch = "y")
    x_l<-nlevels(as.factor(e1$Barcode))
    y_l<-nlevels(as.factor(e1$lib))
    ggsave(rev,filename = "unexpected_Rev_barcodes.png", dpi=600, height = 0.3*(x_l), width=(y_l))
    