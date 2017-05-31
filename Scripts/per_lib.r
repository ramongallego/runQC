#Function that runs all tasks within a library

per_lib<-function(lib, otu, map){
   ############FOR EACH LIBRARY###########
  
  temp<-subset(otu,otu$library==lib )
  temp_sample_map<-subset(map,map$library==lib )
  print(head(temp))
  ##For EACH DIRECTION##  
  
  Fwd_list<-fv(direction = "Fwd",lib = lib, df = temp, map = temp_sample_map)
  Rev_list<-fv(direction="Rev",lib = lib, df = temp, map = temp_sample_map)
  ########NOW Plots per library
  ##First; the heatmap 
  Fwd_list[["data_to_plot"]]$Fwd<-factor(Fwd_list[["data_to_plot"]]$Fwd, levels=Fwd_list[["order_list"]])
  Fwd_list[["data_to_plot"]]$Rev<-factor(Fwd_list[["data_to_plot"]]$Rev, levels=Fwd_list[["order_list_rc"]])
  Fwd_list[["data_to_plot"]][Fwd_list[["data_to_plot"]]$V3==0,"V3"]<-NA
  #Now to show the colouring of the labels in the x axis according to whether those labels were found in the reverse barcode
  #I'll reorder the Revlist color labels accroding to the match between the Fwd_list_rc (sogood matches are still diagonal) and revlist(order)
  x_color<-Rev_list[["color_list"]][order(match(Rev_list[["order_list"]],Fwd_list[["order_list_rc"]]))]
  ##THE PLOTTTT
  p<- ggplot(data = Fwd_list[["data_to_plot"]], aes(x=Rev, y=Fwd, fill=V3))  
  p<-p+ geom_raster()+scale_fill_distiller(palette = "Spectral" , name="# Reads")+theme_bw()+ggtitle(paste0("Library ",lib))+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(face="bold",colour = Fwd_list[["color_list"]]),
          axis.text.x = element_text(face="bold",colour = x_color, angle = 90))
  #  p+geom_tile(aes(fill=cut_interval(V3,5)))+scale_fill_brewer(palette = "YlGnBu")
  
  Rev_list[["Missing_samples_df"]][,"Barcode"]<-Mon_rev_com(Rev_list[["Missing_samples_df"]][,"Barcode"])
  
  print("test length of missing barcodes")
  
  print(str(Fwd_list))
  print(str(Rev_list))
  #,dim(Fwd_list[["Missing_samples_df"]]),dim(Rev_list[["Missing_samples_df"]])))
  if (!is.null(Fwd_list[["Missing_samples_df"]])){
  a<-cbind(library=lib,merge(Fwd_list[["Missing_samples_df"]][-1,],Rev_list[["Missing_samples_df"]][-2,],"Barcode", all=T))
  }else{
    a<-NULL  
  }
  return(list(plot=p,missing=a,unexpectedF=Fwd_list[["Unexpected_barcodes"]], unexpectedR=Rev_list[["Unexpected_barcodes"]]))
}