#Functions from Jimmy O'Donnell's mapfile_handling script
#to be used when transfering banzai outputs into OTU tables
# remove reads from library (id1) and primer (id2) indexes that were not used
## ---- chunk-1 ----
clean_map <- function(map_otu, map_sample){
  return(map_otu[map_otu[,2] %in% map_sample[,1],])
}
remove_rare <- function(map_otu, threshold){
  totals <- sapply(split(map_otu[,3], map_otu[,1]), sum)
  return(map_otu[map_otu[,1] %in% names(which(totals > threshold)),])
}
rename_samples <- function(map_otu, map_sample){
  map_otu[,2] <- map_sample[,3][match(map_otu[,2], map_sample[,1])]
  return(map_otu)
}


#Plus a few more of my own design
gsubseq<-function(x){gsub("ID[0-9][A-Z]=","",x)}
##1.b Create a function that finds if rev is the revcomplement of Fwd
compare_strings<-function(x,y,z){ifelse(DNAString(as.character(x))==reverseComplement(DNAString(as.character(y))),
                                        z<-"Paired",z<-"Unpaired")}
rename_fwd<-function(map_otu, map_sample){
  map_otu[,2] <- map_sample[,3][match(map_otu[,9], map_sample[,2])]
  return(map_otu)
  
}
#Create a function of everything that can be done on a df: Starting with a df with all Duplicate reads,
#once we have added the expected barcodes - according to the mapfile
# and generates a series of plots with all the reads that didn't pass the Fwd or Reverse matching with the barcodes
# Including 
anlysis_df<-function(dataf){
  temp.df<-as.data.frame(aggregate(dataf$V3, list(dataf$Rev, dataf$library),sum ))
  temp.df[,1]<-as.factor(temp.df[,1])
  temp.df[,2]<-as.factor(temp.df[,2])
  faulty_candidates<-interaction(temp.df[temp.df[,3]>5,2],temp.df[temp.df[,3]>5,1], drop = T)#
  faulty.fwd<-dataf[dataf$Fwd %in% faulty_candidates,]
  plot_temp<-ggplot(data=subset(temp.df,x>5), aes(x=Group.1, y=x))+geom_col()+facet_grid(Group.2~Group.1, scales = "free_x")
  #return(mean(temp.df[,3]))
  return(plot_temp)
}
