#Function within the missing_barcodes.r script
#Calculates a few things for a set of read barcodes: either Fwd or Rev
#returns a list that includes: Two lists of the missbehaving barcodes(Missing from the sample and unexpected there)

fv<- function (direction, lib, df, map){
  
  if (direction=="Fwd"){reverse_direction="Rev"}else{reverse_direction="Fwd"}
  print(paste0("The Reverse complement is the ",reverse_direction))
  #Creates lists of barcodes
  all_mappedF<-levels(as.factor(map[,direction]))
  all_present_no_singletonsF<-levels(as.factor(df[,direction]))
  rc_all_present_no_singletons<-levels(as.factor(df[,reverse_direction]))
  ####MISSING ANY OF THE EXPECTED
  #Print if all Fwd are present]
  print(paste0("Library ", lib))
  print(paste0("Direction:", direction))
  print(paste0("Are any expected ",direction," barcodes missing?"));print( sum(!all_mappedF %in% all_present_no_singletonsF))
  #Flag the missing Fwds
  if ( sum(!all_mappedF %in% all_present_no_singletonsF)>0){
    
  
  missed<-all_mappedF[!all_mappedF %in% all_present_no_singletonsF]
  
  missing_barcodes_temp<-data.frame(Barcode=missed,Direction=direction,
                                     stringsAsFactors=F)
  
  #Add the missing fwds to the plot list
  #First create the newones
  empty_to_add = data.frame(matrix(vector(), length(missed), ncol(df),
                                   dimnames=list(missed,colnames(df))),
                            stringsAsFactors=F)
  empty_to_add[,direction]=missed
  empty_to_add[,reverse_direction]=Mon_rev_com(missed)
  empty_to_add$V3=0
  #Now add them to a df
  
  }else{
    missing_barcodes_temp<-NULL
    empty_to_add<-NULL
    
  }
  
  to_plot<-rbind(df,empty_to_add)#Need to do this outside of the function
  to_plot2<-aggregate(to_plot$V3, by=list(to_plot[,direction], to_plot[,reverse_direction]),sum)
  
  colnames(to_plot2)<-c(direction,reverse_direction,"V3")
  ####ANY UNEXPECTED  FOUND?
  print(paste0("Are any unexpected ", direction," barcodes present?"));print( sum(!all_present_no_singletonsF %in% all_mappedF))  
  unexpected_Fwd<-all_present_no_singletonsF[!all_present_no_singletonsF %in% all_mappedF]
  
 
  print("What is the minimum difference between each unexpected barcode and any of the expected fwd barcodes?")

 
  list_temp<-lapply(unexpected_Fwd,distance_to_known_F, dir=direction,rev_dir=reverse_direction, lib=lib, df=df,map=all_mappedF )
  
  Unexpected_barcodes_F_temp<-do.call("rbind",lapply(list_temp, data.frame))         
  print(Unexpected_barcodes_F_temp)
  #Create two vectors: one of the order barcodes should appear, another one with the color code
  new_list_all_fwds<-c(all_mappedF[all_mappedF%in%all_present_no_singletonsF],
                       all_mappedF[!all_mappedF%in%all_present_no_singletonsF],
                       all_present_no_singletonsF[!all_present_no_singletonsF %in% all_mappedF])
  rc_new_list_all_fwds<-c(Mon_rev_com(all_mappedF[all_mappedF%in%all_present_no_singletonsF]),
                          Mon_rev_com(all_mappedF[!all_mappedF%in%all_present_no_singletonsF]),
                          rc_all_present_no_singletons[!rc_all_present_no_singletons %in% Mon_rev_com(all_mappedF)])
  list_of_text_color_F<-c(rep("green",length(all_mappedF[all_mappedF%in%all_present_no_singletonsF])),
                          rep("pink",length(all_mappedF[!all_mappedF%in%all_present_no_singletonsF])),
                          rep("red",length(all_present_no_singletonsF[!all_present_no_singletonsF %in% all_mappedF])))
  rc_list_of_text_color_F<-c(rep("green",length(all_mappedF[all_mappedF%in%all_present_no_singletonsF])),
                          rep("pink",length(all_mappedF[!all_mappedF%in%all_present_no_singletonsF])),
                          rep("red",length(rc_all_present_no_singletons[!rc_all_present_no_singletons %in% Mon_rev_com(all_mappedF)])))
  # return(missing_barcodes_temp)
  return(list(Missing_samples_df=missing_barcodes_temp,
              Unexpected_barcodes=Unexpected_barcodes_F_temp,
              order_list=new_list_all_fwds,
              order_list_rc=rc_new_list_all_fwds,
              color_list=list_of_text_color_F,
              color_list_rc=rc_list_of_text_color_F,
              data_to_plot=to_plot2))
}
