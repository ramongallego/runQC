#A function that looks at any barcode, compares it to the list of expected barcodes in a library or project
# and returns a list of the closest match and how many reads were there

distance_to_known_F<-function(barcode,dir,rev_dir,lib,df, map){
  t<-NULL
  for (j in 1: length(map)){   
    
    t[j]<-string.diff.ex(barcode,map[j])}
  print (paste(map[which.min(t)],min(t), sep =" "))
  Ctag=Mon_rev_com(unique(df[df[,dir] ==barcode,rev_dir]))
  #return(temporary_df)
  return(data.frame(lib,
                    Barcode=barcode,
                    Direction= dir,
                    Closest_Tag=map[which.min(t)],
                    Diffs=min(t), 
                    N_Reads=aggregate(df[df[,dir] ==barcode,"V3"],list(df[df[,dir] == barcode,dir],df[df[,dir] == barcode,rev_dir]),sum)[,3],
                    Complement_tag=Ctag,
                    Mapped=Ctag %in% map,
           stringsAsFactors=F))
  t=NULL
}
