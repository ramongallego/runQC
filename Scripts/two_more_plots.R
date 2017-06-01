greens_AF<-sample_map[which(sample_map[,"library"]=="ID1=A"),"Fwd"]
greens_AR<-sample_map[which(sample_map[,"library"]=="ID1=A"),"Rev"]
greens_BF<-sample_map[which(sample_map[,"library"]=="ID1=B"),"Fwd"]
greens_BR<-sample_map[which(sample_map[,"library"]=="ID1=B"),"Rev"]
to_plotA<-subset(tett2[[5]],subset= Fwd %in% greens_AF & Rev %in% greens_AR)
levels(to_plotA$Fwd)==levels(tett2[[5]]$Fwd)
to_plotB<-subset(tett2[[10]], subset= Fwd %in% greens_BF & Rev %in% greens_BR)
b<-grep(pattern = ".data_to_plot", x = names(tett2))
c<-gsub(pattern = ".data_to_plot",replacement = "",x = names(tett2))
l<-NULL
m<-NULL
for (i in b){
  tett2[[i]]$lib=c[i]
  m<-rbind(m,do.call(cbind.data.frame,tett2[[i]]))
}
#Now do the same for ELWHA
l$Project<-"ELWHA"
m$Project<-"Aquaculture"
b<-rbind(l,m)
print(str(b))
toplot<-subset()
p<- ggplot(data = to_plotA, aes(x=Rev, y=Fwd, fill=V3))   
p<-p+ geom_raster()+scale_fill_distiller(palette = "Spectral" , name="# Reads")+theme_bw()+ggtitle("Library A")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold", angle = 90))
p
q<- ggplot(data = to_plotB, aes(x=Rev, y=Fwd, fill=V3))   
q<-q+ geom_raster()+scale_fill_distiller(palette = "Spectral" , name="# Reads")+theme_bw()+ggtitle("Library B")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold", angle = 90))
q
##Now do that facet with each lib, barcode and divide btween expected and unexpected Rev
#step 1 take all reads FWD,Rev, counts, lib
dim(all_good_F)

#step2, create a list of all reads that got something right: 
  #2.1 asign them to a tag 
Tags_16S<-data.frame(Fwd=levels(as.factor(sample_map[,"Fwd"])),
                     Tag=1:nlevels(as.factor(sample_map[,"Fwd"])))
Tags_16S[,"Rev"]<-sample_map[,"Rev"][match(Tags_16S[,"Fwd"], sample_map[,"Fwd"])]
 #2.2 Asign them to a tag - Fwd
b$Tag<-Tags_16S[,"Tag"][match(b$Fwd,Tags_16S$Fwd)]
summary(is.na(all_good_F))#1812 rightly assigned; 644 nope
b[,"Tag_R"]<-Tags_16S[,"Tag"][match(b$Rev,Tags_16S$Rev)]#Do the same for rev tag - 700 missed
b[,"Tag"][is.na(b[,"Tag"])]<-b[,"Tag_R"][is.na(b[,"Tag"])]# replace NAs with the rev
b<-b[!is.na(b[,"Tag"]),]
b[,"Tag"]<-as.factor(paste0("Tag#",b[,"Tag"]))


compare_strings<-function(x,y,z){ifelse(DNAString(as.character(x))==reverseComplement(DNAString(as.character(y))),
                                        z<-"Paired",z<-"Unpaired")}
b$Paired<-"NA"

##1.c Do a for loop that marks which sequences agree on their Fwd and Rev barcodes
for (i in 1:nrow(b)){
  b$Paired[i]<-compare_strings(b$Fwd[i],b$Rev[i], b$Paired[i])
}
b$Paired<-as.factor(b$Paired)

ggplot(data = subset(b, Project=="ELWHA"), aes(x= Paired, y=V3, fill=Paired))+geom_col()+facet_grid(lib~Tag, scales = "fixed")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+labs(y="# Reads")
  ggplot(data = subset(all_good_F,Fwd %in% greens_AF), aes(x= Paired, y=V3, fill=Paired))+geom_col()+facet_grid(Fwd~lib, scales = "fixed")+
  theme(axis.text.x = element_blank(),
        strip.text.y = element_text(angle=0),
        axis.ticks.x = element_blank())+labs(y="# Reads")
 
