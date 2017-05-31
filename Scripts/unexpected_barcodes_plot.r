#I think this is for unexpected  barcodes per library
p<-ggplot(data=to_plot, aes(x=Mapped,y=Barcode, alpha=N_Reads))
p<- p+geom_raster()+labs(x="")+theme_bw()+theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())
p+facet_grid(Barcode~library, scales = "free_y")
