#-------------------------------------------------------------------------------

setwd("/Users/jimmy.odonnell/Projects/Aquaculture_Diversity/banzai_out_20161206_1607")

# specify otu mapfile
otu_mapfile <- "all_lib/derep.map"

# specify sample mapfile
sample_mapfile <- "sample_trans.tmp"

# read otu map
otu_map <- read.table(otu_mapfile, stringsAsFactors = FALSE, header = FALSE)
head(otu_map)
dim(otu_map)

# read sample map
sample_map <- read.table(sample_mapfile, stringsAsFactors = FALSE, header = FALSE)
head(sample_map)

# remove reads from library (id1) and primer (id2) indexes that were not used
clean_map <- function(map_otu, map_sample){
	return(map_otu[map_otu[,2] %in% map_sample[,1],])
}
otu_map_clean <- clean_map(otu_map, sample_map)
dim(otu_map_clean)
head(otu_map)


remove_rare <- function(map_otu, threshold){
	totals <- sapply(split(map_otu[,3], map_otu[,1]), sum)
	return(map_otu[map_otu[,1] %in% names(which(totals > threshold)),])
}
otu_map_gt10 <- remove_rare(otu_map_clean, 10)

# replace long format names with user-supplied names
# first remove "sample=" prefix
sample_map$V3 <- gsub("sample=", "", sample_map$V3)
head(sample_map)
rename_samples <- function(map_otu, map_sample){
	map_otu[,2] <- map_sample[,3][match(map_otu[,2], map_sample[,1])]
	return(map_otu)
}
otu_map_final <- rename_samples(otu_map_gt10, sample_map)
class(otu_map_final)
head(otu_map_final)

otu_map_w <- xtabs(V3 ~ V2 + V1, data = otu_map_final)
dim(otu_map_w)

# sort by column sums
otu_map_w <- otu_map_w[,order(colSums(otu_map_w), decreasing = TRUE)]

otu_map_w[1:5,1:5]

write.csv(otu_map_w, file = "otu_table.csv", quote = FALSE)
