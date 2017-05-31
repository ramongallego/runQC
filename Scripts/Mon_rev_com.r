#Takes a DNA sequence (as a character), turns it into a DNAString, reverse complement it and turns it into a character again
#Will also work with all characters in a list

Mon_rev_com<-function(char){as.character(reverseComplement(DNAStringSet(char)))}
Mon_rev_com("ATG")
Mon_rev_com(c("ATG","CCC"))
