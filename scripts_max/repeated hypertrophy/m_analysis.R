################################################################
###
###     analysis of repeated hypertrophy methylation data
###
################################################################

library(wateRmelon); library(methylumi);library(FDb.InfiniumMethylation.hg19);library(minfi); library(maxprobes); library(tidyverse);library(ggplot2); library(GEOquery); library(plyr)


path <- "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/GSE114763_RAW.tar"

untar(path, exdir = "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/untared")



my_dir <- "/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/untared"

zip_file <- list.files(path = my_dir, pattern = "*.gz", full.names = TRUE)

ldply(.data = zip_file, .fun = gunzip)

targets <- read.metharray.sheet(my_dir)

rgSet <- read.metharray.exp(targets = targets)

targets$ID <- paste(targets$Sample_Group,targets$Sample_Name,sep=".")
sampleNames(rgSet) <- targets$ID



myLoad2 <- champ.load(my_dir, arraytype = "EPIC", method = "minfi")


