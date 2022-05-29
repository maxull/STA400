################################################################
###
###     analysis of repeated hypertrophy methylation data
###
################################################################

library(wateRmelon); library(methylumi);library(FDb.InfiniumMethylation.hg19);library(minfi); library(maxprobes); library(tidyverse);library(ggplot2); library(GEOquery)


path <- "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/GSE114763_RAW.tar"

untar(path, exdir = "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/untared")

testdir = ("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/untared")
targets <- read.metharray.sheet("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/untared/GPL21145_MethylationEPIC_15073387_v-1-0.csv.gz")

list.files(testdir)
