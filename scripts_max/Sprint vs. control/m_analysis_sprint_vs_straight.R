################################################################
###
###     analysis of sprint vs. control methylation data
###
################################################################

library(wateRmelon); library(methylumi);library(FDb.InfiniumMethylation.hg19);library(minfi); library(maxprobes); library(tidyverse);library(ggplot2); library(GEOquery); library(plyr)
BiocManager::install("Biobase")
library(Biobase)

########################
###
### fetching data 
###
### had to manually create the CSV file sinze there was no correct format csv file in the untared folder



path <- "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Sprint_vs._conrol_data/GSE162288_RAW (1).tar"

untar(path, exdir = "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Sprint_vs._conrol_data/untared")


my_dir <- "/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Sprint_vs._conrol_data/untared"

zip_file <- list.files(path = my_dir, pattern = "*.gz", full.names = TRUE)

ldply(.data = zip_file, .fun = gunzip)



gse <- getGEO( GEO = "GSE162288", GSEMatrix = TRUE, destdir = my_dir)




########
###
### http://rstudio-pubs-static.s3.amazonaws.com/15738_49fb53277af54059b9851755116de6f1.html
###


library(limma);library(reshape2);library(lattice);require(gridExtra); library(sesame)
BiocManager::install("sesame")

metadata <- pData(phenoData(gse[[1]]))

ALL.dat <- as.data.frame(exprs(gse[[1]]))

#  groups : metadata$'experimental condition:ch1'
#  timepoints: metadata$timepoint:ch1

mVals <- BetaValueToMValue(ALL.dat)


mVals2 <- melt(mVals)

mVals2 %>% 
        ggplot(aes(x = value,color = variable))+
        geom_density()

bVals <- melt(ALL.dat)

bVals %>% 
        ggplot(aes(x = value,color = variable))+
        geom_density()

#### expression set looks normalized

library(org.Hs.eg.db); library(AnnotationDbi);library(pathview); library(gage); library(gageData); library(ChAMP);library(IlluminaHumanMethylationEPICanno.ilm10b2.hg19)
library(annotate)
eset <- GDS2eSet(gse, do.log2=TRUE, GPL = gpl)


names(gse)


eset <- as.data.frame(exprs(gse[[1]]))

annotate::getSYMBOL(featureNames(eset), annotation(eset))


show(gse)


################## 
##
### turn expression set into GenomicRatioSet
###
### https://www.rdocumentation.org/packages/minfi/versions/1.18.4/topics/GenomicRatioSet-class


makeGenomicRatioSetFromDataFrame(ALL.dat)
makeGeno

BiocManager::install("GenomicRanges")
library(GenomicRanges)

makeGRangesFromDataFrame(ALL.dat)


makeGRangesListFromFeatureFragments()

BiocManager::install("SummarizedExperiment")
library(SummarizedExperiment)

GR <- makeSummarizedExperimentFromExpressionSet(gse$GSE162288_series_matrix.txt.gz)



GenomicRatioSet(gr = GRanges(GR), Beta = NULL, M = NULL, CN = NULL, pData = DataFrame(), annotation = "", preprocessMethod = "")





Granges <- rowRanges(GR)




 