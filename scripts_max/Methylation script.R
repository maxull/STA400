

#######################################
#### methylation data analysis ########
#######################################

if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

BiocManager::install("minfi")

if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

BiocManager::install("ChAMP", force = TRUE)

library("ChAMP")

if (!requireNamespace("BiocManager", quietly=TRUE))
        install.packages("BiocManager")
BiocManager::install(c("minfi","ChAMPdata","Illumina450ProbeVariants.db",
                       "sva","IlluminaHumanMethylation450kmanifest","limma",
                       "RPMM","DNAcopy","preprocessCore","impute","marray",
                       "wateRmelon","goseq","plyr","GenomicRanges","RefFreeEWAS",
                       "qvalue","isva","doParallel","bumphunter","quadprog","shiny",
                       "shinythemes","plotly","RColorBrewer","DMRcate","dendextend",
                       "IlluminaHumanMethylationEPICmanifest","FEM","matrixStats","missMethyl","combinat", force = "TRUE"))

BiocManager::install("YourErrorPackage")

myLoad <- ChAMP::Champ.load("data/GSM4948519.txt", arraytype = "EPIC")

BiocManager::install("locfit")

BiocManager::install("DMRcate")
