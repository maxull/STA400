

#######################################
#### methylation data analysis ########
#######################################

library(tidyr)

if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

BiocManager::install("ChAMP")

library("ChAMP")


### if champ download did not work, start downloading single packages that are 
### identified as "no package called " .... "

if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

BiocManager::install("DMRcate")

BiocManager::install("IlluminaHumanMethylation450kanno.ilmn12.hg19")
BiocManager::install("org.Hs.eg.db")
BiocManager::install("minfi")
BiocManager::install("GO.db")
BiocManager::install("geneLenDataBase")

warnings()

### seemes like ChAMP is installed
### test on included dataset to se if it works
### pipeline from:
### https://bioconductor.org/packages/devel/bioc/vignettes/ChAMP/inst/doc/ChAMP.html


testDir=system.file("extdata", package = "ChAMPdata") %>% 
        myLoad<- champ.load(testDir, arraytype = "450k")

myLoad <- champ.load(testDir,arraytype = "450k")

### show warnings

warnings()

### dont understand, but maybe not an issue

### next step in pipeline: champ.qc
### quality control and initial visualization

champ.QC()

CpG.GUI()

QC.GUI()

### normalize data, chose nethod "SWAN" or functional normalization
### BMIQ is the standard normalization method
### to plot BQIM: myNorm <- champ.norm(plotBMIQ=TRUE)
### this wil save PDF of density curves

myNorm <- champ.norm(beta = myLoad$beta,
                     rgSet = myLoad2$rgSet,
                     method ="FunctionalNormalization")                  #### find out how to chose functional



champ.SVD()                             #### not sure what this does "singular value decomposition"

### identify differentially methylated positions

myDMP <- champ.DMP()

### visualize

DMP.GUI()

### identify DMR

myDMR <- champ.DMR()

### visualize DMR

DMR.GUI()
