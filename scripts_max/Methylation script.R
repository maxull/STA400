

#######################################
#### methylation data analysis ########
#######################################

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


