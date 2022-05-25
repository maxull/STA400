################################################################
###                                                          ###
### Gene Set Enrichment analysis                             ###
###                                                          ###
################################################################


### if running for the first time you need to install several packages first, othervise skip straight to the loading og the libraries


install.packages("BiocManager")

BiocManager::install("minfi")

BiocManager::install("FDb.InfiniumMethylation.hg19")

BiocManager::install("wateRmelon")

BiocManager::install("methylumi")

BiocManager::install("maxprobes")

install.packages("tidyverse")

install.packages("ggplot2")

### now load the libraries


library(wateRmelon); library(methylumi);library(FDb.InfiniumMethylation.hg19);library(minfi); library(maxprobes); library(tidyverse);library(ggplot2)



######################################
###
### Oshlack workflow for importing and filtering methylation data
###
### https://dockflow.org/workflow/methylation-array-analysis/#loading-the-data
###
######################################

### direct R to your data


### description of how to load your data: https://bioconductor.org/packages/devel/bioc/vignettes/minfi/inst/doc/minfi.html#3_Reading_data


testDir=system.file("extdata", package = "ChAMPdata")           ### here you need to change to where your data is stored on your computer

### load data

targets <- read.metharray.sheet(testDir)

rgSet <- read.metharray.exp(targets = targets)

### merge targets and rgSet

targets$ID <- paste(targets$Sample_Group,targets$Sample_Name,sep=".")
sampleNames(rgSet) <- targets$ID



#################################
###
### filtering
###
#################################


### filter out samples with mean P-values > 0.05

detP <- detectionP(rgSet)

keep <- colMeans(detP) < 0.05
rgSet <- rgSet[,keep]



targets <- targets[keep,]


detP <- detP[,keep]



###############################
###
### functional normalization
###
##################################


mSetSq <- preprocessFunnorm(rgSet)


detP <- detP[match(featureNames(mSetSq), rownames(detP)),]


### filter p_values <0.01


keep <- rowSums(detP < 0.01) == ncol(mSetSq)


mSetSqFlt <- mSetSq[keep,]


### if male and female samples, filter out XY chromosomes


ann450k = getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)

keep <- !(featureNames(mSetSqFlt) %in% ann450k$Name[ann450k$chr %in%  c("chrX","chrY")])



### filter out SNPs

mSetSqFlt2 <- dropLociWithSnps(mSetSqFlt)


### filter out cross reactive probes from Chen et al. 2013, Benton et al. 2015 for 450k data
### and Pidsley et al. 2016 and McCartney et al. 2016 for 850k


MsetExProbes <- dropXreactiveLoci(mSetSqFlt2)



######################################################################
###
### Filtering finnished
###
######################################################################


mVals <- getM(MsetExProbes)


mvals %>% 
        ggplot(aes())




####################
###
### KEGG pathway
###

BiocManager::install("pathview")
BiocManager::install("gage")
BiocManager::install("gageData")

library(pathview); library(gage); library(gageData); library(ChAMP)

myLoad <- champ.load(testDir, arraytype = "450K", method = "minfi")


mVals <- getM(MsetExProbes)

myDMP <- champ.DMP(beta = mVals)

foldchange <- myDMP$C_to_T$logFC

df = as.data.frame(foldchange) %>% 
        mutate(adjuste_pval = myDMP$C_to_T$adj.P.Val,
               genes = myDMP$C_to_T$gene)

df2 = as.data.frame(myDMP$C_to_T) %>% 
        filter(cgi == "island")

df2 = df2[order(abs(df2$logFC), decreasing = TRUE),]


ggplot(df2, aes(logFC, -log10(adj.P.Val)))+
        geom_point()




df = df[order(abs(df$foldchange), decreasing = TRUE),]




myDMR <- champ.DMR(beta = mVals)

GSEA <- champ.GSEA(beta = mVals)

head(GSEA$DMR)








