

#######################################
#### methylation data analysis ########
#######################################

library(tidyr)

if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

BiocManager::install("ChAMP")

library("ChAMP"); library(RColorBrewer); library(limma); library(maxprobes)


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
BiocManager::install("minfiData")
devtools::install_github("markgene/maxprobes")

warnings()

### seemes like ChAMP is installed
### test on included dataset to se if it works
### pipeline from:
### https://bioconductor.org/packages/devel/bioc/vignettes/ChAMP/inst/doc/ChAMP.html


testDir=system.file("extdata", package = "ChAMPdata")

myLoad <- champ.load(testDir,arraytype = "450k")
myLoad2 <- champ.load(testDir, arraytype = "450K", method = "minfi")

pd <- as.data.frame(myLoad$pd)

### show warnings

warnings()



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



champ.SVD()                             #### not sure what this does "singular value decomposition", batch effects

### identify differentially methylated positions

myDMP <- champ.DMP()

### visualize

DMP.GUI()

### identify DMR

myDMR <- champ.DMR()

### visualize DMR

DMR.GUI()


#######################################################
#########################################################
### single function methylation analysis: champ.process

champ.process(directory = testDir,
              )
### champ.EpiMod did not work, try to bypass

#################################################################
######################################################
#########################################################
### Oshlack work flow:

# sample sheet
targets <- read.metharray.sheet(testDir)

# raw data

rgSet <- read.metharray.exp(targets = targets)
# rgSet2 <- myLoad2$rgSet # does the same as above

targets$ID <- paste(targets$Sample_Group,targets$Sample_Name,sep=".")
sampleNames(rgSet) <- targets$ID

detP <- detectionP(rgSet)

# visualize barchart of detP means

pal <- brewer.pal(8,"Dark2")
par(mfrow=c(1,2))
barplot(colMeans(detP), col=pal[factor(targets$Sample_Group)], las=2, 
        cex.names=0.8, ylab="Mean detection p-values")
abline(h=0.05,col="red")
legend("topright", legend=levels(factor(targets$Sample_Group)), fill=pal,
       bg="white")

# center y lab on bar chart

barplot(colMeans(detP), col=pal[factor(targets$Sample_Group)], las=2, 
        cex.names=0.8, ylim=c(0,0.002), ylab="Mean detection p-values")
abline(h=0.05,col="red")
legend("topright", legend=levels(factor(targets$Sample_Group)), fill=pal, 
       bg="white")

# qc rapport

qcReport(rgSet, sampNames=targets$ID, sampGroups=targets$Sample_Group, 
         pdf="Oshlack_workflow/qcReport.pdf")

#######################
# filtering samples(this will filter entire samples with high means)

keep <- colMeans(detP) < 0.05
rgSet <- rgSet[,keep]

# remove bad samples based on targets data

targets <- targets[keep,]
targets[,1:5]

# remove bad samples based on detP means
detP <- detP[,keep]
dim(detP)

######################
# normalization

mSetSq <- preprocessQuantile(rgSet)             ### dont know the nomrlaization methiod in Oshlack workflow
mSetSq2 <- preprocessFunnorm(rgSet)             ### functional normalization

# visualize before and after normalization

mSetRaw <- preprocessRaw(rgSet)

par(mfrow=c(2,1))
densityPlot(mSetRaw, sampGroups=targets$Sample_Group,main="Raw", legend=FALSE)
legend("topright", legend = levels(factor(targets$Sample_Group)), 
       text.col=brewer.pal(8,"Dark2"))
densityPlot(getBeta(mSetSq2), sampGroups=targets$Sample_Group,
            main="Normalized", legend=FALSE)
legend("topright", legend = levels(factor(targets$Sample_Group)), 
       text.col=brewer.pal(8,"Dark2"))

# functional normalization vs. quantile from Oshlack workflow

myNorm <- champ.norm(beta = myLoad$beta,
                     rgSet = myLoad2$rgSet,
                     method ="FunctionalNormalization")

# visualize difference

par(mfrow=c(2,1))

densityPlot(myNorm, main = "Functional",sampGroups=targets$Sample_Group  ,pal = brewer.pal(8, "Dark2"))

densityPlot(getBeta(mSetSq), sampGroups=targets$Sample_Group,
            main="Normalized", legend=TRUE)


###########################
### multi dimensional scaling

dev.off()    ### to reset plots viewer to default settings

plotMDS(getM(mSetSq2), top=1000, gene.selection="common",
        col=pal[factor(targets$Sample_Group)])

### principal component visualization to identify where largest differences lie

par(mfrow = c(3,2))
plotMDS(getM(mSetSq2), top=1000, gene.selection="common", 
        col=pal[factor(targets$Sample_Group)], dim=c(1,2))

plotMDS(getM(mSetSq2), top=1000, gene.selection="common", 
        col=pal[factor(targets$Sample_Group)], dim=c(1,3))

plotMDS(getM(mSetSq2), top=1000, gene.selection="common", 
        col=pal[factor(targets$Sample_Group)], dim=c(1,4))

plotMDS(getM(mSetSq2), top=1000, gene.selection="common", 
        col=pal[factor(targets$Sample_Group)], dim=c(2,3))

plotMDS(getM(mSetSq2), top=1000, gene.selection="common", 
        col=pal[factor(targets$Sample_Group)], dim=c(2,4))

plotMDS(getM(mSetSq2), top=1000, gene.selection="common", 
        col=pal[factor(targets$Sample_Group)], dim=c(3,4))
legend("topright", legend=levels(factor(targets$Sample_Group)), text.col=pal,
       cex=0.7, bg="white")


############################################
### filtering (using the functionally normlaized data)

detP <- detP[match(featureNames(mSetSq2), rownames(detP)),]

# filter p-values < 0.01

keep <- rowSums(detP < 0.01) == ncol(mSetSq2) 

table(keep)     ### returns the amount of probes that are filtered out

mSetSqFlt <- mSetSq2[keep,]
mSetSqFlt       ### shows the remaining rows and samples

### if male and female samples: filter out XYchromosomes

ann450k = getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)

keep <- !(featureNames(mSetSqFlt) %in% ann450k$Name[ann450k$chr %in%  c("chrX","chrY")])
table(keep)


### filter out SNPs

mSetSqFlt <- dropLociWithSnps(mSetSqFlt)
mSetSqFlt


### filter out cross reactive probes from Chen et al. 2013
xReactiveProbes <- read.csv(file=paste("Oshlack_workflow/13059_2016_1066_MOESM1_ESM.csv",
                                       sep="/"), stringsAsFactors=FALSE)
keep <- !(featureNames(mSetSqFlt) %in% xReactiveProbes$TargetID)
table(keep)


### filtering x reactive probes with maxprobes package
### filter out probes based on Chen et al. 2013 and Benton et al . 2015 for 450k
### and Pidsley et al. 2016 and McCartney et al. 2016 for 850k

MsetExProbes <- dropXreactiveLoci(mSetSqFlt)

# how many rows were removed
nrow(mSetSqFlt) - nrow(MsetExProbes)

### visualize data after filtering
plotMDS(getM(MsetExProbes), top=100, gne.select ="common",
        col=pal[factor(targets$Sample_Group)], cex=0.8)


# get M- values and beta values for analysis

mVals <- getM(MsetExProbes)
bVals <- getBeta(MsetExProbes)

colMeans(mVals)
colMeans(bVals)

par(mfrow=c(2,1))

densityPlot(mVals, main = "m-values",sampGroups=targets$Sample_Group  ,pal = brewer.pal(8, "Dark2"), xlab = "mVals")
densityPlot(bVals, main = "B-values",sampGroups=targets$Sample_Group  ,pal = brewer.pal(8, "Dark2"),xlab = "bVals")

colMeans(mVals)
colMeans(bVals)

library("mclust")

fitMvals <- Mclust(bVals1000)
summary(bVals)

#######################################
################################3######
#### hierarchical cluster analysis


bVals1000 <- bVals[-c(10000:nrow(bVals)), ]

d <- dist(bVals1000)

fith <- hclust(d, "ward.D2")

plot(fith)

rect.hclust(fith, k = 2, border = "red")

clusters <- cutree(fith, 2)

clusters

plot(bVals1000, col = clusters)




meth <- myImport$Meth
unmeth <- myImport$UnMeth

colMeans(meth)
colMeans(unmeth)





# chacking unnormalized detP values to check for invalid samples

detP <- myImport$detP

barplot(colMeans(detP))
colMeans(detP)
summary(detP)

rgSet2 <- myLoad2$rgSet
