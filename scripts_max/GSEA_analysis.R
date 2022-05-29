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

myDMP <- champ.DMP(beta = mVals)                ### adjusted p value correction method = "Benjamini-Hochberg"


df2 = as.data.frame(myDMP$C_to_T) 

df2 = df2[order(abs(df2$logFC), decreasing = TRUE),]

### volcano plot of log FC

ggplot(df2, aes(logFC, -log10(adj.P.Val)))+
        geom_point()





#########################################################
###
### tornado plot of DMP in shores, islands etc.

df2 <- df2 %>% 
        mutate(direction = ifelse(logFC < 0, "hypo (-)", "hyper (+)"))

numb <- df2 %>%
        group_by(cgi) %>%
        summarise(count=n()) 

numb = numb[order(abs(numb$count), decreasing = TRUE),]

positions <- c("shelf","island","shore","opensea")     #### change this order based on decreasing order of "numb"

### hyper and hypo counts for annotation

numb2 <- df2 %>%
        group_by(cgi,direction) %>%
        summarise(count=n()) 

a = numb2[3,3]
b = numb2[4,3]
c = numb2[7,3]
d = numb2[8,3]
e = numb2[1,3]
f = numb2[2,3]
g = numb2[5,3]
h = numb2[6,3]



ggplot(df2, aes(x = logFC, y = cgi, fill = direction))+
        geom_bar(stat = "identity")+
        theme_classic()+
        scale_fill_manual(values = c("red3", "springgreen4"))+          
        scale_y_discrete(limits = positions)+
        annotate(geom = "label", x = 8000, y = 4, label = a)+
        annotate(geom = "label", x = -9700, y = 4, label = b)+
        annotate(geom = "label", x = 6300, y = 3, label = c)+
        annotate(geom = "label", x = -2950, y = 3, label = d)+
        annotate(geom = "label", x = 7800, y = 2, label = e)+
        annotate(geom = "label", x = -1500, y = 2, label = f)+
        annotate(geom = "label", x = 2200, y = 1, label = g)+
        annotate(geom = "label", x = -1850, y = 1, label = h)+           ### labels are number of DMPs in each region, while x is the sum of the logFC (needs to be changed)
        labs(fill = "Methylation")
        
        
        

############################################################
###
### for KEGG pathway i need to map ENTREZ ID to gene name
###
############################################################


BiocManager::install("org.Hs.eg.db")
BiocManager::install("AnnotationDbi")

library(org.Hs.eg.db); library(AnnotationDbi)

columns(org.Hs.eg.db)

df2[!(is.na(df2$gene) | df2$gene==""), ]-> df3

df3$ENTREZ = mapIds(org.Hs.eg.db, 
                    key = as.character(df3$gene), 
                    column = "ENTREZID", 
                    keytype ="ALIAS",
                    multiVals = "first")


##### map out logFC to KEGG pathway



foldchange = df3$logFC

names(foldchange) = df3$ENTREZ

data("go.sets.hs")
data("go.subs.hs")

gobpsets = go.sets.hs[go.subs.hs$BP]            ### BP is for biological processes

gobpdf3 = gage(exprs = foldchange, gsets = gobpsets, same.dir = TRUE)


view(gobpdf3$less)                              ### view less expressed GO pathways C vs. T
view(gobpdf3$greater)                           ### view more expressed GO pathways C vs. T



data("kegg.sets.hs")
data("sigmet.idx.hs")   ### smaller subset dataset that contains only signalling and metabolic pathways

kegg.subset = kegg.sets.hs[sigmet.idx.hs]       ### this now contains only the subset kegg pathwyas

### only signalling and metabolic pathways

keggres = gage(exprs = foldchange, gsets = kegg.subset, same.dir = TRUE)

view(keggres$less)                              ### view less expressed KEGG pathways C vs. T
view(keggres$greater)                           ### view more expressed KEGG pathways C vs. T

### all kegg pathways

keggres2 = gage(exprs = foldchange, gsets = kegg.sets.hs, same.dir = TRUE)

view(keggres2$less)                              ### view less expressed KEGG pathways C vs. T
view(keggres2$greater)                           ### view more expressed KEGG pathways C vs. T


keggrespathways = data.frame(id = rownames(keggres$less), keggres$less) %>% 
        tibble::as.tibble() %>% 
        filter(row_number() <= 20) %>% 
        .$id %>% 
        as.character()
as.data.frame(keggrespathways)

keggresids = substr(keggrespathways, start = 1, stop = 8)
keggresids

tmp = sapply(keggresids, function(pid) pathview(gene.data = foldchange, pathway.id = pid, species = "hsa"))



keggrespathways = data.frame(id = rownames(keggres$greater), keggres$greater) %>% 
        tibble::as.tibble() %>% 
        filter(row_number() <= 20) %>%  ### returns top 20 rows
        .$id %>% 
        as.character()

keggresids = substr(keggrespathways, start = 1, stop = 8)

tmp = sapply(keggresids, function(pid) pathview(gene.data = foldchange, pathway.id = pid, species = "hsa"))









