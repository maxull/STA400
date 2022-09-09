################################################################
###
###     analysis of repeated hypertrophy methylation data
###
################################################################

library(wateRmelon); library(methylumi);library(FDb.InfiniumMethylation.hg19);library(minfi); library(maxprobes); library(tidyverse);library(ggplot2); library(GEOquery); library(plyr)


########################
###
### fetching data 
###
### had to manually create the CSV file since there was no correct format csv file in the untared folder


list <- as.data.frame(list.files(my_dir))

list <- list %>% 
        filter(row_number()<=80) %>% 
        mutate(Sentrix_ID = substr(list$`list.files(my_dir)`, start = 12, stop = 23)) %>% 
        strsplit(list$`list.files(my_dir)`, split = "_")

write.csv(list, "/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/names.csv", row.names = FALSE)


path <- "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/GSE114763_RAW.tar"

untar(path, exdir = "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/untared")



my_dir <- "/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/memory_of_hypertrophy_data/untared"

zip_file <- list.files(path = my_dir, pattern = "*.gz", full.names = TRUE)

ldply(.data = zip_file, .fun = gunzip)

targets <- read.metharray.sheet(my_dir)

rgSet <- read.metharray.exp(targets = targets)

targets$ID <- paste(targets$Sample_Group,targets$Sample_Name,sep=".")
sampleNames(rgSet) <- targets$ID

print(targets$Basename)
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

dim(detP)       ### all samples passed (dim 866238 x 39)

###############################
###
### functional normalization
###
##################################


mSetSq <- preprocessFunnorm(rgSet)


detP <- detP[match(featureNames(mSetSq), rownames(detP)),]

dim(mSetSq)
### filter p_values <0.01


keep <- rowSums(detP < 0.01) == ncol(mSetSq)


mSetSqFlt <- mSetSq[keep,]
dim(mSetSqFlt)

### if male and female samples, filter out XY chromosomes
### need to install EPIC annotation set

BiocManager::install("IlluminaHumanMethylationEPICanno.ilm10b2.hg19")

library(IlluminaHumanMethylationEPICanno.ilm10b2.hg19)

annEPIC = getAnnotation(IlluminaHumanMethylationEPICanno.ilm10b2.hg19)

keep <- !(featureNames(mSetSqFlt) %in% annEPIC$Name[annEPIC$chr %in%  c("chrX","chrY")])



### filter out SNPs

mSetSqFlt2 <- dropLociWithSnps(mSetSqFlt)

dim(mSetSqFlt2)
### filter out cross reactive probes from Chen et al. 2013, Benton et al. 2015 for 450k data
### and Pidsley et al. 2016 and McCartney et al. 2016 for 850k


MsetExProbes <- dropXreactiveLoci(mSetSqFlt2)

dim(MsetExProbes)

######################################################################
###
### Filtering finnished
###
######################################################################


mVals <- getM(MsetExProbes)


mVals %>% 
        ggplot(aes())







####
####
####




mSetRaw <- preprocessRaw(myLoad$rgSet)

par(mfrow=c(2,1))
densityPlot(getM(myNorm), sampGroups=targets$Sample_Group,
            main="Normalized", legend=TRUE)
densityPlot(getM(mSetRaw), sampGroups=targets$Sample_Group,
            main="Not-Normalized", legend=TRUE)

myLoad <- champ.load(directory = my_dir,
                     arraytype = "EPIC",
                     method = "minfi")

preprocessFunnorm(myLoad$rgSet)

beta <- getBeta(myLoad$rgSet)

myNorm <- champ.norm(beta = beta,
                     rgSet = myLoad$rgSet,
                     method ="FunctionalNormalization")











####################
###
### KEGG pathway
###

BiocManager::install("pathview")
BiocManager::install("gage")
BiocManager::install("gageData")


library(pathview); library(gage); library(gageData); library(ChAMP)

myLoad <- champ.load(my_dir, arraytype = "EPIC")

targets = targets %>% 
        filter(row_number() <= 35)

        
dim(mVals)
        
        
mVals <- getM(MsetExProbes)

myDMP <- champ.DMP(beta = mVals,
                   arraytype = "EPIC")                ### adjusted p value correction method = "Benjamini-Hochberg"


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



