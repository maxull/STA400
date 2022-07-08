################################################################
###                                                          ###
### Gene Set Enrichment analysis cancer data                 ###
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

install.packages("remotes")
remotes::install_github("xuz1/ENmix")

### now load the libraries


library(wateRmelon); library(methylumi);library(FDb.InfiniumMethylation.hg19);library(minfi); library(maxprobes); library(tidyverse);library(ggplot2); library(ChAMP)
library(pathview); library(gage); library(gageData); library(ChAMP); library(readxl); library(ENmix)


# load normalized pre-processed beta values

bVals <- read.csv("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/Cancer data normalised B_values for clock analysis_transposed.csv")

samp2 <- bVals[,-1]
rownames(samp2) <- bVals[,1]

mVals <- B2M(samp2)

# identify DMPs

participants <- read_xlsx("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/array no & conditions.xlsx")
pheno <- read_xlsx("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/Phenotypes.xlsx")


pheno <- pheno %>% 
        mutate(Group = paste(participants$CONDITION, participants$TIMEPOINT, sep = "_"))

pheno$Group <- sub(" ", "_", pheno$Group)

myDMP <- champ.DMP(beta = mVals,
                   arraytype = "EPIC",
                   pheno = pheno$Group,
                   compare.group = c("Cancer_Trained_Pre", "Cancer_Trained_Post"),
                   adjust.method = "none", 
                   adjPVal = 0.01)                ### adjusted p value correction method = "Benjamini-Hochberg"

myDMR <- champ.DMR(beta = mVals,
                   arraytype = "EPIC",
                   pheno = pheno$Group,
                   compare.group = c("Cancer_Trained_Pre", "Cancer_Trained_Post"))                ### adjusted p value correction method = "Benjamini-Hochberg"


####
####
####   no significant DMPs ??? and DMR not working


df2 = as.data.frame(myDMP$Cancer_Trained_Post_to_Cancer_Trained_Pre) 

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
### all kegg pathways

keggres = gage(exprs = foldchange, gsets = kegg.sets.hs, same.dir = TRUE)

view(keggres$less)                              ### view less expressed KEGG pathways C vs. T
view(keggres$greater)                           ### view more expressed KEGG pathways C vs. T



keggrespathways = data.frame(id = rownames(keggres$greater), keggres$greater) %>% 
        tibble::as.tibble() %>% 
        filter(id == "hsa04110 Cell cycle") %>% 
        .$id %>% 
        as.character()
as.data.frame(keggrespathways)

keggresids = substr(keggrespathways, start = 1, stop = 8)
keggresids

tmp = sapply(keggresids, function(pid) pathview(gene.data = foldchange, pathway.id = pid, species = "hsa"))


##################
####
#### KEGG analysis of hypo methylated island DMPs

df4 <- df2 %>% 
        filter(cgi == "island")


df4[!(is.na(df4$gene) | df4$gene==""), ]-> df5

df5$ENTREZ = mapIds(org.Hs.eg.db, 
                    key = as.character(df5$gene), 
                    column = "ENTREZID", 
                    keytype ="ALIAS",
                    multiVals = "first")


##### map out logFC to KEGG pathway



foldchange = df5$logFC

names(foldchange) = df5$ENTREZ

data("go.sets.hs")
data("go.subs.hs")

gobpsets = go.sets.hs[go.subs.hs$BP]            ### BP is for biological processes

gobpdf3 = gage(exprs = foldchange, gsets = gobpsets, same.dir = TRUE)


view(gobpdf3$less)                              ### view less expressed GO pathways C vs. T
view(gobpdf3$greater)                           ### view more expressed GO pathways C vs. T



data("kegg.sets.hs")
### all kegg pathways

keggres = gage(exprs = foldchange, gsets = kegg.sets.hs, same.dir = TRUE)

view(keggres$less)                              ### view less expressed KEGG pathways C vs. T
view(keggres$greater)                           ### view more expressed KEGG pathways C vs. T



keggrespathways = data.frame(id = rownames(keggres$greater), keggres$greater) %>% 
        tibble::as.tibble() %>% 
        filter(id == "hsa04110 Cell cycle") %>% 
        .$id %>% 
        as.character()
as.data.frame(keggrespathways)

keggresids = substr(keggrespathways, start = 1, stop = 8)
keggresids

tmp = sapply(keggresids, function(pid) pathview(gene.data = foldchange, pathway.id = pid, species = "hsa"))

