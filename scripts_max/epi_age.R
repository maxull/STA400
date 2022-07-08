################################################################
###                                                          ###
### Get epigenetic age with horvath clock                    ###
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



##################################################################
###
### epigenetic age (horvath pan tissue)
###
###################################################################

bVals <- getBeta(MsetExProbes)


epiage_horvath <- agep(bVals, coeff=NULL, method="horvath")



### add sample names to epiage dataframe

df <- as.data.frame(MsetExProbes$Sample_Name)

ea <- cbind(df, epiage_horvath)

df2 <- as.data.frame(MsetExProbes$Sample_Group)

epiage <- cbind(df2, ea)

epiage <- epiage %>% 
        mutate(Group = `MsetExProbes$Sample_Group`,
               Sample = `MsetExProbes$Sample_Name`) %>% 
        select(Sample, Group, horvath.age)


### boxplot of ages for the groups

epiage %>% 
        ggplot(aes(x = Group, y = horvath.age, fill = Group))+
        geom_boxplot()+
        theme_classic()






################################################################################
################################################################################
################################################################################
###
###     MEAT 2.0 muscle tissue clock
###
################################################################################
################################################################################
################################################################################


library(SummarizedExperiment); library(MEAT)



### get bValues as data frame

bVals <- read.csv("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/Cancer data normalised B_values for clock analysis_transposed.csv")


### if col 1 is the CpGs use code below

samp <- bVals[,-1]
rownames(samp2) <- bVals[,1]

### create Summarized experiment element for the epiage_estimation 
### as seen below it can be run with and without a pheno dataframe

cancer <- SummarizedExperiment(assays = list(beta = samp),
                               colData = pheno)                 

cancer2 <- SummarizedExperiment(assays = list(beta = samp))


### then the CpGs need to be "cleaned" so the dataset only contains the relevant 18747 CpG sites

cancer_clean <- clean_beta(SE = cancer,
                           version = "MEAT2.0")

### calibrate beta values 



cancer_clean_calibrated <- BMIQcalibration(cancer_clean)


### then you estimate the epigenetic age with this function, where the "age_col_name" is optional, 
###     but reqired if you wish to get difference and residuals between chronological age and predicted age


epiage_meat <- epiage_estimation(SE = cancer_clean_calibrated,
                                 age_col_name = NULL,
                                 version = "MEAT2.0")

### get only the age estimations


DNAmage <- as.data.frame(epiage_meat$DNAmage)

### add to earlyer created participant data with horvath clock estimations



epiage <- cbind(epiage, DNAmage)


epiage_data <- epiage %>% 
        select(1,2,3,6,7,8,9,10)

write.csv(epiage_data, "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/cancer_epiage.csv",
          row.names = FALSE)










###########################
####
#### cancer data


bVals <- read.csv("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/Cancer data normalised B_values for clock analysis_transposed.csv")



samp2 <- bVals[,-1]
rownames(samp2) <- bVals[,1]


epiage_horvath <- agep(samp2, coeff=NULL, method="horvath")

### read participant data

library(readxl);library(tibble)
df <- tibble::rownames_to_column(epiage_horvath, "ID")

participants <- read_xlsx("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/array no & conditions.xlsx")

epiage <- cbind(df, participants)

epiage <- epiage %>% 
        select(c(1,2,3,5,6,7))

epiage<- epiage %>% 
        mutate(DNAmage = `epiage_meat$DNAmage`+20,
               Timepoint = factor(TIMEPOINT, levels = c("Pre", "Post")),
               Condition = factor(CONDITION, levels = c("Healthy Aged-matched Trained", "Cancer Untrained", "Cancer Trained")))
        ### added +20 years to DNAmage estimate because horvath clock also does that to esimate "chronological" age

levels(epiage$Timepoint) <- c("Pre", "Post")
levels(epiage$Timepoint)
       
               
epiage %>% 
        ggplot(aes(x = Condition, y = DNAmage, fill = Timepoint))+
        geom_boxplot()+
        theme_classic()
        


############################3
###
### running MEAT 2.0 on the same data


BiocManager::install("SummarizedExperiment")

library(SummarizedExperiment); library(MEAT)

pheno <- read_xlsx("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/Phenotypes.xlsx")

cancer <- SummarizedExperiment(assays = list(beta = samp2),
                               colData = pheno)

cancer2 <- SummarizedExperiment(assays = list(beta = samp2))


cancer_clean <- clean_beta(SE = cancer,
                                 version = "MEAT2.0")

cancer2_clean <- clean_beta(SE = cancer2,
                           version = "MEAT2.0")

cancer_clean_with_gold_mean <- cbind(assays(cancer_clean)$beta,
                                           gold.mean.MEAT2.0$gold.mean)

epiage_meat <- epiage_estimation(SE = cancer_clean,
                                 age_col_name = NULL,
                                 version = "MEAT2.0")

epiage_meat <- epiage_estimation(SE = cancer2_clean,
                                 age_col_name = NULL,
                                 version = "MEAT2.0")

DNAmage <- as.data.frame(epiage_meat$DNAmage)

epiage <- cbind(epiage, DNAmage)


epiage_data <- epiage %>% 
        select(1,2,3,6,7,8,9,10)

write.csv(epiage_data, "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/cancer_epiage.csv",
          row.names = FALSE)
