#####################################################################################################
###
###     epiage for cancer data (thormods cancer project)
###
#####################################################################################################


BiocManager::install("SummarizedExperiment")

library(SummarizedExperiment); library(MEAT); library(wateRmelon); library(ggplot2); library(tidyverse)



###########################
####
#### cancer data


bVals <- read.csv("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/Cancer data normalised B_values for clock analysis_transposed.csv")


### col 1 changed to row names

samp2 <- bVals[,-1]
rownames(samp2) <- bVals[,1]



### estimate horwath clock

epiage_horvath <- agep(samp2, coeff=NULL, method="horvath")



### read participant data

library(readxl);library(tibble)
df <- tibble::rownames_to_column(epiage_horvath, "ID")

participants <- read_xlsx("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/array no & conditions.xlsx")


### combine horvath clock and participant information


epiage <- cbind(df, participants)

epiage <- epiage %>% 
        select(c(1,2,3,5,6,7))



############################3
###
### running MEAT 2.0 on the same data



pheno <- read_xlsx("C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/Phenotypes.xlsx")

cancer <- SummarizedExperiment(assays = list(beta = samp2),
                               colData = pheno)                 ### add chronological age her if you want AAdiff and AAresid, and then add the name in "age_col_name = ..."

cancer2 <- SummarizedExperiment(assays = list(beta = samp2))


### clean beta matwix so it only contains the 18747 elements (CpGs) needed

cancer_clean <- clean_beta(SE = cancer,
                           version = "MEAT2.0")

cancer2_clean <- clean_beta(SE = cancer2,
                            version = "MEAT2.0")

### estimate epigenetic age

epiage_meat <- epiage_estimation(SE = cancer_clean,
                                 age_col_name = NULL,
                                 version = "MEAT2.0")

epiage_meat <- epiage_estimation(SE = cancer2_clean,
                                 age_col_name = NULL,
                                 version = "MEAT2.0")

### add the epi age estimation to the epiage dataframe

DNAmage <- as.data.frame(epiage_meat$DNAmage)

epiage <- cbind(epiage, DNAmage)


### fix dataset for visualization


epiage<- epiage %>% 
        mutate(DNAmage = `epiage_meat$DNAmage`+20,
               Timepoint = factor(TIMEPOINT, levels = c("Pre", "Post")),
               Condition = factor(CONDITION, levels = c("Healthy Aged-matched Trained", "Cancer Untrained", "Cancer Trained")))
### added +20 years to DNAmage estimate because horvath clock also does that to esimate "chronological" age


### boxplot of epiage estimations

epiage %>% 
        ggplot(aes(x = Condition, y = DNAmage, fill = Timepoint))+
        geom_boxplot()+
        theme_classic()

### clean dataset and save

epiage_data <- epiage %>% 
        select(1,2,3,6,7,8,9,10)

write.csv(epiage_data, "C:/Users/maxul/OneDrive/Dokumenter/Skole/Master 21-22/Master/Cancer data/cancer_epiage.csv",
          row.names = FALSE)
