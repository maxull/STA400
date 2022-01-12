##### analysis of FYSAK datasett, Lesson 1

library(foreign); library(tidyverse)

### read SPSS file, .sav filetype



Fysak <- read.spss("data/Datasett 13 FYSAK (2).sav",to.data.frame = TRUE)
