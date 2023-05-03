#bring in library
library(readr)
library(stringr)
library(tidyverse)

#set working directory
setwd("~/Projects/inProgress/2020_hillman/cips")

#reading in from DHS https://studyinthestates.dhs.gov/stem-opt-hub/additional-resources/eligible-cip-codes-for-the-stem-opt-extension

df <- read_csv("stemList2022.txt")
#keep only digits of structure 00.0000
df <-df[grep("\\.[0-9][0-9][0-9][0-9]", df$cip_code), ] 

DHSSTEMCIPS <- as.vector(df['cip_code'])

#remove decimals
DHSSTEMCIPS <- str_replace_all(string = DHSSTEMCIPS$cip_code, pattern = '\\.', '')  

#Now add extras from NSF definition and us considering healthcare and technicians
STEMCIPS <- c(DHSSTEMCIPS, 110000:119999, 140000:159999, 260000:279999, 400000:429999, 450000:459999, 510000:519999, 999999)


cips <- as.data.frame(STEMCIPS) 
cips <- cips %>% distinct()
cips$GROUP <- 'STEM'

cips_ls <- as.list(as.numeric(cips$STEMCIPS))

### CURATED CIPS
curated_cips <- read_csv("ManualCurationMajors.csv")


### ADDINT TEST FRAME
text_cips <- read_csv("text_cips.csv")


text_cips$stem_enroll1 <- as.numeric(text_cips$enrollmentcip1 %in% cips_ls)
text_cips$stem_enroll2 <- as.numeric(text_cips$enrollmentcip2 %in% cips_ls)

text_cips$stem_enroll <- as.numeric(ifelse(text_cips$stem_enroll1 == 1 | text_cips$stem_enroll1 == 1, 1,0))



text_cips$stem_degree1 <- as.numeric(text_cips$degreecip1 %in% cips_ls)
text_cips$stem_degree2 <- as.numeric(text_cips$degreecip2 %in% cips_ls)
text_cips$stem_degree3 <- as.numeric(text_cips$degreecip3 %in% cips_ls)
text_cips$stem_degree4 <- as.numeric(text_cips$degreecip4 %in% cips_ls)


text_cips$stem_degree <- as.numeric(ifelse(text_cips$stem_degree1 == 1 | text_cips$stem_degree2 == 1 | text_cips$stem_degree3 == 1 | text_cips$degreecip4 == 1, 1,0))

test <- text_cips %>% 
  filter(stem_enroll1 == 0)

test <- text_cips %>% 
  filter(stem_enroll1 == 0 | stem_enroll2 == 0 | stem_degree1 == 0 |
           stem_degree2 == 0| stem_degree3 == 0 | stem_degree4 == 0)
