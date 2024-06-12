library(vroom)
library(tidyverse)
library(ggplot2)
library(gridExtra)
data_dir <- "/home/asr/Desktop/ProjStat/Data"
wd <- "/home/asr/Desktop/ProjStat/Code"
setwd(wd)
df <- read.csv(paste(data_dir,"pph-subset2.csv", sep = "/"), stringsAsFactors = TRUE) %>% tibble()

#Print summary, note no missingness.
summary(df)

#No variability in PrevKryo and Prev Multbirth, so remove this from the dataset.
df <- df %>% select(-c(PrevKryo,PrevMultbirth))

#PrevEpisotomi encodes the same information as PrevEpisiotomy, but the latter is coded as a factor. Keep the later
#PrevRBC and PrevRBC12 encode the same information
#PrevMacrosomia4500,PrevMacrosomia45, PrevMultMacro encode the same information 
df <- df %>% select(-c(PrevEpisotomi,PrevRBC12, PrevMacrosomia4500, PrevMultMacro))

#Need to encode intendedCS and PPH as integers for subsequent use. Also mutate columns prev_rbc and prev_timing to snake_case 
df <- df %>% mutate(intendedCS = case_when(intendedCS == "No" ~ 0, 
                              intendedCS == "Yes" ~ 1,
                              .default = 999)) %>% 
  mutate(PPH = case_when(PPH == "No" ~ 0,
                         PPH == "Yes" ~ 1,
                         .default = 999)) %>% 
  mutate(PrevRBC = as.factor(case_when(PrevRBC == "-amount= 0" ~ "amt_0",
                             PrevRBC == "-amount= 1-2" ~ "amt_1_2",
                             PrevRBC == "-amount= 3-5" ~ "amt_3_5",
                             PrevRBC == "-amount= 6+" ~ "amt_6",
                             .default = "amt_na"))) %>% 
  mutate(PrevTiming = as.factor(case_when(PrevTiming == "No PPH" ~ "No",
                                       PrevTiming == "Primary PPH" ~ "Primary",
                                       PrevTiming == "Secondary PPH" ~ "Secondary",
                                       .default = "NA")))

df <- df %>% janitor::clean_names()


saveRDS(df, paste(data_dir,"processeddata.rds", sep = "/"))


