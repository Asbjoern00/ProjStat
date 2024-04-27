library(vroom)
library(tidyverse)
library(ggplot2)
library(gridExtra)
plot_marginals <- function(df, type = "count", ncol = 6){
  if(type == "count"){
    geom <- geom_col(fill = gray(0.5))
  }
  else{
    geom <- geom_density(fill = gray(0.5))
  }
  lambda <- function(df, col){
#    if (!(col %in% c("age2", "year"))) {
#      count_text <- geom_text(aes(x = `get(col)`, y = mean(count),
#                                  label = paste(round(count*1e-3,0),"K",sep="")), size = 2)
#    } else {
    count_text <- c()
#    }
    df %>% group_by(get(col)) %>% summarise(count = n()) %>%
      ggplot(aes(x = `get(col)`, y = count)) + geom + ylab("") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.2)) +
      count_text + xlab(col)
  }
  lst_plt <- lapply(colnames(df), lambda, df = df)
  n <- length(lst_plt)
  do.call("grid.arrange", c(lst_plt, ncol=ncol))
}

data_dir <- "/home/asr/Desktop/ProjStat/Data"
wd <- "/home/asr/Desktop/ProjStat/Code"
setwd(wd)
df <- read.csv(paste(data_dir,"pph-subset2.csv", sep = "/"), stringsAsFactors = TRUE) %>% tibble()

#Print summary, note no missingness.
#summary(df)

#No variability in PrevKryo and Prev Multbirth, so remove this from the dataset.
df <- df %>% select(-c(PrevKryo,PrevMultbirth))

#PrevEpisotomi encodes the same information as PrevEpisiotomy, but the latter is coded as a factor. Keep the later
df <- df %>% select(-PrevEpisotomi)

#Other notes: 
# - 3 very large observations of PrevTotal, >40 when rest are <30.
# - 2 of the three very large from above also have very large prevSAGM. This makes fine sense in terms of blood loss. Why did the last person not receive this
# - Last person has very large prevFFP as well. This makes good sense.
# - Some very large values of prevTK, but nothing to be really concerned about
# - PrevPlanned CS has two levels, suffix B means planned before labor, suffix A means acute



#Plot marginals, looks ok.
#plot_marginals(df)

saveRDS(df, paste(data_dir,"processeddata.rds", sep = "/"))
