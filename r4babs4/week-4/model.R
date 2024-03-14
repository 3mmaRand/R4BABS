
## ---------------------------------------------------------------------
library(flowCore)
library(flowAI)
library(tidyverse)
library(ggExtra)

## ---------------------------------------------------------------------
myfiles <- list.files("r4babs4/week-4/data-raw", pattern = ".fcs$")


## ---------------------------------------------------------------------
fs <- read.flowSet(myfiles, 
                   path = "r4babs4/week-4/data-raw")


## ---------------------------------------------------------------------
fs[[1]] 


## ---------------------------------------------------------------------
exprs(fs[[1]]) |> View()


## ---------------------------------------------------------------------
colnames(fs)


## ---------------------------------------------------------------------
meta <- read_csv("r4babs4/week-4/data-meta/meta.csv")




## ---------------------------------------------------------------------
colnames(fs) <- meta$name
# view the effect
colnames(fs) 


## ---------------------------------------------------------------------
fs_clean <- flow_auto_qc(fs, 
                         html_report = FALSE,
                         folder_results = "sample-QC")

exprs(fs_clean[[1]]) |> View()

## ---------------------------------------------------------------------
trans <- estimateLogicle(fs_clean[[1]],
                         colnames(fs_clean[,c(10, 13)]))


## ---------------------------------------------------------------------
# apply the transformation
fs_clean_trans <- transform(fs_clean, trans)


## ---------------------------------------------------------------------
exprs(fs_clean_trans[[1]]) |> 
  data.frame() |> 
  ggplot(aes(x = TNFa_APC_Lin)) +
  geom_histogram(bins = 100)



## ---------------------------------------------------------------------
# Put into a data frame for ease of use
clean_trans <- fsApply(fs_clean_trans, exprs) |> data.frame() 


## ---------------------------------------------------------------------
clean_trans <- clean_trans |> 
  dplyr::mutate(sample = rep(sampleNames(fs_clean_trans),
                             times = c(dim(fs_clean_trans[[1]])["events"],
                                       dim(fs_clean_trans[[2]])["events"],
                                       dim(fs_clean_trans[[3]])["events"],
                                       dim(fs_clean_trans[[4]])["events"],
                                       dim(fs_clean_trans[[5]])["events"],
                                       dim(fs_clean_trans[[6]])["events"])))


## ---------------------------------------------------------------------
clean_trans <- clean_trans |> 
  extract(sample, 
          remove = FALSE,
          c("treatment", "antibody"),
          "([a-zA-Z]+)_([a-zA-Z]+).fcs")


## ---------------------------------------------------------------------
clean_trans <- clean_trans |> 
  mutate(treatment = fct_relevel(treatment, c("MEDIA",
                                              "LPS",
                                              "ECOLIGreen")))


## ---------------------------------------------------------------------
write_csv(clean_trans, "r4babs4/week-4/data-processed/ai_clean_logicle_trans.csv")



