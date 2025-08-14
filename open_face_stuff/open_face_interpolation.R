# script to actually interpolate the open face video data 
# libraries
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)
# if (!require(cluster)) {install.packages("cluster")}; library(cluster)
if (!require(saccades)) {remotes::install_github("tmalsburg/saccades/saccades", dependencies=TRUE)}; library(saccades)


# # index frames
list_subjects <- c("SING1005_P1_sing_V2_mask_DG_06-29-2021_L (2)")
db <- read.csv(paste("~/Downloads/", sample(list_subjects, 1), ".csv", sep = ""))
# the maximum frame length we're saying where interpolation is valid
criterion = 5


