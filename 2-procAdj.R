# Created: 15.07.2025
# Updated: 16.07.2025

# The input to this script is the output of data preprocessing (1-preproc.R).
rm(list = ls())
# Set paths
# CodeDir <- "~/IKW/Experiments/CoRA/Analysis/Scripts/"
# PlotDir <- "~/IKW/Experiments/CoRA/Analysis/Plots/"
# OutputDir <- "~/IKW/Experiments/CoRA/Analysis/outputData/"
CodeDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Scripts/"
PlotDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Plots/"
OutputDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/outputData/"


setwd(CodeDir)

# Libraries:
library(plyr)
library(dplyr)
library(ggplot2)
library(car)
library(lme4)
library(lmerTest)
library(boot)
library(psych)
library(reshape2)
library(Rmisc)
library(lattice)
library(data.table)
library(stats)
library(tidyr)
source("summarySE.R") 
source("summarySEwithin.R") 


# Read in the adjective time-locked data
setwd(OutputDir)


fix.adj <- readRDS("cora-adj.RDS")
head(fix.adj)

# Assign fixations to object referents in the visual scene (based on the AOIs) 
fix.adj$AOI <- as.character(fix.adj$AOI)
str(fix.adj)

fix.adj <- mutate(fix.adj, target = ifelse(AOI==Target.position,1,0),
                  competitor = ifelse(AOI==Competitor.position,1,0),
                  contrast = ifelse(AOI==Contrast.position,1,0),
                  set1 = ifelse(AOI==Set1.position,1,0),
                  set2 = ifelse(AOI==Set2.position,1,0), 
                  single = ifelse(AOI==Single.position,1,0))
str(fix.adj)

fix.adj <- arrange(fix.adj, Subject, Trial, TimeFromAdj)
head(fix.adj)

# Create object column specifying which object collects fixations in each point in time
adjobj <- mutate(fix.adj, object = as.factor(ifelse(AOI==Target.position, "target", ifelse(AOI==Competitor.position, "competitor", 
            ifelse(AOI==Contrast.position, "contrast", ifelse(AOI==Set1.position, "set1", ifelse(AOI==Set2.position, "set2", 
            ifelse(AOI==Single.position, "single", "background"))))))))
str(adjobj)

# Create an adjecticve duration variable
adjobj$adjdur <- 0
adjobj <- mutate(adjobj, adjdur = NounOn - AdjectiveOn)

summary(adjobj)

# Select columns
colnames(adjobj)
ADJ <- subset(adjobj, select = c("id", "Subject", "Item", "Trial", "Time", "TimeFromAdj", "Adjective.type",  
                                 "Contrast", "Absolute", "Relative", "Condition", "object", "AdjectiveOn", "adjdur", 
                                 "target", "competitor", "contrast", "set1", "set2", "single"))
# #c("id", "Subject", "Item", "Trial", "Time", "TimeFromAdj", "Adjective.type",  
# "Contrast", "Absolute", "Relative", "Condition", "object", "Adj_on", "adjdur", 
# "target", "competitor", "contrast", "set1", "set2", "single"))

# Sort
ADJ <- arrange(ADJ, Subject, Trial, TimeFromAdj)
head(ADJ)

setwd(OutputDir)
saveRDS(ADJ, file = "cora-adjfix.RDS") 


### ADJECTIVE ### --------------------------------------
# 
# ## 20 ms time bins
# ## Create 20-ms time bins time-locked to adjective onset (120 Hz sampling rate gives one data point per 8.34 ms)
# Bin=20
# TimeFromAdj = seq(-200.5, 2000, by=Bin)
# a = NULL
# for (i in TimeFromAdj) {
#   adjdata = mutate(ADJ, TimeFromAdj = i)
#   a = rbind(a,adjdata)
# }



# # Filter FIXATIONS that occur before or after the Time variable
# ADJt <- filter(a, FixStart < Time, FixEnd > Time)
# ADJt <- arrange(ADJt, Subject, Trial, Time)
# summary(ADJt)


# # Save new data frame
# setwd(OutputDir)
# save(a, file = "ADJt.RData")
# write.csv(a, file = "ADJt.csv", sep = ",")

