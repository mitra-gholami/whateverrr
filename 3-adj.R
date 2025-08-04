# Created: 15.07.2025
# Updated: 

# This script reads in the binned CoRA adjective data and performs analyses in the Adjective region.
# Two types of analyses are performed. First, we perform analyses on the log-gaze probability of looks to the target vs. all other objects (see Tourtouri et al., 2019).
# The second analysis follows Dale Barr's 2008 paper and blog posts on Multilevel logistic regression (MLR) and aggreagation. 
# For both types of analyses, we define 50 ms time bins over which we aggregate the number of fixations to the areas of interest (referents + background) first
# per participant, and then per item. 

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
library(lme4)
library(lmerTest)
library(data.table)
library(stats)
library(tidyr)
source("summarySE.R") 
source("summarySEwithin.R") 
source("rowShift.R") 

# Read in the adjective time-locked data
setwd(OutputDir)
adj <- readRDS("cora-adjfix.RDS")

adj <- as.data.frame(adj)
head(adj)
summary(adj)

# Add column with inspections to the background
adj <- mutate(adj, bground = ifelse(object=="background",1,0))
head(adj)

# Read them as numeric----why???
adj$target <- as.numeric(adj$Target)
adj$competitor <- as.numeric(adj$Competitor)
adj$contrast <- as.numeric(adj$Contrast.1)
adj$set1 <- as.numeric(adj$Set1)
adj$set2 <- as.numeric(adj$Set2)
adj$single <- as.numeric(adj$Single)

summary(adj)
str(adj)

### CREATE TIME-BINS ### ------------------------------------------------------ 
# Create 50ms bins over which to calculate fixation means
# For the selection of the time bin duration and line of code, see Knoeferle & Kreysa 2012
adj.bin <- arrange(adj, Subject, Trial, TimeFromAdj)
head(adj.bin)

adj.bin$timeBin <- round(adj.bin$TimeFromAdj/50)
adj.bin <- arrange(adj.bin, Subject, Trial, TimeFromAdj)
head(adj.bin)


### COLLAPSE ACROSS CONTRAST CONDITIONS ------------------------------------------------------ 
# Create set column for collapsing
adj.bin$set <- ifelse(adj.bin$object == "target" & adj.bin$Contrast == "contrast", "contrast", 
                     ifelse(adj.bin$object == "target" & adj.bin$Contrast == "nocontrast", "single", 
                            ifelse(adj.bin$object == "competitor" & adj.bin$Contrast == "contrast", "single",
                                   ifelse(adj.bin$object == "competitor" & adj.bin$Contrast == "nocontrast", "contrast",
                                          ifelse(adj.bin$object == "set1", "distractor", ifelse(adj.bin$object == "distractor", "set", 
                                                 ifelse(adj.bin$object == "single", "distractor",10)))))))
adj.bin$set <- as.factor(adj.bin$set)
adj.data <- filter(adj.bin, set != "10") # Use only the single, contrast & distractor 
summary(adj.data)
head(adj.data)

# Assign fixations to the single, contrast objects in the collapsed dataset
scd.fix <- mutate(adj.data, singleton = ifelse(object == "target" & Contrast == "nocontrast", 1, 
                                         ifelse(object == "competitor" & Contrast == "contrast", 1, 0)), 
                 contrasted = ifelse(object == "target" & Contrast == "contrast", 1, 
                                   ifelse(object == "competitor" & Contrast == "nocontrast", 1, 0)),
                 distractors = ifelse(set == "distractor", 1, 0))
head(scd.fix)


# Replace zeros with 0.1 (see Knoeferle and Kreysa 2012)
scd.fix <- mutate(scd.fix, singleton = ifelse(singleton==0, 0.1, singleton), 
                  contrasted = ifelse(contrasted==0, 0.1, contrasted),
                  distractors = ifelse(distractors==0, 0.1, distractors))
summary(scd.fix)

scd.fix$Adjective <- ifelse(scd.fix$Adjective.type=="relative", 0.5, -0.5)

# Save this data for use in the plots script
saveRDS(scd.fix, "cora_adj_for_plots.rds")


# Trim fixations that do not fall within the ROI (fixation start must occur within the ROI)
adj.scd <- filter(scd.fix, TimeFromAdj > 180.5, TimeFromAdj < adjdur + 200.5) 
adj.scd <- arrange(adj.scd, Subject, Trial, TimeFromAdj)
head(adj.scd)

# AGGREGATE BY SUBJECTS -----------------------------------------
# Aggregate only for subjects over the 20-ms bins, as in K&K 2012
# NB: Barr uses AggID to group together values from the same aggregate
# Create dataset with proportions of fixations to the singleton and contrast (DISTRACTORS ARE NOT INCLUDED HERE!)
sc.bySubj <- adj.scd %>% group_by(Adjective,Subject,timeBin) %>% summarise_at(vars(singleton,contrasted),mean)
sc.bySubj <- as.data.frame(sc.bySubj)
summary(sc.bySubj)

# # Divide distactor object values by 2 (cause we cannot compare looks to one object vs. looks to two objects)
# na_scd$distractor_mean <- na_scd$distractor_mean/2
# na_scd$distractor_sum <- na_scd$distractor_sum/2
# summary(na_scd)

sc.bySubj$Subject <- as.factor(as.character(sc.bySubj$Subject))


### PROPORTION OF LOOKS TO THE SINGLETON VS. CONTRAST OBJECT ###
# # Create 100-ms time windows and contrast code them. See K & K 2012 p. 5 for this.
# sc.bySubj$tw <- ifelse(sc.bySubj$timeBin > -12 & sc.bySubj$timeBin < -5, -1.5,
#                        ifelse(sc.bySubj$timeBin > -6 & sc.bySubj$timeBin < 0, -0.5,
#                               ifelse(sc.bySubj$timeBin > -1 & sc.bySubj$timeBin < 5, 0.5,
#                                      ifelse(sc.bySubj$timeBin > 4 & sc.bySubj$timeBin < 11, 1.5, 2))))
# summary(sc.bySubj)
# 
# # Aggregate over the larger time windows
# sc.bySubj.agg <- sc.bySubj %>% group_by(reduction,Subject,tw,Target_feature) %>% summarise_at(vars(single_mean,contrast_mean),funs(mean,sum))
# 
# # Contrast code entropy reduction and feature
# sc.bySubj.agg$red <- ifelse(sc.bySubj.agg$reduction=="high", 0.5, -0.5)
# sc.bySubj.agg$feat <- ifelse(sc.bySubj.agg$Target_feature == "Colour", 0.5, -0.5)


# Calculate the log-gaze probabilities for looks to the Singleton vs. looks to the Contrast
sc.bySubj$sc_log <- with(sc.bySubj, log((contrasted) / (singleton)))
summary(sc.bySubj)


# Model for log-gaze probabilities for Target vs. Competitor including Subject as random effect
sc.bySubj.1 <- lmer(sc_log ~ Adjective + (1 + Adjective | Subject), data=sc.bySubj)
summary(sc.bySubj.1)
print(sc.bySubj.1)

# Check direction of effect
tab <- sc.bySubj %>% group_by(Adjective) %>% summarise(mean = mean(sc_log))
tab <- as.data.frame(tab)
tab

# Adjective        mean
# 1      -0.5 -0.20576233
# 2       0.5 -0.07039617


# new
# Adjective    mean
# 1      -0.5 0.4235
# 2       0.5 0.2537

# library(broom.mixed)
# 
# # Tidy up the fixed effects table
# tidy_table <- tidy(sc.bySubj.1, effects = "fixed", conf.int = TRUE)
# 
# # Save as CSV for easy sharing or Excel use
# write.csv(tidy_table, "MixedModel_FixedEffects.csv", row.names = FALSE)

# install.packages("sjPlot")
# 
# library(sjPlot)
# 
# tab_model(sc.bySubj.1,
#           file = "MixedModel_Report.doc",
#           show.re.var = TRUE,
#           show.icc = TRUE,
#           dv.labels = "Log Fixation Proportion",
#           title = "Mixed-Effects Model Summary")


