# Created: 03.07.2025
# Updated: 08.07.2025

# This script pre-processes the eye-tracking data from the experiment "Comprehension of Redundant Adjectives" (IKW: Sept 2023 - July 2025). 
# It outputs two data files containing pre-processed data: one for the adjective and one for the noun region. 
# This is step 1 of the preprocessing and analysis procedure.

rm(list = ls())

# Set paths
# CodeDir <- "~/IKW/Experiments/CoRA/Analysis/Scripts/"
# GazeDir <- "~/IKW/Experiments/CoRA/Analysis/Data/Gaze/"
# BehDir <- "~/IKW/Experiments/CoRA/Analysis/Data/Behavioural/"
# PlotDir <- "~/IKW/Experiments/CoRA/Analysis/Plots/"
# StimDir <- "~/IKW/Experiments/CoRA/Analysis/Stimuli/"
# ListDir <- "~/IKW/Experiments/CoRA/Analysis/Lists/"
# OutputDir <- "~/IKW/Experiments/CoRA/Analysis/outputData/"

# Set paths
CodeDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Scripts/"
GazeDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Data/Gaze/"
BehDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Data/Behavioural/"
PlotDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Plots/"
StimDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Stimuli/"
ListDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Lists/"
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

setwd(GazeDir)

# Read in data
files.in <- list.files(GazeDir, pattern = "\\.txt$", full.names = TRUE)  # List files
read.files <- lapply(files.in, FUN=read.delim, header = T, sep = "\t")   # Read each file

# Bind files
all.files <- do.call("rbind", read.files) 
gaze.data <- arrange(all.files, Subject, RunningSample, SystemTimestamp)    # Sort data

head(gaze.data)
summary(gaze.data)

# Exclude here any pps that you already know are bad
# read.dt$Participant <- as.character(read.dt$Participant)
# read.dt$Participant[read.dt$Participant == "JXL111"] <- "S17JXL111"
# read.dt$Participant <- as.factor(read.dt$Participant)
# levels(droplevels(read.dt$Participant))

# Get list number
gaze.data <- mutate(gaze.data, List = paste(as.character(substr(ExperimentName,11,11)), as.character(substr(ExperimentName,17,17)), sep="-"))

# Delete irrelevant columns
colnames(gaze.data) 
gaze <- subset(gaze.data, select = -c(1, 3:6, 8:11, 13, 17, 19:20, 22:25, 27:28, 30:51))
gaze <- arrange(gaze, Subject, RunningSample)
head(gaze)

# Rename columns
colnames(gaze)
colnames(gaze)[2] <- "Trial"
colnames(gaze)[4] <- "FixDuration"
colnames(gaze)[5] <- "PositionX"
colnames(gaze)[6] <- "PositionY"


# Only Instruction Slide
gaze <- droplevels(filter(gaze, CurrentObject == "ExpInstruction"))
summary(gaze)


# Fix variable types
str(gaze)
gaze <- mutate(gaze, Subject = as.factor(as.character(Subject)), Trial = as.factor(as.character(Trial)), 
               FixDuration = as.numeric(FixDuration), PositionX = as.numeric(PositionX), PositionY = as.numeric(PositionY), 
               PupilDiameterLeftEye = as.numeric(PupilDiameterLeftEye), PupilDiameterRightEye = as.numeric(PupilDiameterRightEye))

# Checking for drifts           
# p <- qplot(PositionX, PositionY, data=gaze, 
#           geom=c("point"), alpha=0.01, 
#           facets=~Subject,
#           size=FixDuration,
#           xlim = c(0, 1920), ylim = c(0, 1080))
# 
# p + scale_y_reverse() + geom_vline(xintercept = c(330,330+250,582+250,834+250,1086+250,1338+250)) +  geom_hline(yintercept = c(135,135+265,401+250,667+250))  

# p2 = qplot(PositionX, PositionY, data=subset(gaze, Subject=="1"),
#            facets=~Trial,
#            geom=c("point"), alpha=0.5, 
#            size=FixDuration,
#            xlim = c(0, 1920), ylim = c(0, 1080), 
#            label=Trial)
# p2 + scale_y_reverse() + geom_vline(xintercept = c(330,330+250,582+250,834+250,1086+250,1338+250)) +  geom_hline(yintercept = c(135,135+265,401+250,667+250))  

# p3 = qplot(PositionX, PositionY, data=subset(gaze, Subject=="2" & Trial == 214),
#            geom=c("point"), alpha=0.01, 
#            size=FixDuration,
#            xlim = c(0, 1920), ylim = c(0, 1080))  
# p3 + scale_y_reverse() + geom_vline(xintercept = c(330,330+250,582+250,834+250,1086+250,1338+250)) + geom_hline(yintercept = c(135,135+265,401+250,667+250))  
# 

# Create id variable to merge with stims
gaze.for.merge <- mutate(gaze, id = paste(as.character(List),as.character(Trial),sep="-"))


# --- Read in Lists --- 
setwd(ListDir)
files <- dir(ListDir)
lists <- do.call(rbind, lapply(files, read.csv, header = TRUE, sep = ","))
head(lists)


#stims <- filter(lists, block != "Prac") # Get rid of practice trials
lists <- mutate(lists, id = paste(as.character(List),as.character(Number),sep="-")) #Create id variable for merge


# Replace comma with dot
options(digits = 4)         # Fix decimal places

lists$AdjectiveOn <- gsub(",", ".", as.character(lists$AdjectiveOn))    # adjective onset (ms)
lists$AdjectiveOn <- as.numeric(lists$AdjectiveOn)

lists$NounOn <- gsub(",", ".", as.character(lists$NounOn))    # noun onset (ms)
lists$NounOn <- as.numeric(lists$NounOn)


#Rename columns
colnames(lists) 
colnames(lists)[1] <- "Trial"
colnames(lists)[2] <- "Item"
colnames(lists)[3] <- "Condition" 

lists.for.merge <- subset(lists, select = -c(1, 4:7, 9:11, 14:15))      #Get rid of unnecessary columns
gaze.lists <- merge(gaze.for.merge, lists.for.merge, by = "id")     #Merge 
head(gaze.lists)
colnames(gaze.lists)


# --- Read in Stimuli file --- 
setwd(StimDir)
stims <- read.csv("cora-stims.csv", sep = ";", header = T)
head(stims)
colnames(stims)

stimuli <- subset(stims, select = -c(1:4, 7:9, 12:18, 23, 28, 33, 38, 43, 48:50, 52, 56:57))
colnames(stimuli)
colnames(stimuli)[29] <- "Image"
gaze.stim <- merge(gaze.lists, stimuli, by = "Image")     #Merge by image
head(gaze.stim)
gaze.stim <- subset(gaze.stim, select = -c(Adj_on, Noun_on))


fix.conds <- gaze.stim
fix.conds[is.na(fix.conds)] <- 0 #Sets NAs to 0 for the empty object positions, to assign AOIs

# # There will be 6 Warnings that for some variables NAs were not replaced by 0
# # That's OK, as it only concerns Distr1&2 Object, C & P variables that are of type factor

fix.conds$AOI1.x1 = 582
fix.conds$AOI1.x2 = 834
fix.conds$AOI1.y1 = 135
fix.conds$AOI1.y2 = 401

fix.conds$AOI2.x1 = 1086
fix.conds$AOI2.x2 = 1338
fix.conds$AOI2.y1 = 135
fix.conds$AOI2.y2 = 401

fix.conds$AOI3.x1 = 1338
fix.conds$AOI3.x2 = 1590
fix.conds$AOI3.y1 = 401
fix.conds$AOI3.y2 = 667

fix.conds$AOI4.x1 = 1086
fix.conds$AOI4.x2 = 1338
fix.conds$AOI4.y1 = 667
fix.conds$AOI4.y2 = 933

fix.conds$AOI5.x1 = 582
fix.conds$AOI5.x2 = 834
fix.conds$AOI5.y1 = 667
fix.conds$AOI5.y2 = 933

fix.conds$AOI6.x1 = 330
fix.conds$AOI6.x2 = 582
fix.conds$AOI6.y1 = 401
fix.conds$AOI6.y2 = 667


fix.conds$AOI <- 0
fix.aoi <- mutate(fix.conds, AOI = ifelse(PositionX>AOI1.x1 & PositionX<AOI1.x2 & PositionY>AOI1.y1 & PositionY<AOI1.y2, 1, AOI))
fix.aoi <- mutate(fix.aoi, AOI = ifelse(PositionX>AOI2.x1 & PositionX<AOI2.x2 & PositionY>AOI2.y1 & PositionY<AOI2.y2, 2, AOI))
fix.aoi <- mutate(fix.aoi, AOI = ifelse(PositionX>AOI3.x1 & PositionX<AOI3.x2 & PositionY>AOI3.y1 & PositionY<AOI3.y2, 3, AOI))
fix.aoi <- mutate(fix.aoi, AOI = ifelse(PositionX>AOI4.x1 & PositionX<AOI4.x2 & PositionY>AOI4.y1 & PositionY<AOI4.y2, 4, AOI))
fix.aoi <- mutate(fix.aoi, AOI = ifelse(PositionX>AOI5.x1 & PositionX<AOI5.x2 & PositionY>AOI5.y1 & PositionY<AOI5.y2, 5, AOI))
fix.aoi <- mutate(fix.aoi, AOI = ifelse(PositionX>AOI6.x1 & PositionX<AOI6.x2 & PositionY>AOI6.y1 & PositionY<AOI6.y2, 6, AOI))


# no.aoi <- fix.aoi[which(fix.aoi$AOI != fix.aoi$FixIA & fix.aoi$trial_type == "Exp"), ]
# The EyeLink output seems to have corrected for drifts (check manual that this is the case)
# In this case, the FixIA values are more accurate than the AOI values that are calculated above.
# In any case, it's only a subset of the data that the two don't agree on.


### Remove outliers (long fixations) OR NOT?
data <- arrange(fix.aoi, Subject, Trial, SystemTimestamp)
head(data, 20)

qplot(FixDuration,data=data)
hist(data$FixDuration)

### Merge small fixations with previous ones if lay within 12px (ca. 0.5 degrees) cf. Corley & Crocker
# PosDiff calculates the PositionX difference between current and previous fixation
# The first argument is the data to be operated on, the second argument is the group, 
# and the last argument is the function to be applied to the data from each group.
data$PosDiff <- ave(data$PositionX, data$id, FUN=function(x) c(0, abs(diff(x))))

# FixDur adds the duration of the current fixation to the duration of the previous
data$FixDur <- with(data, c(FixDuration[1],FixDuration[-1]+FixDuration[-nrow(data)]))
# but we only want to do this within trials, so we keep the FixDur value only if the current id
# matched the previous id, otherwise we go back to the FixDuration (original) value
setwd(CodeDir)
source("rowShift.R") 
data$previd <- rowShift(data$id,-1)
data$previd[1]<-"1-111"
data$FixDur <- ifelse(data$id == data$previd, data$FixDur, data$FixDuration)

# This line adds the duration of current to previous fixation, but per trial. 
# It groups by id and "run-length-encoding" of FixDuration and creates sequences using the group sizes 
# while multiplying by FixDuration in order to keep the first FixDuration of each Trial
# It uses the library data.table converting the df to dt, so we convert back to df
# But couldn't make it work... 
# setDT(fix.aoi)[, FixDur := seq(.N) * FixDuration, by = .(id, rleid(FixDuration))]
# fix.aoi <- as.data.frame(fix.aoi)

# Get the new FixDur values one row down
# Create vectors with values from previous row for fixation duration and fixation position 
data$FixDur <- ifelse(data$id == data$previd, c(NA, data$FixDur[1:length(data$FixDur)-1]), 10)
data$PrevFixDuration <- ifelse(data$id == data$previd, c(NA, data$FixDuration[1:length(data$FixDuration)-1]), NA)
data$PrevPosDiff <- ifelse(data$id == data$previd, c(NA, data$PosDiff[1:length(data$PosDiff)-1]), NA)

# If previous fixation is shorter than 80ms and lays within 12px from current fixation 
# get the new (summed) duration value, otherwise keep the old one
fixdata <- mutate(data, FixDuration = ifelse(PrevFixDuration<80 & PrevPosDiff<12, FixDur, FixDuration))

# There is no need to do the merge small fixations with the following ones, as there are no small fixations left!

# Delete the now un-necessary columns
colnames(fixdata)
fixdata <- subset(fixdata, select = -c(PosDiff, FixDur, previd, PrevFixDuration, PrevPosDiff))

# Get rid of the small fixations that were not merged with an adjacent longer fixation
fix.long <- filter(fixdata, FixDuration>80)

# Get rid of fixations longer than 2000 ms 
fix.final <- filter(fix.long, FixDuration<2000) ####
hist(fix.final$FixDuration)
summary(fix.final)

# Sort by subject, trial, number.
fix.final <- arrange(fix.final, Subject, Trial, SystemTimestamp)

# Create Time variable (0 = trial onset)
fix.time <- fix.final %>%
  group_by(Subject, Trial) %>%
  arrange(SystemTimestamp) %>%
  mutate(Time = (SystemTimestamp - SystemTimestamp[1])/1000)

fix.time <- as.data.frame(fix.time)
head(fix.time)


# # Fix sound offset (if any)
# fix.off = mutate(fix.final,
#     MessageToSoundOnset = ExpInstruction.OnsetTime - ExpImage.OffsetTime, # The time between Image and Instruction slide
#     InstructionWindow = ExpImage.OffsetTime + MessageToSoundOnset + NounOn + 1000)


# Useful for later on
fix.time <- mutate(fix.time, Condition = as.factor(Condition), Item = as.factor(Item))

# Fix Condition labels
fix.time$Conditions <- fix.time$Condition  #For Exp1
fix.time$Conditions <- as.factor(ifelse(fix.time$Condition=="a", "Absolute-Contrast",ifelse(fix.time$Condition=="b", "Absolute-NoContrast",
                        ifelse(fix.time$Condition=="c", "Relative-Contrast", ifelse(fix.time$Condition=="d", "Relative-NoContrast", fix.time$Condition)))))



###################################################################
### Accuracy check

# Get behavioural data
setwd(BehDir)

# Read data
beh.files <- list.files(BehDir, pattern = "\\.txt$", full.names = TRUE)  # List behavioral files

beh.data.list <- lapply(beh.files, function(f) {
  read.delim(file(f, encoding = "UTF-16LE"), header = TRUE, sep = "\t", fileEncoding = "UTF-16LE")
})

# sapply(beh.data.list, ncol)
# lapply(beh.data.list, names)
# # Get the column names from the first and second file
# colnames1 <- names(beh.data.list[[1]])
# colnames2 <- names(beh.data.list[[2]])
# # Identify the extra column in the first file
# extra.col <- setdiff(colnames1, colnames2)
# # Print the extra column name(s)
# print(extra.col)

# Drop the extra column from the first data frame
beh.data.list[[1]] <- beh.data.list[[1]][ , !(names(beh.data.list[[1]]) %in% "FixPrac.DEVICE")]

# Bind all the data frames
beh.data <- do.call("rbind", beh.data.list)

# Sort by Subject and Trial
beh.data <- arrange(beh.data, Subject, Trial)

# Check the structure and column names
str(beh.data)
summary(beh.data)
head(beh.data, 15)
colnames(beh.data)

# Fix columns and exclude the ones you don't need (everything, exc. accuracy, RTs, and whatever variables you need for id)

# Delete all columns for the practice trials

beh <- subset(beh.data, select = -c(3:63, 66:69,71:109, 111:117, 119, 120))
colnames(beh)
head(beh)
# Get list number
beh <- mutate(beh, List = paste(as.character(substr(ExperimentName,11,11)), as.character(substr(ExperimentName,17,17)), sep="-"))
colnames(beh)
head(beh)

# Create id variable
beh <- mutate(beh, id = paste(as.character(List),as.character(Number),sep="-")) #Create id variable for merge
colnames(beh)
head(beh)
#Rename columns
colnames(beh)[3] <- "Item"
colnames(beh)[5] <- "Trial"
colnames(beh)[6] <- "accuracy"
colnames(beh)[7] <- "rt" 
head(beh)

#beh <- filter(beh, "Item" != "NULL")
beh <- beh[beh$Item != "NULL", ]

head(beh)

# Fix variable types
str(beh)
beh <- mutate(beh, accuracy = as.numeric(accuracy), rt = as.numeric(rt))
head(beh)
# Merge behavioural data with gaze data. Call the new dataset "fix.beh"
beh.for.merge <- subset(beh, select = -c(1:5))      #Get rid of unnecessary columns
str(beh.for.merge)
head(beh.for.merge)


fix.beh <- merge(fix.time, beh.for.merge, by = "id")     #Merge 
head(fix.beh)
colnames(fix.beh)
# ***************************************************************



# Per participant (any super bad ones? super bad = accuracy < 80%)
s.tab <- table(fix.beh$Subject, fix.beh$accuracy)
prop.table(s.tab, margin=1)
# By condition
s.tab.cond <- table(fix.beh$Conditions, fix.beh$accuracy)
prop.table(s.tab.cond, margin=1)

# Filter out the super bad ones (analysis is done on accurate trials)

fix.acc <- filter(fix.beh, )

### STATS FOR RTs ### 
detach(package:dplyr)
rt.1 <- summarySE(fix.acc, measurevar="rt", groupvars=c("Conditions", "Subject", "Item"), na.rm=T)

# Just another way of doing the same thing without summarySE
#rt.1 <- fix.acc %>% group_by(Cond_code,Subject,Target_feature,Item) %>% summarise_at(vars(ExpTask.RT),funs(mean))
#rt.1 <- as.data.frame(rt.1)

# Deviation-coding (centering for experimental factors, following Scheepers can be done with function: scale())
rt.1$Adjective <- ifelse(rt.1$Conditions %in% c("Absolute-Contrast", "Absolute-NoContrast "), 0.5, -0.5)
rt.1$Contrast <- ifelse(rt.1$Conditions %in% c("Absolute-Contrast", "Relative-Contrast"), 0.5, -0.5)
rt.1$AdjectiveXContrast <- rt.1$Adjective * rt.1$Contrast

# Log-transform the Response times
rt.1$logRT <- log(rt.1$rt)

# "Maximal" linear mixed model without covariate; 
rt_m1 <- lmer(logRT ~ Adjective + Contrast + AdjectiveXContrast +
                (1 + Adjective + Contrast + AdjectiveXContrast | Subject) +
                (1 + Adjective + Contrast + AdjectiveXContrast | Item),
                data = rt.1) 
summary(rt_m1)



library(dplyr)
# Check means and SD for Factor 1 
tab <- rt.1 %>% group_by(Adjective) %>% summarise(mean = mean(rt), sd = sd(rt))
tab <- as.data.frame(tab)
tab

# Check means and SD for Factor 2 
tab <- rt.1 %>% group_by(Contrast) %>% summarise(mean = mean(rt), sd = sd(rt))
tab <- as.data.frame(tab)
tab

# Check direction of interaction 
tab <- rt.1 %>% group_by(Adjective, Contrast) %>% summarise(mean = mean(rt), sd = sd(rt))
tab <- as.data.frame(tab)
tab



##############

# Align timing to critical word onset based on ExpImage.OnsetToOnsetTime and Adjective Onset
# Did the sound start playing write away? Were any delays logged?


# Adjective
fix.adj <- mutate(fix.acc, TimeFromAdj=Time-AdjectiveOn) # Time lock to adjective onset

# Noun
fix.noun <- mutate(fix.acc, TimeFromNoun=Time-NounOn) # Time lock to noun onset

# Save new data frames
setwd(OutputDir)
# save(fix.adj, file = "cora-adj.RData")
# save(fix.noun, file = "cora-noun.RData")
saveRDS(fix.adj, file = "cora-adj.RDS")
saveRDS(fix.noun, file = "cora-noun.RDS")
# To load the data again
#load("data.RData")

