# Eye-tracking Data Analysis Problem Set
# Visualizing fixation patterns for adjective processing

# Created: 30.07.2025

# TASK 1: 
# Clean your workspace and set up directories
# Complete the missing code:

rm(list = ls())

# Set paths
CodeDir <- _______________    # Your scripts directory
PlotDir <- _______________    # Your plots directory  
OutputDir <- _____________    # Your data directory

setwd(CodeDir)

# Libraries:
library(plyr)
library(dplyr)      
library(ggplot2)      
source("summarySE.R") 

# TASK 2: Load Data
# Change to the right directory
# Complete the code to load and read the RDS file you saved in Script 3 for creating plots:

setwd(______) 
scd <- ______

# TASK 3: Create Time Bins For The Plots
# Eye-tracking data needs to be binned into time windows for analysis
# The TimeFromAdj variable represents time relative to adjective onset
# Fill in the missing parameters:

scd$bin <- cut(scd$TimeFromAdj,
               breaks = c(-200.5, -100.5, 0.5, 100.5, 200.5, 300.5, 400.5, 500.5, 600.5, 700.5),
               labels = c("-200", "____", "0", "____", "____", "____", "____", "____", "600"),
               right = FALSE)

# TASK 4: Aggregate Data by Conditions
# We need to calculate mean fixations for each condition
# Complete the aggregation formula for contrasted objects (Hint: check the aggregation function for singleton objects)

# For singleton objects 
singleton <- aggregate(singleton~Adjective.type:Subject:Item:bin, scd, FUN = mean)

# For contrasted objects 
contrasted <- ______

# TASK 5: Assign Column Names and Add Condition Labels
# Make the data frames consistent for combining
# Fill in the missing code:

names(singleton) <- c("adjective", "subject", "item", "bin", "fix")
singleton$Object <- as.factor("________")

names(contrasted) <- c("________", "________", "________", "________", "________")
contrasted$Object <- as.factor("Contrast")

# TASK 6: Merge
# Merge the two conditions into one dataset
# Complete the rbind operation:

collapsed <- as.data.frame(rbind(________, ________))
collapsed$subject <- as.factor(collapsed$subject)
collapsed$bin <- as.numeric(as.character(collapsed$bin))
collapsed$adjective <- as.factor(collapsed$adjective)

# Inspect your data structure
summary(collapsed)

# TASK 7: Filter Data by Adjective Type
# Separate absolute and relative adjectives for analysis
# Complete the filter operations:


adj.abs <- filter(________, adjective == "________")
adj.rel <- filter(________, adjective == "________")


detach(package:dplyr)

# TASK 8: Calculate Summary Statistics for collapsed, relative, and absolute datasets 
# Use the summarySE function to get means, standard errors, and confidence intervals
# Fill in the missing parameters:

adj.m <- summarySE(collapsed, measurevar = "fix", groupvars = c("adjective", "Object", "bin"), na.rm = TRUE)

adj.abs.m <- ________(adj.abs, measurevar = "________", groupvars = c("________", "________", "________"), na.rm = TRUE)

adj.rel.m <- ________


# Define colors, line types, and labels for the visualization

labels <- c("Relative" = "Relative adjective", "Absolute" = "Absolute adjective")
lcols = c("#F8766D", "#00BFC4")        # Colors for the two conditions
llines = rep(c('solid', 'dotted'))    # Line types
LegendTitle = "Object"

# TASK 9: Create the Main Plot
# Build a ggplot showing fixation proportions over time
# Fill in the missing ggplot components:

p <- ggplot(data=________, aes(x=bin, y=fix, group=Object)) +
  facet_grid(~________, labeller=labeller(adjective = labels))+
  geom_smooth(aes(ymin=fix-ci, ymax=fix+ci, fill = Object, color = Object, linetype=Object), 
              stat = "identity") +
  geom_vline(xintercept=c(________), linetype='dashed', alpha = .5) +  # Mark critical time windows: add vertical line at the average adjective duration
  scale_colour_manual(values=lcols)+
  scale_linetype_manual(values = llines) +
  ylim(0,0.6) +
  labs(x = "________", y = "________", 
       color = LegendTitle, linetype = LegendTitle) +
  scale_x_continuous(breaks=seq(________,________,by=________)) +  # Make sure your x-axis starts 200 ms before the onset of adjective until 600 ms after 
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=8),
        legend.position = "bottom",
        legend.text = element_text(size = 8, margin = margin(l = 6, unit = "pt")),
        text = element_text(size=10))

# TASK 10: Save the Plot
# Export your visualization as a PDF:

setwd(PlotDir)
ggsave("________", p, width = 174, height = 96, units = "mm")