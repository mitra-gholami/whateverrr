# Created: 15.07.2025
# Updated: 30.07.2025 ***4 main changes ***

rm(list = ls())

# Set paths
CodeDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Scripts/"
PlotDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/Plots/"
OutputDir <- "C:/Users/mitra/Desktop/SP/CoRA/Analysis/outputData/"
setwd(CodeDir)

# Libraries:
library(plyr)
library(dplyr)
library(ggplot2)
source("summarySE.R") 

# Read in the data
setwd(OutputDir)
scd <- readRDS("cora_adj_for_plots.rds")

# Calculate adjective offset (onset + duration) ***
scd <- mutate(scd, AdjOff = AdjectiveOn + adjdur)

# Time bins (starting from -200 ms)
scd$bin <- cut(scd$TimeFromAdj,
               breaks = c(-200.5, -100.5, 0.5, 100.5, 200.5, 300.5, 400.5, 500.5, 600.5, 700.5), #***
               labels = c("-200", "-100", "0", "100", "200", "300", "400", "500", "600"),
               right = FALSE)


singleton <- aggregate(singleton~Adjective.type:Subject:Item:bin, scd, FUN = mean)
contrasted <- aggregate(contrasted~Adjective.type:Subject:Item:bin, scd, FUN = mean)

names(singleton) <- c("adjective", "subject", "item", "bin", "fix")
singleton$Object <- as.factor("Singleton")

names(contrasted) <- c("adjective", "subject", "item", "bin", "fix")
contrasted$Object <- as.factor("Contrast")


collapsed <- as.data.frame(rbind(singleton, contrasted))
collapsed$subject <- as.factor(collapsed$subject)
collapsed$bin <- as.numeric(as.character(collapsed$bin))
collapsed$adjective <- as.factor(collapsed$adjective)

summary(collapsed)


# Filter by adjective type
adj.abs <- filter(collapsed, adjective == "absolute")
adj.rel <- filter(collapsed, adjective == "relative")
 

detach(package:dplyr)
adj.m   <- summarySE(collapsed, measurevar = "fix", groupvars = c("adjective", "Object", "bin"), na.rm = TRUE)
adj.abs.m <- summarySE(adj.abs, measurevar = "fix", groupvars = c("adjective", "Object", "bin"), na.rm = TRUE)
adj.rel.m <- summarySE(adj.rel, measurevar = "fix", groupvars = c("adjective", "Object", "bin"), na.rm = TRUE)



# plot
labels <- c("Relative" = "Relative adjective", "Absolute" = "Absolute adjective")
lcols = c("#F8766D", "#00BFC4")
llines = rep(c('solid', 'dotted'))
LegendTitle = "Object"

# Calculate average adjective offset time from the data ***
adjective_offset <- mean(scd$AdjOff, na.rm = TRUE)
cat("Average adjective offset time:", round(adjective_offset, 1), "ms\n")


# plot
p <- ggplot(data=adj.m, aes(x=bin, y=fix, group=Object)) +
  facet_grid(~adjective, labeller=labeller(adjective = labels))+
  geom_smooth(aes(ymin=fix-ci, ymax=fix+ci, fill = Object, color = Object, linetype=Object), stat = "identity") +
  geom_vline(xintercept=adjective_offset, linetype='dashed', alpha = .5) +  # To add vertical line at adjective offset ***
  scale_colour_manual(values=lcols)+
  scale_linetype_manual(values = llines) +
  ylim(0,0.6) +
  labs(x = "Time from adjective onset (ms)", y = "Proportion of fixations", color = LegendTitle, linetype = LegendTitle) +
  scale_x_continuous(breaks=seq(-200,600,by=100)) +  # Start from -200 ms (, limits=c(-200, 600))
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8),
        legend.position = "bottom",
        legend.text = element_text(size = 8, margin = margin(l = 6, unit = "pt")),
        text = element_text(size=10))


# Save plot as pdf
setwd(PlotDir) # Change directory
ggsave("FixationPlot_by_AdjectiveType.pdf", p, width = 174, height = 96, units = "mm")
