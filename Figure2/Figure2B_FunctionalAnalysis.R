## Perform LinDa analysis, export data to perform MetaCyc annotation, and create heatmap. 

# Load in Libraries
library(readxl)
library(MicrobiomeStat)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(pheatmap)

# Read in picrust raw values
dat <- read_excel("Aim1_Onset_Picrust.xlsx", sheet = "Onset1")
df <- dat[,-1]
rownames(df) <- dat$pathway

# read in metadata sheet
dat2 <- read_excel("Aim1_Onset_Picrust.xlsx", sheet = "metadata1")
meta <- dat2[,-1]
rownames(meta) <- dat2$Patients
meta$Group <- as.factor(meta$Group)

# Calculate LinDa
linda_out <- linda(df, meta,
                   formula = "~ Group",
                   feature.dat.type = "count")

# Export LinDa results into csv file
results_df <- linda_out$output$GroupSterileNF
plot_data <- results_df %>%
  mutate(direction = ifelse(log2FoldChange > 0, "Up", "Down"))
write.csv(plot_data, file="idk.csv")

### Write data to csv. Comparing with the other two 2-comparison groups, I then chose to focus on the top # pathways
# Used MetaCyc to annotate pathways
# Created new datasheet on excel for heatmap. 


# Load in the data

heatdat <- read_excel("Aim1_Picrust.xlsx", sheet = "heatmapNAMED")
df <- heatdat[,-1]
df <- df[,-1]
rownames(df) <- heatdat$Pathway

ann_df <- data.frame(heatdat[,1])
rownames(ann_df) <- rownames(df)
colnames(df) <- c("Infectious NF vs Non-Infectious NF", "Non-Infectious NF vs No Fever", "Infectious NF vs No Fever")

pheatmap(df,
         scale = "none",
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         breaks = c(-1.5, -0.5, 0.5, 1.5),  # Creates exactly 3 categories
         color = c("blue", "white", "red"),  # Colors for Down, No Change, Up
         legend_breaks = c(-1,0,1),
         legend_labels = c("Down", "No Change", "Up"),
         show_rownames = TRUE,
         main = "Top Pathways",
         angle_col = 45,
         fontsize_col = 12,
         cellwidth = 25,
         annotation_row = ann_df)



