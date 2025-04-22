## THis file contains the code necessary to perform Mann-Whitney tests for two-group comparisons
##    and code to calculate the log 2 fold change and exporting that to be used in the supplemntal info
##    and code to create a volcano plot. 

# Load in libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

####### Mann-Whitney Test ###########
df <- read_excel("Aim1_Volcano.xlsx", sheet = "OnsetMW")

new_df <- subset(df, Group %in% c("SterileNF", "InfectiousNF")) # subset as needed
new_df$Group <- as.factor(new_df$Group)

for( i in new_df[3:112]){
  res <- wilcox.test(i ~ new_df$Group)
  print(res$p.value)
}
# I'm lame and just copied and pasted the p-values, you could totaly save this as a csv or whatevs. 

### Calculate Fold Change
df <- read_excel("Aim1_Volcano.xlsx", sheet = "Onset")

# Change columns to be the correct columns corresponding to the samples within the group of interest
group1mean <- apply(df[61:75], 1, FUN=mean)  #InfectiousNF, InfectiousNF, SterileNF
group2mean <- apply(df[47:60], 1, FUN=mean)  # STerileNF, No fever, No Fever

FC <- group1mean/group2mean
log2FC <- log(FC,2)

pvalue <- read_excel("Aim1_Volcano.xlsx", sheet = "ResultsOnset")
pvalue1 <- pvalue$InfectiousNF_v_SterileNF

vd <- data.frame(df$Group,log2FC, pvalue1)

#Change pvalue significance level in the log10(#)
vd$significant <- "No"
vd$significant[-log10(vd$pvalue1) > -log10(0.05)] <- "Yes"

vd$names <- NA
vd$names[vd$significant != "No" & abs(vd$log2FC) > 1] <- vd$df.Group[vd$significant != "No" & abs(vd$log2FC) > 1]


sig_levels <- c(0.3, 0.05)
colors <- c("blue", "green")

vd$color <- "darkgrey"
for(i in seq_along(sig_levels)){
  vd$color[abs(vd$pvalue1) <= sig_levels[i]] <- colors[i]
}

#removing values with Inf values
vd <- vd %>%
  filter(is.finite(vd$log2FC))

write.csv(vd, "volcano_res.csv") # Include as supplemental data

ggplot(data = vd, aes(x = log2FC, y = -log10(pvalue1), label = names)) +
  geom_point(colour = vd$color, size = 3)+
  scale_fill_manual(values = c("grey", "blue", "green"))+
  xlab("Log2FoldChange") +
  ylab("-Log10Pvalue")+
  theme_classic()+
  geom_text_repel(size = 4, max.overlaps = 30)+
  ggtitle("Sterile NF v No Fever")+
  geom_vline(xintercept=0,linetype=3) +
  lapply(seq_along(sig_levels), function(i) {
    geom_hline(yintercept=-log10(sig_levels[i]), linetype="dashed", color=colors[i])
  }) +
  annotate(geom="text", x=-8.5, y=0.59, label = "p-val=0.3", color = "blue", size =4.5)+
  annotate(geom="text", x=-8.4, y=1.25, label = "p-val=0.05", color = "green", size =4.5)+
  theme(plot.title = element_text(hjust=0.5, size = 20),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11)) +
  coord_cartesian(xlim = c(-10,15))
