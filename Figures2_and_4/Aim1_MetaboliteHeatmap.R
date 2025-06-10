### CReate a correlation heatmap for the metabolites with the outcome groups

library(pheatmap)

df <- read_excel("Aim1_OnsetMetabolites.xlsx", sheet="Heatmap")
rownames(df) <- df$Patient
df <- df[,-1]

table <- cor(df, method='spearman', use = 'pairwise.complete.obs')

t1 <- table[, 4:13]
mat <- t1[1:3,]

pheatmap(mat, show_rownames = T, 
         show_colnames = T, 
         cluster_rows = F, 
         cluster_cols = T,
         fontsize_row = 12,
         fontsize_col = 12,
         angle_col = 45,
         color = colorRampPalette(c("darkred", "white", "darkgreen"))(100),
         legend_labels = T)
