# PCoA plots and PERMANOVA for Beta Diversity Metrics #

# Load in Libraries
library(readxl)
library(ggfortify)
library(viridis)
library(cluster)
library(vegan)
library(gridExtra)

#################
### Figure 1C ###
#################

# Load in Files
File1 <- read_excel("Aim1_Onset_BetaDiversity.xlsx", sheet = "WeightedUnifrac")
File2 <- read_excel("Aim1_Onset_BetaDiversity.xlsx", sheet = "UnweightedUnifrac")

# File configuring
df1 = data.frame(File1)
df2 = data.frame(File2)

#removes the 'Group' column from the datamatrix, this will get added in later
pam1 = pam(df1[-c(75)], 3)
pam2 = pam(df2[-c(75)], 3)
data1 <- df1[-c(75)]
data2 <- df2[-c(75)]

# Run PERMANOVA Test
# Weighted
set.seed(1)
adon.results1<-adonis2(data1 ~ df1$Group, method = "bray", perm=999)
print(adon.results1)

#Unweighted
set.seed(1)
adon.results2<-adonis2(data2 ~ df2$Group, method = "bray", perm=999)
print(adon.results2)


# Plot PCoA
#Weighted
a <- autoplot(pam1, frame = TRUE, frame.type = 'norm', data = df1, 
              colour = 'Group', size=3, shape=16)+
  scale_colour_manual(values = c("red", "darkgreen", "blue"))+
  scale_fill_manual(values = c("#F8766D", "darkgreen", "#619CFF"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size=12))+
  geom_text(x = -0.15, y = 0.4,
            label =
              "PERMANOVA: 
  p=0.333, R2=0.0313, F=1.1469")+
  ggtitle("Weighted Baseline Unifrac")+
  coord_cartesian(ylim=c(-0.3,0.4))
a
#Unweighted
b <- autoplot(pam2, frame = TRUE, frame.type = 'norm', data = df2, 
              colour = 'Group', size=3, shape=16)+
  scale_colour_manual(values = c("red", "darkgreen", "blue"))+
  scale_fill_manual(values = c("#F8766D", "darkgreen", "#619CFF"))+
  theme_classic()+
  theme(legend.text = element_text(size=12),
        legend.title = element_blank(),
        axis.title = element_text(size=12))+
  geom_text(x = -0.15, y = 0.32,
            label =
              "PERMANOVA: 
  p=0.006, R2=0.0472, F=1.7586")+
  ggtitle("Unweighted Baseline Unifrac")+
  coord_cartesian(ylim=c(-0.45, 0.35))
b

grid.arrange(a,b, ncol=2, widths = c(1,1.3))

#################
### Figure 1B ###
#################

# Load in Files
File1 <- read_excel("Aim1_BetaDiversity.xlsx", sheet = "WeightedBaseline")
File2 <- read_excel("Aim1_BetaDiversity.xlsx", sheet = "UnweightedBaseline")

# Data configuring
df1 = data.frame(File1)
df2 = data.frame(File2)

#removes the 'Group' column from the datamatrix, this will get added in later
pam1 = pam(df1[-c(71)], 3)
pam2 = pam(df2[-c(71)], 3)
data1 <- df1[-c(71)]
data2 <- df2[-c(71)]

# Weighted
set.seed(1)
adon.results1<-adonis2(data1 ~ df1$Group, method = "bray", perm=999)
print(adon.results1)

# Unweighted
set.seed(1)
adon.results2<-adonis2(data2 ~ df2$Group, method = "bray", perm=999)
print(adon.results2)


# Plot PCoA
#Weighted
a <- autoplot(pam1, frame = TRUE, frame.type = 'norm', data = df1, 
              colour = 'Group', size=3, shape=16)+
  scale_colour_manual(values = c("red", "darkgreen", "blue"))+
  scale_fill_manual(values = c("#F8766D", "darkgreen", "#619CFF"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size=12))+
  geom_text(x = -0.15, y = 0.4,
            label =
              "PERMANOVA: 
  p=0.667, R2=0.0211, F=0.7225")+
  ggtitle("Weighted Baseline Unifrac")+
  coord_cartesian(ylim=c(-0.3,0.4))
a
#Unweighted
b <- autoplot(pam2, frame = TRUE, frame.type = 'norm', data = df2, 
              colour = 'Group', size=3, shape=16)+
  scale_colour_manual(values = c("red", "darkgreen", "blue"))+
  scale_fill_manual(values = c("#F8766D", "darkgreen", "#619CFF"))+
  theme_classic()+
  theme(legend.text = element_text(size=12),
        legend.title = element_blank(),
        axis.title = element_text(size=12))+
  geom_text(x = -0.15, y = 0.32,
            label =
              "PERMANOVA: 
  p=0.785, R2=0.0258, F=0.8882")+
  ggtitle("Unweighted Baseline Unifrac")+
  coord_cartesian(ylim=c(-0.45, 0.35))
b

grid.arrange(a,b, ncol=2, widths = c(1,1.3))
