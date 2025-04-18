##Kruskal-Wallis Test to test for change in alpha diversity across 3 groups ##

# Load in libraries
library(readxl)
library(FSA)
library(ggplot2)
library(openxlsx)
library(scales)
library(ggbreak)
library(ggpubr)

# Load in data #
data <- read_excel("Aim1_Baseline_AlphaDiversity.xlsx", sheet = "Onset")

# Adjust dataframe for statistics #
variable <- as.factor(data$ThreeGroup)
Metabolite <- data$observed_otus

# Perform KW test #
test <- kruskal.test(Metabolite ~ variable, data = data)$p.value
print(test)

# Dunn's Test for multiple comparisons, only perform when KW is significant #
dunn <- dunnTest(Metabolite ~ variable,
                 data = data,
                 method = "bonferroni")
print(dunn$res$P.unadj)
print(dunn$res$Comparison)

# Plot Figure #
df <- read_excel("Aim1_Baseline_AlphaDiversity.xlsx", sheet = "RInput_multiplot")

ggplot(data=df,aes(x=factor(Time), y=Level,  fill=Group)) +
  geom_boxplot() +
  ylab("Simpson Alpha Diversity")+
  geom_point(position = position_dodge(width=0.75), aes(group=Group))+
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        legend.text = element_text(size=12)) +
  scale_y_continuous(limits = c(0,45))
