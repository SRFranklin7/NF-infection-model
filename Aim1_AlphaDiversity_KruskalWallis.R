##Kruskal-Wallis Test
library(readxl)
library(FSA)
library(ggplot2)
library(openxlsx)
library(scales)
library(ggbreak)
library(ggpubr)


df <- read_excel("Aim1_Baseline_AlphaDiversity.xlsx", sheet = "RInput")
data <- read_excel("Aim1_Baseline_AlphaDiversity.xlsx", sheet = "Onset")
variable <- as.factor(data$ThreeGroup)
Metabolite <- data$observed_otus


test <- kruskal.test(Metabolite ~ variable, data = data)$p.value
print(test)


dunn <- dunnTest(Metabolite ~ variable,
                 data = data,
                 method = "bonferroni")
print(dunn$res$P.unadj)
print(dunn$res$Comparison)


ggplot(df, aes(x=Taxa, y=Level)) +
  geom_boxplot(aes(fill=Group)) +
  geom_point(position = position_dodge(width=0.75), aes(group=Group))+
  theme_bw()+
  theme(text = element_text(size =15),
        axis.title.x = element_blank())+
  scale_fill_manual(values=c("darkred","darkgreen", "blue", "darkgrey"))+
  annotate("text", x = c(1), y=8, label = c("KW p=0.05102"))+
  ggtitle("Baseline")

### MUTLIPLE TIME POINTS ###
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
