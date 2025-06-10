# Code used to create Figure 3C
# To depict the average variable importance scores for the top variables contributing to model performance

library(tidyverse)
library(ggplot2)
library(readxl)


df <- read_excel("Aim1_BubblePlot.xlsx")

ggplot(df,
       aes(x=Model, y=Variable))+
  geom_point(aes(col=AverageScore, fill=AverageScore, size = AverageScore))+
  theme_minimal()+
  theme(legend.position = 'top',
        axis.title = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.text = element_text(size=10))+
  guides(col = guide_none(),
         size = guide_none(),
         fill = guide_colorbar(
           barheight=unit(0.5, 'cm'),
           barwidth = unit(4,'cm'),
           title.position='top'))+
  labs(fill = 'Average Variable Importance Score')
