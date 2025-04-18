# Create a waterfall plot/stacked barplot displaying the top 20 classes #

# Load in libraries
library(readxl)
library(ggplot2)

# Load in dataset; already averaged classes across patients within each group
df <- read_excel("Aim1_Waterfall.xlsx", sheet = "RInput")

# Set x-axis group order
level_order <- c('InfectiousNF', 'NoFever', 'NonInfectiousNF')

# Set order of classes
taxa_order <- c("Actinobacteria", "Alphaproteobacteria", "Anaerolineae", "Bacilli", "Bacteroidia", 
                "Blastocatellia", "Campylobacteria", "Clostridia", "Coriobacteriia",
                "Cyanobacteriia", "Deinococci", "Desulfovibrionia", "Fusobacteriia", 
                "Gammaproteobacteria", "Methanobacteria", "Negativicutes", "Synergistia", "Thermoplasmata",
                "Vampirivibrionia", "Verrucomicrobiae", "Other")

#Plot
ggplot(df, aes(x = factor(Group, level = level_order), 
               fill = factor(Taxa, levels = taxa_order),
               y = Abundance))+
  geom_bar(position = "fill", stat = "identity")+
  facet_wrap(~ Time, ncol=2)+
  xlab("") +
  ylab("Average Abundance")+
  theme_bw()+
  theme(axis.text = element_text(size = 10))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size=12)) +
  guides(fill = guide_legend(ncol = 1))+
  scale_fill_manual(name = "Class", values=c("#9f4464", "#d5736f", "#cd4636", "#9a5e2d", "#dd8435",
                                              "#d2a25d", "#bdb22c", "#72732b", "#9fb156", "#478734",
                                              "#62be4a", "#60c185", "#469990", "#388661", "#41c0c7",
                                              "#638dce", "#00008B", "#6f69d8", "#8a61a9", "#bd56c1", "#a9a9a9"))
