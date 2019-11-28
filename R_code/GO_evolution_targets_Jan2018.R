library(tidyverse)
library(reshape2)
library(randomcoloR)
library(ggplot2)

zz <- gzfile("data/Proteases_GO_BP_immune_system_process_bioactivity_trends_proteins.csv.gz", 'rt')
GO_targets <- read.csv(zz, header=TRUE, as.is=TRUE)
close(zz)

colourCount = length(unique(GO_targets$Protein_Class))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

GO_Targets %>% 
  ggplot(aes(x = Year, y = Percentage_Bioactivities, 
             fill = Protein_Class)) +
  geom_bar(colour = "black", stat = "identity") + 
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_bw() +
  theme(legend.text=element_text(size=8), 
        legend.key.size=unit(8,"point")) +
  guides(fill=guide_legend(nrow=colourCount, title='Protein Class'))+
  ylab("% Bioactivities")+ggtitle("Protease: Immune System Process (GO BP)")
ggsave(file = "figs/Proteases_BP_immune_system_process.tiff", dpi = 300, device = "tiff",
       width=6, height=5/1.618)

