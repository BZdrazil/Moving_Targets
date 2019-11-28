library(tidyverse)
library(reshape2)
library(randomcoloR)
library(ggplot2)

zz <- gzfile('data/GPCR_pancreatic_neoplasm_single_target.csv.gz', 'rt')
Disease_targets <- read.csv(file = zz, header=TRUE, as.is=TRUE)
close(zz)

colourCount = length(unique(Disease_targets$Protein_Name))
getPalette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

g <- ggplot(data = Disease_targets, aes(x = Year, y = Percentage_Bioactivities, fill = Protein_Name)) +
  geom_bar(colour = "black", stat = "identity") + 
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_bw() +
  theme(legend.position="right",legend.text=element_text(size=8), 
        legend.key.size=unit(8,"point") ) +
  guides(fill=guide_legend(nrow=colourCount, title='Protein Name'))+
  ylab("% Bioactivities")+
  ggtitle("GPCR: Pancreatic Neoplasm")

ggsave(file = "figs/GPCR_pancreatic_neoplasm_proteins.tiff", dpi = 300, device = "tiff",
       width=6, height=5/1.618)

