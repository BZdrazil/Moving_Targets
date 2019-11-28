library(tidyverse)
library(ggplus)
library(reshape2)

make_figures_tdl <- function(infile, img_file, title) {
  zz <- gzfile(infile, 'rt')  
  d <- read.csv(zz, header=TRUE, as.is=TRUE) %>% 
    dplyr::select(-contains("target", ignore.case = FALSE)) 
  close(zz)
  names(d)[1:3] <- c("Year", "TDL", "Percent_Targets")

  g <- d %>% 
    ggplot(aes(x = Year, y = Percent_Targets, fill = TDL, color = TDL)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(values=c("darkseagreen", "slategray2", "slateblue1", "black")) +
    scale_colour_manual(values=c("white", "white", "white", "black")) +
    theme_bw() +
    theme(text = element_text(size=14))+
    ylab("% Targets")
  g + ggtitle(title)
  ggsave(file = img_file, dpi = 300, device = "tiff", 
         width=6, height=5/1.618)
}

make_figures_tdl("data/Ion_channels_TDLperyear.csv.gz",
                 "figs/TDL_evolution_ion_channels.tiff",
                 "Ion channels: TDL evolution")
make_figures_tdl("data/Transporter_TDLperyear.csv.gz",
                 "figs/TDL_evolution_transporter.tiff",
                 "Transporter: TDL evolution")
make_figures_tdl("data/GPCR_TDLperyear.csv.gz",
                 "figs/TDL_evolution_GPCR.tiff",
                 "GPCR: TDL evolution")
make_figures_tdl("data/Kinase_TDLperyear.csv.gz",
                 "figs/TDL_evolution_Kinase.tiff",
                 "Kinase: TDL evolution")
make_figures_tdl("data/Protease_TDLperyear.csv.gz",
                 "figs/TDL_evolution_Protease.tiff",
                 "Protease: TDL evolution")
make_figures_tdl("data/NR_TDLperyear.csv.gz",
                 "figs/TDL_evolution_NR.tiff",
                 "NR: TDL evolution")
