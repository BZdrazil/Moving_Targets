library(wesanderson)
library(tidyverse)
library(dplyr)
library(reshape2)
library(stringr)

make_figure <- function(infile, suffix, metric, img_file) {
  
  zz <- gzfile(sprintf("data/%s", infile),'rt')  
  Target_family <- read.csv(zz, header=TRUE, as.is=TRUE) %>% 
    dplyr::select(Year, contains(suffix))
  close(zz)
  
  names(Target_family) <- gsub(suffix, "", names(Target_family))
  melted_Target_family <- melt(Target_family, id.vars = "Year")
  names(melted_Target_family)[1:3] <- c("Year", "Target_family", metric)
  
  melted_Target_family <- melted_Target_family %>% 
    mutate(Target_family = case_when(
      str_detect(as.character(Target_family), "Ion_channels") ~ "Ion Channels",
      TRUE ~ as.character(Target_family)
    ))

  if (str_detect(infile, "percentage")) {
    g <- melted_Target_family %>% 
      ggplot(aes_string(x = 'Year', y = metric, fill = 'Target_family')) +
      geom_bar(stat = "identity") + scale_fill_brewer(palette="Accent", name='Target Family')
  } else {
    g <- melted_Target_family %>% 
      ggplot(aes_string(x = 'Year', y = metric, colour = 'Target_family')) +
      geom_line(size = 1.5) + scale_color_brewer(palette="Accent", name='Target Family')
  }
  
  g + theme_bw() +
    ylab(gsub("_", " ", metric))+
    theme(legend.position="left", text = element_text(size=20))
  
  ggsave(file = img_file, dpi = 300, device = "tiff", width=7, height=4)
}

make_figure("Target_family_evolution_percentage.csv.gz",
                  "_cmp_no_percentage", "Percent_Compounds",
                  "figs/Target_families_compound_percentages.tiff")
make_figure("Target_family_evolution_percentage.csv.gz",
                  "_PMID_no_percentage", "Percent_PMIDs", 
                  "figs/Target_families_PMID_percentages.tiff")
make_figure("Target_family_evolution_percentage.csv.gz",
                  "_target_no_percentage", "Percent_Targets",
                  "figs/Target_families_Target_percentages.tiff")
make_figure("Target_family_evolution_percentage.csv.gz",
                  "_disease_efficacy_percentage", "Percent_Drug_Annotations",
                  "figs/Target_families_Percentage_Drug_Efficacy_Target_Annotations_percentages.tiff")

make_figure("Target_family_evolution.csv.gz",
                  "_cmp_no", "Num_Compounds",
                  "figs/Target_families_compound_numbers.tiff")
make_figure("Target_family_evolution.csv.gz",
                  "_pmid_no", "Num_PMIDs",
                  "figs/Target_families_PMID_numbers.tiff")
make_figure("Target_family_evolution.csv.gz",
                  "_target_no", "Num_Targets",
                  "figs/Target_families_Target_numbers.tiff")
make_figure("Target_family_evolution.csv.gz",
                  "_disease_efficacy", "Num_Drug_Annotations",
                  "figs/Target_families_Drug_Target_Annotation_numbers.tiff")
