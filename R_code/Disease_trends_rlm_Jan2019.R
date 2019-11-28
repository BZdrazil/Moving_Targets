library(forcats)
library(dplyr)
library(tidyverse)
library(broom)
library(MASS)
library(sfsmisc)

zz <- gzfile("data/Kinase_disgenet_bioactivity_trends.csv.gz", "rt")
d <- read.csv(zz, header=TRUE, as.is=TRUE)
close(zz)
g <- d$Disease %>% unique()
y = 1995:2016

base <- expand.grid(Disease = g, publication_year = y) %>% 
  arrange(Disease)

# preparation
db <- left_join(base, d, by = c("Disease" = "Disease", "publication_year" = "publication_year")) %>% 
  mutate(percentage_bioactivities = ifelse(is.na(percentage_bioactivities), 
                                           0, percentage_bioactivities)) %>%
  group_by(Disease) %>% nest() %>%
  mutate(ave_percentage = map_dbl(.x = data, 
                                  .f = ~ mean(.x$percentage_bioactivities))) %>%
  dplyr::filter(ave_percentage >= 2) %>%
  unnest()

db_go <- db %>% 
  group_by(Disease) %>% 
  nest() %>% 
  mutate(model = map(.x = data, 
                     .f = ~ MASS::rlm(percentage_bioactivities ~ publication_year, 
                                      data = .x)),
         coeff = map_dbl(.x = model, .f = ~ (.x  %>% coefficients())[2]),
         robbie = map(.x = model, .f = ~ f.robftest(.x) %>% tidy()),
         p_value = map_dbl(robbie, "p.value")) %>% 
  dplyr::filter(p_value < 0.05) %>% 
  arrange(-coeff) 

db_go %>% dplyr::select(Disease, data, coeff, p_value) %>% 
  top_n(n = 36, wt = coeff) %>%
  mutate(Disease = fct_reorder(.f = Disease, .x = -coeff)) %>% 
  unnest() %>% 
  ggplot(mapping = aes(x = publication_year, y = percentage_bioactivities)) +
  geom_point() + 
  ggtitle("Kinase: Disease trends") +
  theme(strip.text = element_text(size = 6.0, face = "bold"), plot.title = element_text()) +
  geom_smooth(method = "rlm", formula = y ~ x) + 
  facet_wrap(~Disease, nrow = 6)+
  ylab("% Bioactivities")+xlab("Publication Year")
 
  
ggsave(file = "figs/Ion_channel_rlm_pos_Disgenet_trends_bioactivities.tiff", 
       device = "tiff", dpi=150, width=6*1.75, height=6*1.75)



