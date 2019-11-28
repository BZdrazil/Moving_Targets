library(forcats)
library(dplyr)
library(tidyverse)
library(broom)
library(MASS)
library(sfsmisc)

zz <- gzfile("data/GPCR_GO_BP_bioactivity_trends.csv.gz", "rt")
d <- read.csv(zz, header=TRUE, as.is=TRUE) 
close(zz)

g <- d$go_pref_name %>% unique()
y <- 1995:2016

base <- expand.grid(go_pref_name = g, publication_year = y) %>% 
  arrange(go_pref_name)

# preparation
db <- left_join(base, d, 
                by = c("go_pref_name" = "go_pref_name",
                       "publication_year" = "publication_year")) %>% 
  mutate(percentage_bioactivities = ifelse(is.na(percentage_bioactivities), 
                                           0, 
                                           percentage_bioactivities)) %>%
  group_by(go_pref_name) %>% nest() %>%
  mutate(ave_percentage = map_dbl(.x = data, 
                                  .f = ~ mean(.x$percentage_bioactivities))) %>%
  dplyr::filter(ave_percentage >= 2) %>%
  unnest()

db_go <- db %>% 
  group_by(go_pref_name) %>% 
  nest() %>% 
  mutate(model = map(.x = data, 
                     .f = ~ MASS::rlm(percentage_bioactivities ~ publication_year, 
                                      data = .x)),
         coeff = map_dbl(.x = model, .f = ~ (.x  %>% coefficients())[2]),
         robbie = map(.x = model, .f = ~ f.robftest(.x) %>% tidy()),
         p_value = map_dbl(robbie, "p.value")) %>% 
  dplyr::filter(p_value < 0.05) %>% 
  arrange(-coeff) 

db_go %>% dplyr::select(go_pref_name, data, coeff, p_value) %>% 
  top_n(n = 12, wt = -coeff) %>%
  mutate(go_pref_name = fct_reorder(.f = go_pref_name, .x = coeff)) %>% 
  unnest() %>% 
  ggplot(mapping = aes(x = publication_year, y = percentage_bioactivities)) +
  geom_point() + 
  ggtitle("GPCR: GO biological process trends") +
  theme(strip.text = element_text(size = 6.0, face = "bold"), plot.title = element_text()) +
  geom_smooth(method = "rlm", formula = y ~ x) + 
  facet_wrap(~go_pref_name) +
  xlab("Publication Year")+ylab("% Bioactivities")
  
ggsave(file = "figs/GPCR_rlm_neg_GO_trends_bioactivities_BP.tiff", 
       device = "tiff", width=4*2, height=3*1.75)


