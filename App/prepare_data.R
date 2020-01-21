library(data.table)
library(dplyr)
library(reshape2)

all_data <- fread('mt_data.csv.gz', header=TRUE)

# Load in ChEMBL target annotations
tmap <- fread("targetmap.csv", header=TRUE)

# Get unique target metadata from all_data
target_metadata <- all_data %>% 
  dplyr::select(chembl_id=target_chembl_id, protein_class_pref_name, 
                protein_family=protein_familiy, target_pref_name) %>% 
  unique() %>% 
  left_join(tmap)

target_go <- all_data %>% 
  dplyr::select(chembl_id=target_chembl_id, go_term = go_pref_name) %>% 
  unique()

target_disease <- all_data %>% 
  dplyr::select(chembl_id=target_chembl_id, disease = diseaseName) %>% 
  unique()

# Activity counts by target, per year, with target metadata added in
by_target <- all_data %>% 
  dplyr::select(chembl_id = target_chembl_id, 
                year = publication_year, 
                n_act = No_bioactivities_per_target_and_paper, doc_id) %>% 
  unique() %>% 
  group_by(chembl_id, year) %>% 
  summarize(n_act = sum(n_act), n_doc = length(unique(doc_id))) %>% 
  ungroup() %>% 
  left_join(target_metadata)

by_protein_family <- all_data %>% 
  dplyr::select(target_chembl_id,
                protein_family = protein_familiy, 
                year = publication_year, 
                n_act = No_bioactivities_per_target_and_paper) %>% 
  unique() %>% 
  group_by(protein_family, year) %>% 
  summarize(n_act = sum(n_act)) %>% 
  ungroup()

by_protein_class <- all_data %>% 
  dplyr::select(target_chembl_id,
                protein_class = protein_class_pref_name,
                year = publication_year, 
                n_act = No_bioactivities_per_target_and_paper) %>% 
  unique() %>% 
  group_by(protein_class, year) %>% 
  summarize(n_act = sum(n_act)) %>% 
  ungroup()

by_go_bp <- all_data %>% 
  dplyr::select(target_chembl_id,
                go_bp = go_pref_name,
                year = publication_year, 
                n_act = No_bioactivities_per_target_and_paper) %>% 
  unique() %>% 
  group_by(go_bp, year) %>% 
  summarize(n_act = sum(n_act)) %>% 
  ungroup()


by_disease <- all_data %>% 
  dplyr::select(target_chembl_id, 
                diseaseName,
                year = publication_year, 
                n_act = No_bioactivities_per_target_and_paper) %>% 
  unique() %>% 
  group_by(diseaseName, year) %>% 
  summarize(n_act = sum(n_act)) %>% 
  ungroup()

save(by_disease, by_protein_family, by_target, by_protein_class, by_go_bp,
     target_disease, target_metadata, target_go, 
     file="MovingTargets/datasets.Rda")


if (FALSE) {
  
  d0 <- subset(by_disease, year >= 1998 & year <= 2017)
  totals <- d0 %>% group_by(year) %>% summarize(total_act = sum(n_act))
  d1 <- subset(by_disease, diseaseName %in% 'Intellectual Disability'  & 
                 year >= 1998 & year <= 2017)
  
  
  foo <- all_data %>% 
    dplyr::select(
                  target_chembl_id,
                  diseaseName,
                  year = publication_year, 
                  n_act = No_bioactivities_per_target_and_paper) %>% 
    unique() %>% 
    group_by(diseaseName, year) %>% 
    summarize(n_act = sum(n_act)) %>% 
    ungroup()
  
  subset(foo, year == 1998 & diseaseName == 'Intellectual Disability')
  
  totals <- foo %>% group_by(year) %>% summarize(total_act = sum(n_act))
  subset(totals, year == 1998)
  
  d1 <- d1 %>% 
    left_join(target_disease, by=c('diseaseName'='disease')) %>% 
    left_join(by_target, by='chembl_id') %>% 
    dplyr::select(year=year.x, n_act=n_act.y, protein_family) %>% 
    group_by(protein_family, year) %>% 
    summarize(n_act = sum(n_act)) %>% 
    ungroup()
  # Get percentages by year
  d2 <- d1 %>% group_by(year) %>% summarize(s_act = sum(n_act))
  d1 <- d1 %>% left_join(d2) %>% mutate(p_act = round(100*n_act/s_act,1))
  
  t1 <- all_data %>% 
    dplyr::filter(diseaseName == 'leukemia') %>% 
    dplyr::select(year=publication_year, target_chembl_id, protein_familiy, 
                  n_act=No_bioactivities_per_target_and_paper) %>% 
    unique() %>% 
    group_by(year, protein_familiy) %>% 
    summarize(s_act = sum(n_act))
  t2 <- t1 %>% group_by(year) %>% summarize(year_total = sum(s_act))
  t1 <- t1 %>% left_join(t2) %>% mutate(p_act = s_act/year_total)
  t1 %>% 
    ggplot(aes(x=year, y=p_act, fill=protein_familiy))+
    geom_bar(stat='identity')
    
  
  t1 <- target_disease %>% 
    dplyr::filter(disease == 'leukemia') %>% 
    left_join(by_target, by='chembl_id') %>% 
    dplyr::select(year, n_act, protein_family)
  t2 <- t1 %>% group_by(year) %>% summarize(year_total = sum(n_act))
  t1 <- t1 %>% left_join(t2) %>% mutate(p_act = n_act/year_total)
  t1 %>% 
    ggplot(aes(x=year, y=p_act, fill=protein_family))+
    geom_bar(stat='identity')
  
}