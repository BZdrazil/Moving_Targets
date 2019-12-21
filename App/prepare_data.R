library(data.table)
library(dplyr)
library(reshape2)

all_data <- fread('mt_data.csv', header=TRUE)

# Load in ChEMBL target annotations
tmap <- fread("MovingTargets/targetmap.csv", header=TRUE)

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
                n_act = No_bioactivities_per_target_and_paper) %>% 
  unique() %>% 
  group_by(chembl_id, year) %>% 
  summarize(n_act = sum(n_act)) %>% 
  ungroup() %>% 
  left_join(target_metadata)

by_protein_family <- all_data %>% 
  dplyr::select(protein_family = protein_familiy, 
                year = publication_year, 
                n_act = No_bioactivities_per_target_and_paper) %>% 
  unique() %>% 
  group_by(protein_family, year) %>% 
  summarize(n_act = sum(n_act)) %>% 
  ungroup()

by_disease <- all_data %>% 
  dplyr::select(diseaseName,
                year = publication_year, 
                n_act = No_bioactivities_per_target_and_paper) %>% 
  unique() %>% 
  group_by(diseaseName, year) %>% 
  summarize(n_act = sum(n_act)) %>% 
  ungroup()

save(by_disease, by_protein_family, by_target,
     target_disease, target_metadata, target_go, 
     file="MovingTargets/datasets.Rda")