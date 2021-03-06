
# (e.g., 0=white; 1-2=yellow;2-4=orange; 5-6=red; 7-8=purple; 9-10=blue; >10: darkgrey)

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(glue)

top_n_target <- 5
top_n_DISEASE <- 15
filename <- "data/Networks/NEW_percent/Kinase_GPCR_circulatory_system_process_Diseases_target_classes_2006-2016_percent.csv.gz"
plot_title <- "Circulatory System Process: 1995-2005"

zz <- gzfile(filename, 'rt')
raw_data <- read.csv(zz, header=TRUE, as.is = TRUE) %>% 
  select(-Target_class_Connectivity)
close(zz)

# prepare list
id_list <- bind_rows(raw_data %>% select(protein_class_short_name) %>% distinct() %>% rename(id_name = protein_class_short_name) %>% mutate(type = "protein"),
                     raw_data %>% select(diseaseName) %>% distinct() %>% rename(id_name = diseaseName) %>%  mutate(type = "DISEASE")) %>% 
  rownames_to_column("id")

target_data <- raw_data %>% 
  dplyr::select(-diseaseName) %>% 
  distinct() %>% 
  rename(id_name = protein_class_short_name) %>% 
  arrange(-Percentage_Bioactivities) %>% 
  mutate(category = "protein") %>% 
  head(top_n_target)

DISEASE_data <- raw_data %>% 
  group_by(diseaseName) %>% 
  select(-protein_class_short_name) %>% 
  summarise_if(is.numeric, sum) %>% 
  rename(id_name = diseaseName) %>% 
  arrange(-Percentage_Bioactivities) %>% 
  mutate(category = "DISEASE") %>% 
  head(top_n_DISEASE)

nodes <- bind_rows(target_data, DISEASE_data) %>% 
  left_join(id_list, by = c("id_name" = "id_name")) %>% 
  select(id, id_name, everything()) %>% 
  mutate(
    drug_class = cut(
      No_Drug_Efficacy_Target_Annotations,
      breaks = c(-Inf, 0, 5, 10, Inf),
      labels = c("0", "1-5", "6-10", ">10")
    )
  )

links <- raw_data %>% 
  select(protein_class_short_name, diseaseName, Percentage_Bioactivities) %>% 
  distinct() %>% 
  left_join(id_list, by = c("protein_class_short_name" = "id_name")) %>% 
  rename(from = id) %>% 
  left_join(id_list, by = c("diseaseName" = "id_name")) %>% 
  rename(to = id) %>% 
  select(from, to, Percentage_Bioactivities) %>% 
  dplyr::filter(from %in% nodes$id, to %in% nodes$id)

graph <- graph_from_data_frame(d = links, vertices = nodes, directed = FALSE) %>% 
  as_tbl_graph()

# Does assortativity change when we randomize the connections
# mean(sapply(1:1000, function(i) {
#   links_rnd <- links
#   links_rnd$from <- sample(links_rnd$from, size=length(links_rnd))
#   rnd <- graph_from_data_frame(d = links_rnd, vertices = nodes, directed = FALSE)
#   assortativity(rnd, types1=as.factor(V(graph)$type), directed=FALSE)
# }))

colvalues = c("lightblue", "blue", "navy","red")
names(colvalues) <- c("0", "1-5", "6-10", ">10")

ggraph(graph, layout = "in_circle") +
  #geom_node_point(aes(shape = category, color = drug_class, size = 3)) +
  geom_edge_link(aes(edge_alpha = Percentage_Bioactivities))+
  geom_node_point(aes(shape = category, size = Percentage_Bioactivities, fill = drug_class)) +
  theme_graph()+
  ggtitle(plot_title) +
  scale_shape_manual(values = c(21,22)) +
  #geom_node_text(aes(label = id_name), repel = TRUE, nudge_x = 0, nudge_y = -0.05) +
  geom_node_label(aes(label = id_name), repel = TRUE, nudge_x = 0, nudge_y = 0, label.padding = unit(0.15,"lines")) +
  scale_fill_manual(values = colvalues, drop = FALSE) +
  theme(legend.position = "none")
ggsave(file = "figs/Network_immune_system_process_P1.tiff", dpi = 300, 
       device = "tiff")
 



