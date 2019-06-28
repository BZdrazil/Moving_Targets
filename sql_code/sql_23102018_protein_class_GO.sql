SELECT
target_dictionary.tid AS target_tid,
protein_classification.pref_name AS protein_class_pref_name,
protein_classification.short_name AS protein_class_short_name,
protein_classification.protein_class_desc,
protein_classification.definition AS protein_class_def,
protein_classification.class_level AS protein_class_level,
go_classification.go_id,
go_classification.parent_go_id,
go_classification.pref_name AS go_pref_name,
go_classification.path AS go_path,
go_classification.aspect AS go_aspect
FROM
target_dictionary,
target_components,
component_sequences,
component_class,
protein_classification,
component_go,
go_classification
WHERE
target_dictionary.tid = target_components.tid AND
target_components.component_id = component_sequences.component_id AND
component_sequences.component_id = component_class.component_id AND
component_class.protein_class_id = protein_classification.protein_class_id AND
component_sequences.component_id = component_go.component_id AND
component_go.go_id = go_classification.go_id




