SELECT
molecule_dictionary.molregno AS molregno_dict,
molecule_dictionary.pref_name AS compound_pref_name,
molecule_dictionary.chembl_id AS compound_chembl_ID,
molecule_dictionary.indication_class,
molecule_dictionary.first_approval,
molecule_dictionary.natural_product,
molecule_dictionary.withdrawn_year,
molecule_dictionary.therapeutic_flag,
compound_structures.standard_inchi,
compound_structures.standard_inchi_key,
compound_structures.canonical_smiles,
compound_properties.alogp,
compound_properties.mw_freebase,
compound_properties.full_mwt,
compound_properties.hba,
compound_properties.hbd,
compound_properties.psa,
compound_properties.rtb AS RotB,
compound_properties.aromatic_rings,
compound_properties.heavy_atoms,
compound_properties.qed_weighted AS QED,
compound_properties.num_ro5_violations,
compound_properties.ro3_pass,
compound_properties.num_alerts,
compound_records.compound_name AS compound_name,
docs.doc_id,
docs.journal,
docs.year AS publication_year,
docs.first_page,
docs.authors,
docs.pubmed_id,
docs.doi,
docs.title,
docs.patent_id,
activities.pchembl_value,
activities.standard_type,
activities.standard_value,
activities.standard_relation,
activities.standard_units,
assays.confidence_score,
target_dictionary.pref_name AS target_pref_name,
target_dictionary.organism AS target_organism,
target_dictionary.chembl_id AS target_chembl_ID,
target_dictionary.target_type,
target_dictionary.tid AS target_tid,
molecule_hierarchy.molregno AS molregno_hierarchy,
molecule_hierarchy.parent_molregno AS molregno_parent
FROM
molecule_dictionary,
compound_structures,
compound_properties,
compound_records,
docs,
activities,
assays,
target_dictionary,
molecule_hierarchy
WHERE
molecule_dictionary.molregno = compound_structures.molregno AND
molecule_dictionary.molregno = compound_properties.molregno AND
molecule_dictionary.molregno = compound_records.molregno AND
compound_records.doc_id = docs.doc_id AND
docs.year >1985 AND
compound_records.record_id = activities.record_id AND
activities.assay_id = assays.assay_id AND
assays.confidence_score >8 AND
activities.standard_units IS NOT NULL AND
assays.tid = target_dictionary.tid AND
target_dictionary.target_type = 'SINGLE PROTEIN' AND
target_dictionary.organism = 'Homo sapiens' AND
molecule_dictionary.molregno = molecule_hierarchy.parent_molregno
FETCH FIRST 10000 ROWS ONLY;





