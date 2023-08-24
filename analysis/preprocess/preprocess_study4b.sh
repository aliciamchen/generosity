mkdir data/study4b/non_anonymized
python preprocess_study4b.py --in_dir data/study4b/raw --out_dir data/study4b/non_anonymized --expt_label study4b

Rscript preprocess_study4b.R

# make the non-anonymized directory