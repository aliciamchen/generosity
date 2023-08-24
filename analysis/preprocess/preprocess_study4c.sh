mkdir data/study4c/non_anonymized
python preprocess_study4c.py --in_dir data/study4c/raw --out_dir data/study4c/non_anonymized --expt_label study4c

Rscript preprocess_study4c.R

# make the non-anonymized directory