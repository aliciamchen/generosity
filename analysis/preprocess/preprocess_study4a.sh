mkdir data/study4a/non_anonymized
python preprocess_study4a.py --in_dir data/study4a/raw --out_dir data/study4a/non_anonymized --expt_label study4a

Rscript preprocess_study4a.R

# make the non-anonymized directory