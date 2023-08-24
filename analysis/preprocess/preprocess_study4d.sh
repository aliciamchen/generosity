mkdir data/4d_pilot/non_anonymized
python preprocess_study4d.py --in_dir data/4d_pilot/raw --out_dir data/4d_pilot/non_anonymized --expt_label study4d

Rscript preprocess_study4d.R

# make the non-anonymized directory