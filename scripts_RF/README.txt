Scripts to perform random forest model fitting, validation, map creation and other related analysis

1. The site predictors for RF training were created using scripts in .\\RF\\data_prep_examples\\

2. The raw site target AABI for RF training were created using TR-SNP v1.0: 

3. calculate_multi_site_mean.py: get the multi-site mean at the same pixel

Inputs: .\\RF\\ITRDB_us_canada_s1982_multiple_sites.csv *This file defines the sites located at the same pixel of the 0.08333 map
	.\\RF\\delta_bio_mean\\delta_bio_mean\\

Outputs: .\\RF\\delta_bio_mean\\delta_bio_mean_ms_convert\\

4. Git_RF.R: perform random forest model fitting, validation, map creation  and others
Inputs: .\\RF\\input_data_age_corrected.csv *summarized from process 1-3. 
	.\\RF\\map_input\\

Outputs: .\\RF\\FigS5c-5g.pdf
	.\\RF\\FigS9a.pdf

The AABI products were deposited at figshare (https://figshare.com/projects/Tree-ring_based_AABI_in_North_America/153072).
The outputs were summarized as .\\RF\\FigS5a-b.pdf *ED Fig5a-b*

Data for CV related analysis were summarized as .\\RF\\FigS7a-c.tif *ED Fig7a-c*

Data for spatial distribution analysis were summarized as .\\RF\\FigS7d-f *ED Fig7d-f*

Data for age comparison were summarized as .\\RF\\FigS8b-e *ED Fig8b-e*

Data for uncertainty analysis: 
.\\RF\\map_out_boot_full\\       complete output for bootstrap analysis
.\\RF\\map_out_var_all_full\\     complete output for predictor analysis
.\\RF\\map_out_boot_mean\\    data to produce FigS9b 
.\\RF\\map_out_var_all_mean\\      data to produce FigS9c
.\\RF\\FigS9b.tif *ED Fig9b*
.\\RF\\FigS9c.tif *ED Fig9c*
