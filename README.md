#  Data for Differential coding of perception in the world’s languages

Majid et al. (2018) Differential coding of perception in the world’s languages, Proceedings of the National Academy of Sciences of the United States of America. See the [article online](http://www.pnas.org/content/early/2018/10/30/1720419115)

Authors: Asifa Majid, Seán G. Roberts, Ludy Cilissen, Karen Emmorey, Brenda Nicodemus, Lucinda O’Grady, Bencie Woll, Barbara LeLan, Hilário de Sousa, Brian L. Cansler, Shakila Shayan, Connie de Vos, Gunter Senft, N. J. Enfield, Rogayah A. Razak, Sebastian Fedden, Sylvia Tufvesson, Mark Dingemanse, Ozge Ozturk, Penelope Brown, Clair Hill, Olivier Le Guen, Vincent Hirtzel, Rik van Gijn, Mark A. Sicoli, and Stephen C. Levinson.

The folders are organised as follows:

## data

Raw data and combined and cleaned datasets.  `AllData_LoP.csv` holds all responses from all languages in a common format.  `DiversityIndices_ND.csv` is the main data file for Simpson's diversity index for each stimulus within each language.  `SAE_data_frame.rDat` is the cleaned and prepared response type data used in the SAE analyses.


## processing


### collectData.R

Load data from the raw excel sheets and combine into a single data frame (`data/AllData_LoP.csv`).  The raw data is not available in this repository.

### getDiversityMeasures.R

Reads `data/AllData_LoP.csv` and calculates the diversity measures.  It produces `data/DiversityIndices.csv`, where "no description" responses are removed, and `data/DiversityIndices_ND.csv`, where "no description" responses are counted as unique responses.  The file `data/DiversityIndices_ND_withLengths.csv` includes data on the length of responses.

### getEthnography.R

Load ethnographic data from various sources and combine into a single data frame.  Collects data from the raw ethnography files (not available in this repository), and creates `data/ethnography/LoP_ethnography_processed.csv`.


## analyses

Analyses are R scripts in a markdown fiel format, with results compiled to pdf.

###  compareDiversityMeasures{.rmd|.pdf}

Compare the different diversity measures.

### testDiversity{.rmd|.pdf}

Test the relative codability of stimuli by language and domain.

### testEthnography{.rmd|.pdf}

Tests of non-linguistic explanations of the linguistic codability in different domains.

### testSAE{.rmd|.pdf}

Test the distribution of response types over languages and domains (abstract, source-based and evaluative).

### `run_full_SAE_MCMCglmm_model.R`

A sub-part of the testSAE.rmd file that was run on the cluster for convenience.  Generates `results/SAE_mcmc_model_full.rdat`.

## visualisations

Various scripts for making the graphs.

## results

Various graphs and intermediate data.

##
