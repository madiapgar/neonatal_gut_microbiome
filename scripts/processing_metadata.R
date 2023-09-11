## 9-11-23
## fixing my metadata file and writing it out as a .tsv so I don't have to edit it
## in each script

library(tidyverse)
library(qiime2R)
library(magrittr)

## input file path
metadata_FP <- './data/misc/MG_metadata.csv'

## needed function
metadata_fixer <- function(metadata_fp) {
  tmpMeta <- read_csv(metadata_fp, n_max = 2)
  mycols <- colnames(tmpMeta)
  metadata <- read_csv(metadata_fp, skip = 2, col_names = mycols)
  names(metadata)[names(metadata) == 'cage-number'] <- 'cage_number'
  return(metadata)
} 

## correcting metadata file
metadata <- metadata_fixer(metadata_FP)

## saving output to my data/misc directory
write_tsv(metadata,
          './data/misc/corrected_metadata.tsv')
