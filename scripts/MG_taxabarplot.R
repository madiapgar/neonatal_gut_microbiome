## 3-9-23
## attempting to install qiime2R

if (!requireNamespace("devtools", quietly = TRUE)){
  install.packages("devtools")
  }
devtools::install_github("jbisanz/qiime2R", force = TRUE)

## we did it finally!!
library(qiime2R)
library(tidyverse)


# read_csv (read_tsv) underscored read_ functions are
# from the tidyverse bullshit. use them when you can.
# they're quite nice.
#location of metadata
METADATA_FP <- 'MG_metadata.csv'

metadata_fixer <- function(metadata_fp) {
  # we want a function that will get rid of the second
  # line (qiime2 stuff that we don't need)
  # to do this, we need to 
  # only read in first two lines to save time.
  tmpMeta <- read_csv('MG_metadata.csv', n_max = 2)
  mycols <- colnames(tmpMeta)
  
  #reading in metadata file, specifying that we want first two rows skipped
  ## then we're saying that we want 'mycols' to be saved as the column names 
  ## we took 'mycols' from the original metadata file (i.e. one of the rows that
  ## we skipped)
  metadata <- read_csv('MG_metadata.csv', skip = 2, col_names = mycols)
  ## return allows you to return an object as a variable instead of just printing
  ## it 
  return(metadata)
} 


## reading in my filtered table
## we did it!! 
## remember to use list.files() to see what's available in your current working directory 
## added $data to the end so that I can just extract the ASV table from the file and use it to make graphs
svs<-read_qza(file='~/neonatal-gut-microbiome/tax-filtered-MG.qza')$data

## breaks up single string with confidence level that you get from above
read_qza(file='~/neonatal-gut-microbiome/taxonomy-MG.qza')$data %>% 
  parse_taxonomy() -> taxonomy_MG

## need to read metadata file in as a tsv/csv instead of as a txt file 
metadata <- metadata_fixer(metadata_fp = METADATA_FP)

## filtering taxonomy down to genus level ($Genus) to create a taxa barplot
taxasums<-summarize_taxa(svs,taxonomy_MG)$Genus

## go from wide to long format
taxasums %>% 
  as_tibble(rownames = 'taxonomy') %>% 
  gather(-taxonomy, key = sampleid, value = abund) %>% 
  left_join(metadata) %>% 
  group_by(sampleid) %>% 
  mutate(relabund=abund/sum(abund)) %>% 
  separate(taxonomy, into = c('Kingdom', 'Phylum', 'Class', 
                              'Order', 'Family', 'Genus', 
                              'Species'), sep = ' ') -> biom_long


biom_long %>% 
  select(Order, relabund, sampleid) %>% 
  group_by(Order) %>% 
  summarize(prevalence = sum(relabund > 0)/nrow(metadata)) %>% 
  arrange(desc(prevalence)) -> prevalence

PREVALENCE_THRESH <- 0.7

biom_long %>%
  left_join(prevalence) %>% 
  group_by(sampleid, Order, prevalence, age) %>% 
  summarize(relabund = sum(relabund),
            prevalence = sum(prevalence) > 0) %>% 
  filter(prevalence > PREVALENCE_THRESH) %>% 
  ggplot(aes(x = sampleid, y = relabund)) +
    geom_bar(aes(fill = Order), color = 'black',
             stat='identity', position='stack') +
    scale_fill_viridis(option = "D", discrete = TRUE) +
    facet_wrap(~age, scales = 'free_x')

## taking out NA; ?
## filter(prevalence != 'NA;')
