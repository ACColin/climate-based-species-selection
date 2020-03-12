####        STEP 2: TAXONOMY DATA INPUT & FILTERING

librarian::shelf(sp, raster, tidyverse, readxl, plyr)
library(sp)
library(raster)
library(stats)
library(nycflights13)
library(plyr)
library(readxl)
library(tidyverse)

detach("package:raster", unload=TRUE)
cca_worldclim<-read.csv("output/CCA_worldclim_df.csv",header=T,na = "-")
cca_meta<-read.csv("data/CCA_meta.csv",header=T,na = "-")

#view(cca_worldclim)
#view(cca_meta)

# create subset of CCA meta with only taxa / unique_ID / offspring tree reps year of plantation for subsequent filtering

colnames(cca_meta)[1]<-"species"
colnames(cca_meta)[3]<-"unique_ID"
colnames(cca_meta)[18]<-"year"
#view(cca_meta)

sp_id <- select(cca_meta,"species","unique_ID","year")

#view(sp_id)
nrow(sp_id)

# merging df from STEP 1 with sp_id for year of plantation
merged.dataframe<-read.csv("output/CCA_merged_df.csv")
colnames(merged.dataframe)[22]<-"unique_ID"
#view(merged.dataframe)
all.df<-merge(merged.dataframe,sp_id, by='unique_ID')
#(all.df)
write.csv(all.df, "output/CCA.dataframe.all.species.csv")
?write.csv

# ----- To make the original script work, ignore this section and start with clean environment from script #1 -------

### Which species in the old taxonomy are not found in the cleaned taxonomy? --------------------------

u_all.df<-
  all.df %>% distinct(unique_ID,.keep_all = T)
nrow(all.df)
nrow(u_all.df)
view(u_all.df)
write_csv(u_all.df, "output/u_all.df.csv")
taxa_raw <- read_xlsx("data/DNTaxonomyCleaned.xlsx", skip = 1, na = "-")
view(taxa_raw)

missing.old.sp <- anti_join(u_all.df, taxa_raw, by = c("species" = "Binomial"))
view(missing.old.sp)
nrow(missing.old.sp)

### Which species in the cleaned taxonomy are not found in the old taxonomy? --------------------------

missing.new.sp <- anti_join(taxa_raw, u_all.df, by = c("Binomial" = "species"))
view(missing.new.sp)
nrow(missing.new.sp)

# So there is more than 300 sp. that have a different name between the old and new taxonomy basically
# A lot of inconsistency in the species name using characters like:
# '', x (letter) or × (cross symbol), with ' - ' and '-', upper AND lower case inconsistency
# The hybrids: change symbol to letter in DNTaxoCleaned, I don't trust symbols in R
# Sometimes there is descriptions in brackets () after the species name, will be moved to an additional decription column
# Need to check what doesn't match after that (compare row names) and do it manually.
# Should work by sorting in alphabetical order after doing all the cleaning w/ stringr.
# The unmatching rows should result from taxonomy change for a species.
#symbol: × & letter: x

# Let's start...
 
taxa_raw.clean <- read_xlsx("output/DNTaxonomyCleaned.cleaned.xlsx", skip = 1, na = "-")
u_all.df.clean <- read.csv("output/u_all.df.clean.csv", header = TRUE)

u_all.df$species <- str_to_lower(u_all.df$species)
taxa_raw.clean$Binomial <- str_to_lower(taxa_raw$Binomial)
u_all.df$species <- str_split_fixed(u_all.df$species, "[(]", 2)
#u_all.df$species <- str_replace_all(u_all.df$species, "'", "")
df <- view(u_all.df.clean)
taxa <- view(taxa_raw.clean)
nrow(u_all.df.clean)
nrow(taxa_raw.clean)
# -------------------------------------------------------------------------------------------------------------------

# merge all df: adding the full taxonomy description to previous df
taxa_raw <- read_xlsx("data/DNTaxonomyCleaned.xlsx", skip = 1, na = "-")
view(taxa_raw)
full.df <- left_join(u_all.df.clean, taxa_raw.clean, by = c("species" = "Binomial"))
view(full.df)
nrow(full.df)
#adding taxonomy not possible for now so replaced by old taxo: basically all.df changed name to full.df to work the following script
full.df <-all.df
# filter unique_ID again
u_full.df<-
  full.df %>% distinct(unique_ID,.keep_all = T)
#view(u_full.df)
colnames(u_full.df)[4]<-"Lat"
u_full.df[[24]]<-NULL
nrow(u_full.df)
min(u_full.df$Long)

write.csv(u_full.df, "output/CCA.species.info.full.dataframe.final.csv")

#view(u_full.df)
nrow(u_full.df)

# Filter for:
#     - Sections: Maidenaria, Exsertaria, Adnataria & Eucalyptus
#     - Year window: 1994 - 2000 (planted roughly at the same time, around 6 years before first drought at CCA to ensure good implantation and growth prior drought) (1994 - 2000 also include the biggest blocks, 1993 and 2001 can be included if needed -> check CCA blocks map)

cca_filtered <- 
  u_full.df %>% 
  filter(Section %in% c("Maidenaria", "Eucalyptus", "Exsertaria", "Adnataria"),
         between(year, 1994, 2001))
view(cca_filtered)

# output: a csv file with species / unique ID / bioclim data / taxonomic info
write.csv(cca_filtered, "output/voucher.ID.taxonomic.bioclim.year.sections.info.csv")

# More thorough description of the df... cca_filtered is a dataframe which displays:
#       - each CCA voucher tree (unique_ID)
#       - with the Lat and Longs of sample site
#       - and the bioclim variables associated with the sample sites.
# In the dataframe are only selected the mother trees within the Exsertaria, Maidenaria, Eucalyptus and Adnataria sections and when the seeds were planted.
# Only the seeds planted between 1994-2000 are shown to select trees that were planted at CCA during a general same period of time, at least 6 years before 2006's drought to ensure the trees were strong enough to endure the dry period.
# The dataframe doesn't show all trees (replicates) at CCA, only the mother tree.



#       DATA VISUALS

#plot climate means
plot.mean.section<-ggplot(data = cca_filtered, mapping = aes(x = BIO1, y = BIO12)) + 
  geom_point(mapping = aes(color = Section))+
  facet_grid(Section ~ .)

print(plot.mean.section)
dev.print(pdf, 'figs/plot_bioclim_mean_CCA_sections.pdf')

plot.mean<-ggplot(data = cca_filtered, mapping = aes(x = BIO1, y = BIO12)) + 
  geom_point(mapping = aes(color = Section))
print(plot.mean)

#plot climate extremes
plot.extreme.section<-ggplot(data = cca_filtered, mapping = aes(x = BIO5, y = BIO14)) + 
  geom_point(mapping = aes(color = Section))+
  facet_grid(Section ~ .)

print(plot.extreme.section)

plot.extreme<-ggplot(data = cca_filtered, mapping = aes(x = BIO5, y = BIO14)) + 
  geom_point(mapping = aes(color = Section))

print(plot.extreme)
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA.pdf')


##### Adding flora traits
# Flora traits extracted from bibliography by Desi Quintans (PhD student in Paul's lab group)
euc_trait_table <- 
    read.csv("data/euc_trait_table.csv", header = T) %>% 
    mutate(species_name = 
               paste(binomen, "subsp.", subsp, "var.", variety) %>% 
               str_remove_all(" (var|subsp)\\. NA"))

cca_filtered_trait <- left_join(cca_filtered, euc_trait_table, by = c("species" = "species_name"))

write_csv(cca_filtered_trait, "output/cca_filtered_trait.csv")



# What species in CCA are not accounted for in the Flora book? ------------

missing_spp_in_cca <- 
    anti_join(cca_filtered, euc_trait_table, by = c("species" = "species_name")) %>% 
    distinct(species, .keep_all = TRUE) %>% 
    glimpse()

missing_spp_in_flora <-
    anti_join(euc_trait_table, cca_filtered, by = c("species_name" = "species")) %>% 
    distinct(species_name, .keep_all = TRUE) %>% 
    glimpse()

spp_in_both_tables <- 
    semi_join(cca_filtered, euc_trait_table, by = c("species" = "species_name")) %>% 
    distinct(species, .keep_all = TRUE) %>% 
    glimpse()

count(spp_in_both_tables, Section)

# How many are missing in each section?

count(missing_spp_in_cca, Section) %>% View

########################################