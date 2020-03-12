CCATrees <- read.csv("data/CCATreesCleaned.csv", header=TRUE, skip = 1)
View(CCATrees)
CCAMeta <- read.csv("output/CCA_meta_AC_mod.csv", header = TRUE)
View(CCAMeta)

CCAMetaTrees <- merge(CCAMeta, CCATrees, by = c("RowTree" = "RowTree"))
nrow(CCAMetaTrees)
CCAMetaTrees.ID <- CCAMetaTrees[,c(1,4,12,13,19,26)]
View(CCAMetaTrees.ID)


CCA.species.info <- read.csv("output/voucher.ID.taxonomic.bioclim.year.sections.info.final.csv", header = TRUE)
View(CCA.species.info)
CCA.AC.metadata <- merge(CCAMetaTrees.ID, CCA.species.info, by.x = "Collector.s..", by.y = "unique_ID")
view(CCA.AC.metadata)
nrow(CCA.AC.metadata)

#write.csv(CCA.AC.metadata, "output/CCA.field.trip.species.list.csv")



## Adding 2016/2019 drought incidence information to the previous dataframe

CCADrought <- read.csv("output/CCA_meta_AC_mod.csv", header = TRUE)
CCASpecies <- read.csv("output/CCA.field.trip.species.list.V.csv", header = TRUE)
View(CCADrought)
colnames(CCADrought)[13] <- "status.2016"
colnames(CCADrought)[14] <- "status.2019"

CCAMergeSpeciesDrought <- left_join(CCASpecies, CCADrought, by = c("RowTree" = "RowTree"))
nrow(CCAMergeSpeciesDrought)
view(CCAMergeSpeciesDrought)


u_CCAMergeSpeciesDrought <-
  CCAMergeSpeciesDrought %>% distinct(CCAMergeSpeciesDrought,RowTree,.keep_all = T)
view(u_CCAMergeSpeciesDrought)
nrow(u_CCAMergeSpeciesDrought)

write.csv(u_CCAMergeSpeciesDrought, "output/CCASpeciesListAlivePostDrought.csv")


u_CCAMergeSpeciesDrought %>% 
  filter %>%
  group_by(, b) %>% group_size()