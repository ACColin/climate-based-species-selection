library(raster)
library(sp)
library(stats)
r <- getData("worldclim",var="bio",res=2.5)
?getData
r <- r[[c(1:19)]]
names(r) <- c("BIO1","BIO2","BIO3","BIO4","BIO5","BIO6",
              "BIO7","BIO8","BIO9","BIO10","BIO11","BIO12",
              "BIO13","BIO14","BIO15","BIO16","BIO17","BIO18","BIO19")


#coords <- read.csv("Sites GPS and climate variables.csv")
coords <- read.csv("data/CCA_meta.csv", header=T)
head(coords)
coords$LatD <- as.numeric(as.character(coords$LatD))
coords$LatM <- as.numeric(as.character(coords$LatM))
coords$LatS <- as.numeric(as.character(coords$LatS))

coords$LongD <- as.numeric(as.character(coords$LongD))
coords$LongM <- as.numeric(as.character(coords$LongM))
coords$LongS <- as.numeric(as.character(coords$LongS))

#dms2dd(dd, mm, ss, ns)

coords$Lat <- as.numeric(coords$LatD) + as.numeric(coords$LatM/60) + as.numeric(coords$LatS/60^2)
coords$Long <- as.numeric(coords$LongD) + as.numeric(coords$LongM/60) + as.numeric(coords$LongS/60^2)
head(coords)
coords$Lat <- coords$Lat*-1 # make the southern lats negative
head(coords)

coords.all <- coords[,c("Collector.s..",'Lat','Long')]
coords <- na.omit(coords)
head(coords.all)
coords.ll <- na.omit(coords.all)
coords.ll <- coords.ll[,c(2,3)]
plot(coords.ll$Long, coords.ll$Lat)
?subset
coords.sub <- subset(coords.ll, Long > 100) # remove the weird numbers in the data set (keeping only samples in Aus)
min(coords.sub$Long)
plot(coords.sub$Long, coords.sub$Lat)

coords.sub <- coords.sub[, c(2, 1)]

#getting our decimal degress points ready for spatial projection
#the few next lines of code are for creating the spatial points based on the lat and longs of the tree origin (coords.sub)
points <- SpatialPoints(unique(coords.sub), proj4string = r@crs)
plot(points)
head(points)
###### extracting the rasters to points
#the the raster points are extracted based on the object 'points' which are the tree origin points coords using r which are the bioclim variables. check the ?getData for more information on raster extraction
values <- raster::extract(r, points)

######Stuff not working... df nrow don't match, also chunk of Collin's script that has nothing to do with my stuff
#dataframe <- cbind.data.frame(coords,values)
##key <- read.table("popKEY.txt", header = FALSE)
#head(dataframe     )
#dataframe
#col.sites <- dataframe
#collected <- cbind(coords, col.sites)
#collected.sites <- collected[-c(2,3)]
#head(collected.sites)


##in that part we enter the bioclim values in a dataframe called all.sites.
#then we have to match the number of rows between the all.sites and coords (lat and long) for cbind
#unique removes the duplicates by rows so we end up with 1651 rows with coords just like all.sites
all.sites <- as.data.frame(values)
head(all.sites)
##all.sites <- na.omit(as.data.frame(dataframe))
nrow(all.sites)
nrow(coords.sub)
nrow(unique(coords.sub))
head(coords.sub)
head(all.sites)
unique_sites.dataframe <- cbind(unique(coords.sub),all.sites)
head(unique_sites.dataframe)
#now that the two dataframes are merged we have a dataframe with unique points with the coords and the bioclim data
#but we don't know which trees they are, we need the sample unique ID for each point...
all.dataframe<- na.omit(coords.all)
head(all.dataframe)
#same here we have the duplicates in the dataframe so we use the unique function to remove them
#then the two dataframes are merged by the long variable, there is no need for the same number of rows with merge function
nrow(unique(all.dataframe))
u.all.dataframe <- unique(all.dataframe)
merged.dataframe <- merge(unique_sites.dataframe, u.all.dataframe, by="Long") # by.x="Long, by.y="Long"
head(merged.dataframe)

#Laty and Lat x are the same so we remove lat y and name Lat x = Lat, put collector.s as the first column and naming it unique_ID
#data cleaned!
final.dataframe <- merged.dataframe[,c(22,1:21)]
head(final.dataframe)
colnames(final.dataframe)[1] <- "unique_ID"
colnames(final.dataframe)[3] <- "Lat"
write.table(final.dataframe, "output/CCA_worldclim_df.txt", quote = FALSE, row.names = FALSE)
write_csv(final.dataframe, "output/CCA_worldclim_df.csv")
#plotting the data using the final.dataframe, ploting by variables 
#cliamte extemes
plot.extreme <- with(final.dataframe, plot(BIO5, BIO14, cex=1, pch=16, col="grey46"))
#climate means
plot.mean <- with(final.dataframe, plot(BIO1, BIO12, cex=1, pch=16, col="grey46"))

print(plot.extreme)
dev.print(pdf, 'figs/plot_bioclim_extreme_CCA_trees_origin.pdf')
print(plot.mean)
dev.print(pdf, 'figs/plot_bioclim_mean_CCA_trees_origin.pdf')


#####################################################
