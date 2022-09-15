require('ecochange') #ecochange_1.6.tar.gz
packs <- c('raster','rgdal','parallel', 'R.utils', 'rvest','xml2','tidyverse', 'landscapemetrics', 'sf','dplyr','httr','getPass','gdalUtils','gdalUtilities','rgeos', 'viridis', 'rasterVis','rlang', 'rasterDT')
sapply(packs, require, character.only = TRUE)


#Set rout to folder where the armonized rasters are stored
dir. <- "/Users/sputnik/Library/CloudStorage/OneDrive-TempleUniversity/Dissertation/Maps_ARD/clusters_VICH/msk_alt_vich.tif"

#set list of if files to align

tiffes1 <- '/Users/sputnik/Library/CloudStorage/OneDrive-TempleUniversity/Dissertation/Maps_ARD/clusters_VICH/msk_alt_vich.tif'

#set path to the reference file
reference. <-'/Users/sputnik/Library/CloudStorage/OneDrive-TempleUniversity/Dissertation/Maps_ARD/msk_500.tif'

tiffes2  <-'/Users/sputnik/Library/CloudStorage/OneDrive-TempleUniversity/Dissertation/Maps_ARD/clusters_VICH/msk_alt_vich_reprj.tif'

system.time(
malr <- Map(function(x,y)
    align_rasters(
        unaligned=x,
    reference=reference.,
    dstfile=y,
    nThreads=8,
    verbose=TRUE),
    tiffes1,tiffes2)
)

setwd('/Users/sputnik/Documents/Biotablero/data')
msk <- raster('msk_f.tif')
m <- c(0,1,0)
m <- matrix(m, ncol=3,byrow = TRUE)
msk <- reclassify(msk,m)

ras1 <- raster('tree_arm_pcs.tif')
ras1 <- merge(ras1,msk)
ras1 <- ras1*100
writeRaster(ras1, 'tree_arm2_pcs.tif', overwrite=TRUE)

ras2 <- raster('loss_arm.tif')
m <- c(-Inf, -0.01,0)
m <- matrix(m, ncol=3,byrow=TRUE)
ras2 <- reclassify(ras2,m)
writeRaster(ras2, 'loss_arm2.tif')

writeRaster(msk, 'msk_reproj0.tif')



msk <- raster('/Users/sputnik/Library/CloudStorage/OneDrive-TempleUniversity/Dissertation/Maps_ARD/clusters_VICH/msk_alt_vich_reprj.tif')
writeRaster(msk, 'msk_vich.tif')

