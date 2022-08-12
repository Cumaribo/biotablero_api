## Get into the forest metrics
metric <- 'forest'
ebvstat <- 'area'
sour <- 'arm'
ebvyearnum <- c(2000,2021)
forestporcrng <- c(50,100)
forestyearrng <- c(2000,2021)

dep <- raster::getData('GADM', country = 'COL', level = 0)
prj <- "+proj=tmerc +lat_0=4.596200417 +lon_0=-74.07750791700001 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs"
prj_wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Draw a WKT polygon in: 
#  + https://clydedacruz.github.io/openstreetmap-wkt-playground/
#   
# Paste the URL as a R string variable
simplePol <- 'POLYGON((-74.0478515625 5.101887070062318,-74.278564453125 4.5764249358536375,-73.9984130859375 4.319023948318517,-73.839111328125 4.65855495640821,-73.90502929687499 4.921305737130183,-74.0478515625 5.101887070062318))'
simplePol <- gsub( ' ', '%20', simplePol) # Be sure blank spaces are converted in '%20'

polyg <- rgeos::readWKT( gsub('%20', ' ', simplePol))
proj4string(polyg) <- CRS(prj_wgs84)
# Check polygon location
plot(dep, axes = TRUE, xlim = c(-75, -70), ylim = c(-4, 12))
plot(polyg, add = TRUE, border = 1, col = 3)
thresholdKm2 <- 1000000

biotForYearString <- as.character(paste0(forestyearrng[1], ':', forestyearrng[2]))
biotForPorcString <- as.character(paste0(forestporcrng[1], ':', forestporcrng[2]))
  
ebvporcrange <- biotForPorcString
ebvporcrangenum <- as.numeric(strsplit(ebvporcrange, ':')[[1]])
ebvyear <- biotForPorcString 

test_for <- function(metric = NA, lay = NA, polID = NA, pol = NA, 
         ebvstat = NA, sour = NA, ebvyear = NA, ebvporcrange = NA,
         dataPath = '/data') {

  dots <- tryCatch(c(...), error = function(e) NULL)
  tStart <- Sys.time()
  print(paste0('====================================================='))
  print(paste0('Metric: ', metric, ' - Date: ', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE)  ))  
  if(metric %in% 'test'){
    result <- "Welcome to Biotablero API. Test function from 'biotablero' endpoint"
    return(result)
    stop()
  }  
  
  ## Create a gdal-readable object from WKT
  wkt <- suppressWarnings(tryCatch(SpatialPolygonsDataFrame(readWKT(gsub('%20', ' ', pol)), data.frame(ID = 1)),
                                   error = function(e) NULL))
  
  if (is.null(wkt)){
    return("ERROR: Not a valid WKT object")
    stop()
  }
  
  wkt@proj4string@projargs <- '+proj=longlat +ellps=GRS80 +no_defs'
  
  ## Project polygon in order to clip and get areas from projected original layers
  wkt_pcs <- suppressWarnings(spTransform(wkt, CRSobj = CRS(prj)))
  wkt_pcs$km2 <- suppressWarnings(sapply(slot(wkt_pcs, "polygons"), function(x) sum(sapply(slot(x, "Polygons"), slot, "area")))/1000000)
  
  ## Establish a treshold for the polygon. Colombia surface is 1,141,748 km2
  if (sum(wkt_pcs$km2) > thresholdKm2){
    return( paste0("ERROR: Polygon bigger than the threshold (", thresholdKm2, " km2)") )
    stop()
  }
  


if (metric %in% 'forest') { 
  
  if (is.null(ebvstat) | is.null(sour)){
    return(paste0("ERROR: forest metric requiere 'ebvstat' and 'sour' arguments"))
    stop()
  }
  
  ## Validate parameters
  if (! ebvstat %in% c('area', 'lsm_l_tafc', landscapemetrics::list_lsm(level = 'landscape')$function_name)){
    return(paste0('ERROR: EBVstat "', ebvstat, '" not "area" or "landscape-level metrics'))
    stop()
  }
  
  if (! sour %in% c('hansen', 'ideam' , 'arm')){
    return(paste0('ERROR: Source "', sour, '" not "ideam", "hansen" or "hansen_armonized" for forest source'))
    stop()
  }
  
  if ((is.na(ebvyear) & is.null(ebvyear))){
    return(paste0('ERROR: ebvyear must be provided'))
    stop()
  }
  
  if ((is.na(ebvporcrange) & is.null(ebvporcrange))){
    return(paste0('ERROR: ebvporcrangenum must be provided'))
    stop()
  }
  
  ebvyearnum <- as.numeric(strsplit(ebvyear, ':')[[1]])
  
  if (sour == 'hansen' | sour =='arm'){
    if( ! all(ebvyearnum %in% 2000:2021)){
      return(paste0('ERROR: ebvyear "', ebvyear, '" not in 2000:2021 for "hansen" or "arm" source'))
      stop()
    }
  } else if (sour == 'ideam'){
    if(! all(ebvyearnum %in% 1990:2016)){
      return(paste0('ERROR: ebvyear "', ebvyear, '" not in 1990:2016 for "ideam" source'))
      stop()
    }
  }
  
  ebvporcrangenum <- as.numeric(strsplit(ebvporcrange, ':')[[1]])
  
  if( !all( ebvporcrangenum >= 0 & ebvporcrangenum <= 100 ) ){
    return(paste0('ERROR: ebvporcrange "', ebvporcrange, '" not in 0:100'))
    stop()
  }
  
  print(' > Validating source and stats...')
  
  
  ## Assign temporal paths to rasters for forest extent and forest loss
  
  timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
  tempRastDir <- paste0(dataPath, '/tempR/', timeMark, 
                        '_r1', round(runif(1, 0, 100)), 
                        '_r2', round(runif(1, 0, 100)), 
                        '_r3', basename(tempfile()))
  dir.create(tempRastDir, recursive = TRUE)
  print(paste0('Temp dir: ', tempRastDir))
  
  rasterOptions(tmpdir = tempRastDir, format = 'GTiff')
  treeTemp <- paste0(tempRastDir, '/', basename(tempfile()), 'tree.tif')
  lossTemp <- paste0(tempRastDir, '/', basename(tempfile()), 'loss.tif')
  
  print(paste0('Calculating results from -', metric, '- metric'))
  
  ## Cut Colombia Hansen layers with WKT geometry
  
  suppressWarnings(gdalUtilities::gdalwarp(srcfile = paste0(dataPath, '/forest/tree_', sour ,'_pcs.tif'),
                                           dstfile = treeTemp,
                                           csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                                           cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                                           dstnodata = 999, crop_to_cutline = TRUE,
                                           overwrite = TRUE))
  
  print(' > Cutting tree cover ...')
  
  suppressWarnings(gdalUtilities::gdalwarp(srcfile = paste0(dataPath, '/forest/loss_', sour ,'_pcs.tif'), 
                                           dstfile = lossTemp, 
                                           csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                                           cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                                           overwrite = TRUE, dstnodata = 999, crop_to_cutline = TRUE))
  
  
  print(' > Cutting forest loss ...')
  
  # file1ad189416loss
  maskTemp <- gsub(x = lossTemp, '\\.tif', '\\_mask.tif')
  gdal_cmd <- paste0('gdal_calc.py -A ',treeTemp,' -B ',lossTemp,' --outfile=', maskTemp,' --calc="B*logical_and(A>=',ebvporcrangenum[1],
                     ',A<=',ebvporcrangenum[2],')" --NoDataValue=0 --quiet --overwrite') # --type Int16 
  suppressWarnings(system(gdal_cmd))
  print(' > Masking forest loss ...')
  
  ## Apply EBVmetric
  
  del10 <- ifelse(sour == 'ideam', 10, 0)
  
  ## Calculate metric
  if (ebvstat %in% c('area')){
    
    ## Only using pixel counts
    suppressWarnings(rastInfo <- capture.output(gdalinfo(datasetname = treeTemp)))
    pixelAreas <- prod(abs(as.numeric(gsub(" |[[:alpha:]]|([.-])|[[:punct:]]", "\\1",
                                           strsplit(grep('Pixel Size ', rastInfo, value = TRUE)
                                                    , ',')[[1]]))))/1000000
    
    countForestTime0 <- raster_count(treeTemp, n256 = TRUE)
    countForestTime0 <- sum(countForestTime0$count[
      which(countForestTime0$id >= ebvporcrangenum[1] 
            & countForestTime0$id<= ebvporcrangenum[2]) ]) * pixelAreas
    
    countYearLoss <- raster_count(maskTemp, n256 = TRUE)
    countYearLoss <- subset(countYearLoss, count != 0)
    countYearLoss$count <- countYearLoss$count * pixelAreas
    
    result <- data.frame(year = c(0, countYearLoss$id) + 2000, 
                         area = countForestTime0 - (c(0, cumsum(countYearLoss$count)) ))
    
  } else {
    
    ## Use ForestChange package
    stk <- stack(treeTemp, maskTemp) 
    # names(stk) <- c("treecover2000", "lossyear")
    # fcmask <- forestChange::FCMask(pol = stk, year = (ebvyearnum[1]:ebvyearnum[2]) + del10, 
    #                                perc = eval(parse(text = ebvporcrange)), pr.utm = FALSE)
    # fcmetric <- forestChange::EBVmetric(fcmask, what = ebvstat)
    # fcmetricSubset <- subset(fcmetric , layer %in% ( (ebvyearnum[1]:ebvyearnum[2]) + del10 - 2000) )
    # result <- data.frame(year = (ebvyearnum[1]:ebvyearnum[2]) + del10, metric = fcmetricSubset$value, row.names = fcmetricSubset$layer)
    # colnames(result)[2] <- ebvstat
    
    fcmask <- ecochange::echanges(pol = stk, 
                                  eco = c('treecover2000','lossyear'),
                                  eco_range = c(ebvporcrange,100), ###
                                  change_vals = (ebvyearnum[1]:ebvyearnum[2]) + del10)
    fcmetric <- ecochange::EBVstats(fcmask, stats = ebvstat)
    fcmetricSubset <- subset(fcmetric , layer %in% ( (ebvyearnum[1]:ebvyearnum[2]) + del10 - 2000) )
    result <- data.frame(year = (ebvyearnum[1]:ebvyearnum[2]) + del10, metric = fcmetricSubset$value, row.names = fcmetricSubset$layer)
    colnames(result)[2] <- ebvstat
  }
  rownames(result) <- result$year <- result$year - del10
  result <- subset(result, year %in% ebvyearnum[1]:ebvyearnum[2])
  
  file.remove(treeTemp, lossTemp)
  unlink(tempRastDir, recursive = TRUE, force = TRUE)
} 
  print(paste0('Exporting results from -', metric, '- metric'))
  timeElapsed <- Sys.time() - tStart
  return(
    list(result =  result,
         #list(result =  jsonlite::toJSON(as.list(result), auto_unbox = TRUE),
         params = data.frame(params = c(metric = metric, lay = lay, polID = polID,
                                        pol = pol,
                                        ebvstat = ebvstat, sour = sour,
                                        ebvyear = ebvyear,
                                        ebvporc = ebvporcrange, dots,
                                        time = paste(timeElapsed, attr(timeElapsed, 'units')) ) ) ))
}


test105 <- test_for(metric = 'forest', lay = NA, polID = NA, pol = simplePol, 
                     ebvstat = 'area', sour = 'arm', ebvyear = biotForYearString , ebvporcrange = biotForPorcString,
                     dataPath = '/Users/sputnik/Documents/Biotablero/data') 

rm(test105)

test305 <- test_for(metric = 'forest', lay = NA, polID = NA, pol = simplePol, 
                  ebvstat = 'area', sour = 'hansen', ebvyear = biotForYearString , ebvporcrange = biotForPorcString,
                  dataPath = '/Users/sputnik/Documents/Biotablero/data') 
  
test <- test_for(metric = 'test', lay = NA, polID = NA, pol = NA, 
                 ebvstat = NA, sour = NA, ebvyear = NA , ebvporcrange = NA,
                 dataPath = '/Users/sputnik/Documents/Biotablero/data') 

  
wkt <- suppressWarnings(tryCatch(SpatialPolygonsDataFrame(readWKT(gsub('%20', ' ', pol)), data.frame(ID = 1)),
                                 error = function(e) NULL))

  ## Get into the forest metrics ------

dataPath <- '/Users/sputnik/Documents/Biotablero/data'

wkt <- suppressWarnings(tryCatch(SpatialPolygonsDataFrame(readWKT(gsub('%20', ' ', pol)), data.frame(ID = 1)),
                                 error = function(e) NULL))

wkt <- suppressWarnings(tryCatch(SpatialPolygonsDataFrame(readWKT(gsub('%20', ' ', simplePol)), data.frame(ID = 1)),
                                 error = function(e) NULL))

tempRastDir <- paste0(dataPath, '/tempR/', timeMark, 
                      '_r1', round(runif(1, 0, 100)), 
                      '_r2', round(runif(1, 0, 100)), 
                      '_r3', basename(tempfile()))
dir.create(tempRastDir, recursive = TRUE)
print(paste0('Temp dir: ', tempRastDir))

rasterOptions(tmpdir = tempRastDir, format = 'GTiff')
treeTemp <- paste0(tempRastDir, '/', basename(tempfile()), 'tree.tif')
lossTemp <- paste0(tempRastDir, '/', basename(tempfile()), 'loss.tif')


wkt_pcs <- suppressWarnings(st_transform(wkt, crs = CRS(prj)))
wkt_pcs$km2 <- suppressWarnings(sapply(slot(wkt_pcs, "polygons"), function(x) sum(sapply(slot(x, "Polygons"), slot, "area")))/1000000)

suppressWarnings(gdalUtilities::gdalwarp(srcfile = paste0(dataPath, '/forest/tree_', sour ,'_pcs.tif'),
                                         dstfile = treeTemp,
                                         csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                                         cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                                         dstnodata = 999, crop_to_cutline = TRUE,
                                         overwrite = TRUE))
test1$result
test105$result
test305$result


fcmask <- ecochange::echanges(pol = stk, 
                              eco = c('treecover2000','lossyear'),
                              eco_range = c(ebvporcrange,100), ###
                              change_vals = (ebvyearnum[1]:ebvyearnum[2]) + del10)
fcmetric <- ecochange::EBVstats(fcmask, stats = ebvstat)
fcmetricSubset <- subset(fcmetric , layer %in% ( (ebvyearnum[1]:ebvyearnum[2]) + del10 - 2000) )
result <- data.frame(year = (ebvyearnum[1]:ebvyearnum[2]) + del10, metric = fcmetricSubset$value, row.names = fcmetricSubset$layer)
colnames(result)[2] <- ebvstat
}
