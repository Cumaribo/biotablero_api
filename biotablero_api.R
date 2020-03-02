library(forestChange)
library(gdalUtils)
library(rgeos)
library(rgdal)
library(raster)
library(plumber)
library(foreign)

forestyy <- c(1990, 2000, 2005, 2010, 2012, 2013, 2014, 2015, 2016)
ideamyyebm <- c(0, 10, 15, 20, 22, 23, 24, 25, 26)
metrics <- c('biome', 'ecouicn', 'faccomp', 'bioticreg', 'tropdryforest', 'param',
             'hum', 'sma', 'protectareas', 'colectareas', 'clc', 'forest', 'rli', 'species',
             'threatspecies', 'evalspecies', 'knownspecies', 'surface', 'test')
prj <- "+proj=tmerc +lat_0=4.596200417 +lon_0=-74.07750791700001 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs"
thresholdKm2 <- 75000

#* @apiTitle BiotableroAPI

#* Testing the API
#* @param None None parameter
#* @get /test
function(){
  list(msg = "Hello. Glad to see you. Endpoint 'test'")
}

#* Provide the current working directory
#* @param None None parameter
#* @get /gwd
function(){
  list(gwd = getwd())
}

#* List files and folders in a given path
#* @param path The path requierd
#* @get /lsFiles
function(path){
  list(files = c(list.files(path = path, recursive = FALSE)),
       dirs = c(list.dirs(path = path, recursive = FALSE)))
}

#* Evaluate the polygon area in Km2
#* @param pol The WKT geometry
#* @get /polsizekm2
function(pol = NULL){
  if (!is.null(pol)){
    
    ## Create a gdal-readable object from WKT
    wkt <- tryCatch(SpatialPolygonsDataFrame(readWKT(gsub('%20', ' ', pol)), data.frame(ID = 1)),
                    error = function(e) NULL)
    
    if (is.null(wkt)){
      return("ERROR: Not a valid WKT object")
      stop()
    }
    
    return(area(wkt)/10000000)
  }
}

#* List the available stpatial templates
#* @param templatesPath The path containign the available layers
#* @get /listTemplates
function(templatesPath = '/data/templates'){
  gsub('.dbf', '', list.files(path = templatesPath, pattern = '.dbf'))
}

#* Get the template table & IDs
#* @param template The table requiered
#* @param dataTemplates The path containign the available layers
#* @get /getTemplate
function(template = NULL, templatesPath = '/data/templates'){
  templateTable <- tryCatch(foreign::read.dbf(paste0(templatesPath, '/', template, '.dbf')),
                            error = function(e) NULL)
  if (is.null(templateTable)){
    return("ERROR: Not a valid template. Use the 'listTemplates' function to know available options")
    stop()
  }
  
  return(templateTable)
}


#* Calculate biodiversity metrics
#* @param metric The metric to be consulted. Incluided options are: 'biome', 'ecouicn', 'faccomp', 'bioticreg', 'tropdryforest', 'param', 'hum', 'sma', 'protectareas', 'colectareas', 'clc', 'forest', 'rli', 'species', 'threatspecies', 'evalspecies', 'knownspecies', 'surface'
#* @param lay The layer or spatial templates to be consulted. Optional in all metrics
#* @param polID The ID of polygon or element from the given layer or spatial extent. Optional in all metrics
#* @param pol A polygon in format WKT. Requeried in almost all metrics.
#* @param ebvstat The Essential Biodiversity Variables so be calculated. 'area' or 'all' available 
#* @param sour The data source to calculate the metric. If metric is 'forest', 'hansen' and 'ideam' are allowed. If metric is 'species', the values 'biomod', 'uicn' and 'records' are allowed.
#* @param cellSize The cell size for 'surface' metric. 1, 2, 5, 10, 20, 50, 100, 200, 500 are allowed. Required in 'surface' metric.
#* @param spFormat The possible formats for 'species' metric when the source (sour) is Biomodelos (biomod)
#* @param ebvyear The years to estimate the forest metrics. Requierd in 'forest' metric
#* @param ebvporcrange The threshold range values used to consider the forest cover extent in each pixel as a forest. Requierd in 'forest' metric
#* @param clclevel The Corine Land Cover level. Required in 'clc' metric
#* @param dataPath The data folder path

#* @get /biotablero
function(metric = NULL, lay = NULL, polID = NULL, pol = NULL, 
         ebvstat = NULL, sour = NULL, cellSize = NULL, spFormat = NULL,
         # ebvyear = '2000:2018', ebvporcrange = '80:100', 
         ebvyear = NULL, ebvporcrange = NULL, 
         clclevel = NULL, dataPath = '/data') {
  
  dots <- tryCatch(c(...), error = function(e) NULL)
  tStart <- Sys.time()
  print(paste0('Metric: ', metric, '- Date: ', Sys.time()  ))
  
  ## Testing
  if(metric %in% 'test'){
    result <- "Welcome to Biotablero API. Test function from 'biotablero' endpoint"
    return(result)
    stop()
  }
  
  ## Validate metric
  # 'metric' into the available metrics?
  if(!all(metric %in% metrics)){
    return(paste0("ERROR: ", metric, " not a valid metric"))
    stop()
  }
  
  
  ## Get into the static metrics
  if (metric %in% 'rli') {
    rli <- read.csv(paste0(dataPath, '/rli/rli.csv'), stringsAsFactors = FALSE)
    spAssesByYear <- read.csv(paste0(dataPath, '/rli/groupAssessByYear.csv'), stringsAsFactors = FALSE)
    sppCountbyGroup <- read.csv(paste0(dataPath, '/rli/sppCountbyGroup.csv'), stringsAsFactors = FALSE)
    result <<- list(redListIndex = rli,  sppAssesByYear = spAssesByYear, sppCountbyGroup = sppCountbyGroup)
  } 
  
  
  
  ## Validate polygon if given.
  # Identify if a polygon is into the arguments. If 'pol' is not NULL, should
  # be a wkt polygon which will cut the layers
  
  if (!is.null(pol)  & (is.null(lay) & is.null(polID) )){
    
    ## Create a gdal-readable object from WKT
    wkt <- tryCatch(SpatialPolygonsDataFrame(readWKT(gsub('%20', ' ', pol)), data.frame(ID = 1)),
                    error = function(e) NULL)
    
    if (is.null(wkt)){
      return("ERROR: Not a valid WKT object")
      stop()
    }
    
    wkt@proj4string@projargs <- '+proj=longlat +ellps=GRS80 +no_defs'
    
    ## Project polygon in order to clip and get areas from projected original layers
    wkt_pcs <- spTransform(wkt, CRSobj = CRS(prj))
    wkt_pcs$km2 <- sapply(slot(wkt_pcs, "polygons"), function(x) sum(sapply(slot(x, "Polygons"), slot, "area")))/1000000

    ## Establish a treshold for the polygon. Colombia surface is 1,141,748 km2
    if (sum(wkt_pcs$km2) > thresholdKm2){
      return("ERROR: Not a valid WKT object")
      stop()
    }

    ## Get into the crop-like functions for single layers
    if (metric %in% c('biome', 'bioticreg' ,'colectareas', 'ecouicn', 'faccomp', 
                      'hum', 'param', 'sma', 'protectareas', 'tropdryforest')){
      
      print(paste0('Calculating results from -', metric, '- metric'))
      
      layerFields <- c('id', 'name')
      if(metric == 'faccomp'){
        layerFields <- c('id', 'name', 'factor')
      }

      
      ## Calculate areas
      # areakm2 <- ogrinfo(datasource_name = paste0(dataPath, '/singleLayers/', metric,'.shp'),
      #                    dialect = 'SQLite', 
      #                    sql = paste0("SELECT ", paste0(layerFields, collapse = ', '),", ST_Area(GEOMETRY)/1000000 as km2 FROM ",
      #                                 metric, " WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('",
      #                                 writeWKT(wkt_pcs, byid = F), "'))"))
       
      areakm2 <- ogrinfo(datasource_name = paste0(dataPath, '/singleLayers/', metric,'.shp'), dialect = 'SQLite', 
                         sql = paste0("SELECT ", paste0(layerFields, collapse = ', '),", ST_Area(intersection)/1000000 as km2 FROM (SELECT ", paste0(layerFields, collapse = ', '),", ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM ", metric, " WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F),"')))")
                         )
      
      ## Convert OGR query in table
      vals <- grep(paste0(' ',  paste0(layerFields, collapse = ' | ') ,' | km2 '), areakm2, value = TRUE)
      vals <- gsub('^ |^  |[[:alpha:]].+\\= ', '', iconv(vals, from = 'utf8'))
      result <- data.frame(matrix(vals, ncol = length(layerFields)+1, byrow = TRUE), stringsAsFactors = FALSE)
      colnames(result) <- c(layerFields, 'km2')
      colnames(result) <- gsub('name', metric, colnames(result))
      result$km2 <- as.numeric(as.character(result$km2))
      
      if(metric == 'faccomp'){
        result <- with(result, aggregate(km2, list(faccomp, factor), sum))
        colnames(result) <- c('faccomp', 'factor', 'km2')
      }
      
    }
    
    
    ## Get into the crop-like functions for temporal layers: CLC
    if (metric %in% c('clc')){
      if (is.null(clclevel)){
        return(paste0('ERROR: clclevel required (1, 2 or 3)'))
        stop()
      }
      
      if (! clclevel %in% c('1', '2', '3', '4' )){
        return(paste0('ERROR: clclevel "', clclevel, '" not 1, 2, 3 or 4'))
        stop()
      }
      
      print(paste0('Calculating results from -', metric, '- metric'))
      
      ## Calculate areas
      clc2002 <- ogrinfo(datasource_name = paste0(dataPath, '/clc/N', clclevel, '_2000_2002.shp'), dialect = 'SQLite', 
                         sql = paste0("SELECT x2002, ST_Area(intersection)/1000000 as km2 FROM (SELECT x2002, ST_Intersection(GEOMETRY, ST_GeomFromText('",
                                      writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM N", clclevel, "_2000_2002 WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F),"')))"))
      
      clc2009 <- ogrinfo(datasource_name = paste0(dataPath, '/clc/N', clclevel, '_2005_2009.shp'), dialect = 'SQLite', 
                         sql = paste0("SELECT x2009, ST_Area(intersection)/1000000 as km2 FROM (SELECT x2009, ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM N", clclevel, "_2005_2009 WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F),"')))"))
      
      clc2012 <- ogrinfo(datasource_name = paste0(dataPath, '/clc/N', clclevel, '_2010_2012.shp'), dialect = 'SQLite', 
                         sql = paste0("SELECT x2012, ST_Area(intersection)/1000000 as km2 FROM (SELECT x2012, ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM N", clclevel, "_2010_2012 WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F),"')))"))
      
      ## Convert in table
      vals02 <- grep(' km2 | x2', clc2002, value = TRUE)
      result02 <- as.data.frame(matrix(as.numeric(gsub('^ .+\\= ', '', vals02)), ncol = 2, byrow = TRUE))
      vals09 <- grep(' km2 | x2', clc2009, value = TRUE)
      result09 <- as.data.frame(matrix(as.numeric(gsub('^ .+\\= ', '', vals09)), ncol = 2, byrow = TRUE))
      vals12 <- grep(' km2 | x2', clc2012, value = TRUE)
      result12 <- as.data.frame(matrix(as.numeric(gsub('^ .+\\= ', '', vals12)), ncol = 2, byrow = TRUE))
      
      allCLC <- rbind(cbind(yy = 2002, na.omit(result02)), cbind(yy = 2009, na.omit(result09)), cbind(yy = 2012, na.omit(result12)))
      resultCLC <- as.data.frame.matrix(xtabs(V2 ~ V1 + yy, data = allCLC))
      resultCLC$cod <- rownames(resultCLC)
      
      load(paste0(dataPath, '/clc/clcLegN', clclevel, '.RData'))
      result <- merge(resultCLC, clcLeg[ , c('legend', 'cod')], by = 'cod', all.x = TRUE,
                      all.y = FALSE, sort = TRUE)

    }
    
    
    ## Get into the forest metrics
    if (metric %in% 'forest') {
      
      if (is.null(ebvstat) |is.null(sour)){
        return(paste0("ERROR: forest metric requiere 'ebvstat' and 'sour' arguments"))
        stop()
      }
      
      ## Validate parameters
      if (! ebvstat %in% c('all', 'area')){
        return(paste0('ERROR: EBVstat "', ebvstat, '" not "frac.dim.index" or "forest.ext" for forest metric'))
        stop()
      }
      
      if (! sour %in% c('hansen', 'ideam')){
        return(paste0('ERROR: Source "', sour, '" not "ideam" or "hansen" for forest source'))
        stop()
      }
      
      ebvyearnum <- as.numeric(strsplit(ebvyear, ':')[[1]])
      
      if (sour == 'hansen'){
        if( ! all(ebvyearnum %in% 2000:2018)){
          return(paste0('ERROR: ebvyear "', ebvyear, '" not in 2000:2018'))
          stop()
        }
      } else if (sour == 'ideam'){
        if(! all(ebvyearnum %in% 1990:2016)){
          return(paste0('ERROR: ebvyear "', ebvyear, '" not in 1990:2016'))
          stop()
        } else {
          ebvyearnum <- ebvyearnum + 10
          }
      }
      
      ebvporcrangenum <- as.numeric(strsplit(ebvporcrange, ':')[[1]])
      
      if( !all( ebvporcrangenum >= 0 & ebvporcrangenum <= 100 ) ){
          return(paste0('ERROR: ebvyear "', ebvyear, '" not in 0:100'))
          stop()
        }

      print(' > Validating source and stats...')
      
      ## Assign temporal paths to rasters for forest extent and forest loss
      
      timeMark <- gsub('[[:punct:]]| ', '', Sys.time())
      tempRastDir <- paste0(dataPath, '/tempR/', timeMark)
      dir.create(tempRastDir, recursive = TRUE)
      print(paste0('Temp dir: ', tempRastDir))
      
      rasterOptions(tmpdir = tempRastDir, format = 'GTiff')
      treeTemp <- paste0(tempRastDir, '/', basename(tempfile()), 'tree.tif')
      lossTemp <- paste0(tempRastDir, '/', basename(tempfile()), 'loss.tif')
      
      print(paste0('Calculating results from -', metric, '- metric'))
      
      ## Cut Colombia Hansen layers with WKT geometry
      
      tree <- gdalwarp(srcfile = paste0(dataPath, '/forest/', sour ,'/tree.tif'),
                       dstfile = treeTemp,
                       csql = paste0("select ST_GeomFromText('", writeWKT(wkt, byid = F), "')"),
                       cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                       dstnodata = 999,
                       crop_to_cutline = TRUE,
                       overwrite = TRUE,
                       output_Raster = TRUE)
      print(' > Cutting tree cover ...')
      
      loss <- gdalwarp(srcfile = paste0(dataPath, '/forest/', sour ,'/loss.tif'), 
                       dstfile = lossTemp, 
                       csql = paste0("select ST_GeomFromText('", writeWKT(wkt, byid = F), "')"),
                       cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                       overwrite = TRUE,
                       dstnodata = 999,
                       crop_to_cutline = TRUE,
                       output_Raster = TRUE)
      
      print(' > Cutting forest loss ...')
      
      ## Apply EBVmetric
      stk <- stack(tree, loss) 
      names(stk) <- c("treecover2000", "lossyear")
      
      
      if (! sour %in% c('hansen', 'ideam')){
        return(paste0('ERROR: Source "', sour, '" not "ideam" or "hansen" for forest source'))
        stop()
      }
      
      ## Calculate areas
      pixelAreas <- area(stk$treecover2000)
      countForestTime0 <- sum( (Which(stk$treecover2000 >= ebvporcrangenum[1] & stk$treecover2000 <= ebvporcrangenum[2]) * pixelAreas)[] ) 
      countYearLoss <- tapply(INDEX = stk$lossyear[], pixelAreas[], sum)
      forestArea <- data.frame(area = countForestTime0 - (cumsum(c(0, countYearLoss[-1])) ))
      
      
      if (ebvstat %in% c('area')){
        rownames(forestArea)[1] <- 0
        result <- subset(forestArea, (as.numeric(rownames(forestArea))+2000) %in% ebvyearnum[1]:ebvyearnum[2])
        
      } else if (ebvstat %in% c('all')){
        fcmask <- forestChange::FCMask(pol = stk, year = ebvyearnum[1]:ebvyearnum[2], perc = eval(parse(text = ebvporc)))
        fcmetric <- forestChange::EBVmetric(mas_fc, met = ebvstat)
        result <- data.frame()
        result$area <- forestArea$area
      }
      
      
      if (sour == 'ideam'){
        rownames(result) <- as.numeric(rownames(result)) + 2000 - 10
      }
      
      
      file.remove(treeTemp, lossTemp, pixelAreas)
      unlink(tempRastDir, recursive = TRUE, force = TRUE)
    } 
    
    
    ## Get into the species metrics
    if (metric %in% c('species', 'threatspecies', 'evalspecies', 'knownspecies')){
      
      # Validate parameters
      if (! sour %in% c('biomod', 'uicn', 'records') & metric %in% 'species'){
        return(paste0('ERROR: Source "', sour, '" not "biomod", "uicn" or "records" for species metric'))
        stop()
      }
      
      if(sour == 'biomod'){
        
        if (! spFormat %in% c('list', 'area') ){
          return(paste0('ERROR: spFormat "', spFormat, '" not "list" or "area" for species metric'))
          stop()
        }
        
        print(paste0('Calculating results from -', metric, '- metric'))
        
        ## Create temporal tif from ID Raster
        tempIDrast <- paste0(tempfile(), '.tif')
        
        ## Get the pixels ID in the region
        rastID <- gdalwarp(srcfile = paste0(dataPath, 'species/biomod/idRast.tif'),
                           dstfile = tempIDrast,
                           cutline = paste0(dataPath, '/singleLayers/tempSQlite.sqlite'),
                           csql = paste0("select ST_GeomFromText('", writeWKT(wkt, byid = F), "')"),
                           crop_to_cutline = TRUE,
                           overwrite = TRUE,
                           output_Raster = TRUE)
        
        cellID <- as.numeric(unique(na.omit(rastID[])))
        
        ## Load species lists from those pixels. Species are numbers here
        spNumbers <- sapply(cellID, function(x){
          ld <- tryCatch(load(paste0(dataPath, '/species/biomod/spByCell/', x,'.RData')),
                         error = function(e) NULL)
          if(!is.null(ld)){
            return(sp)
          }
        })
        
        spNumbers <- unlist(spNumbers)
        
        ## Load species list 
        load(paste0(dataPath, '/species/biomod/spNames.RData'))
        load(paste0(dataPath, '/species/biomod/taxIssuesBm.RData'))
        
        ## Get the species numbers 
        if (spFormat %in% c('list') ){
          spNumbers <- unique(spNumbers)
          spNames. <- spNames[spNumbers]
          result <- taxonomyBm[taxonomyBm$speciesBlank  %in% gsub('[[:blank:]]', '', spNames.), ]
        }
        
        if (spFormat %in% c('area') ){
          spNumbers <- data.frame(table(spNumbers), stringsAsFactors = FALSE)
          spNames. <- spNames[as.numeric(as.character(spNumbers$spNumbers))]
          result <- taxonomyBm[taxonomyBm$speciesBlank  %in% gsub('[[:blank:]]', '', spNames.), ]
          result$km2 <- NA
          pos <- match(result$speciesBlank, gsub('[[:blank:]]', '', spNames.))
          result$km2[which(!is.na(pos))] <- spNumbers$Freq[na.omit(pos)]
        }
        
        
      }
      
      if(sour == 'uicn'){
        print(paste0('Calculating results from -', metric, '- metric'))
        
        if (spFormat %in% c('list') ){
          uicnSpp <- ogrinfo(datasource_name = paste0(dataPath,'/species/uicn/sp_UICN.shp'),
                             dialect = 'sqlite',
                             sql = paste0("SELECT binomial FROM sp_UICN WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('",
                                          writeWKT(wkt_pcs, byid = F),"'))"))
          binomSpp <- grep(' binomial \\(String\\) ', uicnSpp, value = TRUE)
          result <- unique(gsub('^ | binomial | km2 |\\(.+\\= ', '', binomSpp))
        }
        
        if (spFormat %in% c('area') ){
          
          
          uicnSpp <- ogrinfo(datasource_name = paste0(dataPath,'/species/uicn/sp_UICN_pcs.shp'), dialect = 'sqlite',
                             sql = paste0("SELECT binomial, ST_AREA(intersection)/1000000 as km2 FROM (SELECT binomial, ST_Intersection(GEOMETRY, ST_GeomFromText('",
                                          writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM sp_UICN_pcs WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", writeWKT(wkt_pcs, byid = F),"')))"))
          
          # Consider GEOS error in topology
          chunks <- sapply(grep(' bin', uicnSpp2, value = FALSE), function(x){
            y <- uicnSpp2[x:(x+6)]
            y <- paste0( gsub('GEOS error.+|', '',  y[1:(which(y == '')-1)] ) , collapse = '') 
            z <- gsub('^ | binomial | km2 |\\(.+\\= | $|  $|   $', '', strsplit(y, ' km2')[[1]])
          })
          
          result <- data.frame(matrix(chunks, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
          colnames(result) <- c('species', 'km2')
          result$km2 <- as.numeric(as.character(result$km2))
          result <- result[!is.na(result$km2), ]
        }
      }
      
    }
    
    
    ## Get into the surface metric
    if (metric %in% 'surface') {
      
      # Validate parameters
      if (! cellSize %in% c(1, 2, 5, 10, 20, 50, 100, 200, 500) ){
        return(paste0('ERROR: CellSize "', cellSize, '" not in the available sizes 1, 2, 5, 10, 20, 50, 100, 200, 500'))
        stop()
      }
      
      print(paste0('Calculating results from -', metric, '- metric'))
      
      surf <- raster(paste0(dataPath, '/surface/RecCoun-', cellSize, '.tif'))
      msk <- mask(crop(surf, wkt), wkt)
      result <- mean(msk[] > 0, na.rm = TRUE) * 100
    }                 
    
  }
  
  
  ## Load pre-calculated metrics
  if (is.null(pol) & (!is.null(lay) & !is.null(polID) ) ){
    ## Load pre-calculated values. A 'metrics' variable is
    ## stored in each RData from forest output/forest/...RData
    
    load(paste0(dataPath, '/output/', metric, '/', lay, '/', polID,'.RData'))
  }
  
  
  print(paste0('Exporting results from -', metric, '- metric'))

  timeElapsed <- Sys.time() - tStart
  
  # Return
  return(
    list(result = result,
         params = data.frame(params = c(metric = metric, lay = lay, polID = polID,
                                        pol = pol, spFormat = spFormat,
                                        ebvstat = ebvstat, sour = sour,
                                        clclevel = clclevel, ebvyear = ebvyear,
                                        ebvporc = ebvporcrange, dots,
         time = paste(timeElapsed, attr(timeElapsed, 'units')) ) ) ))
}
