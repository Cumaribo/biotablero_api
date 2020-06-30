### Load required libraries
library(forestChange)
library(gdalUtils)
library(rgeos)
library(rgdal)
library(raster)
library(plumber)
library(foreign)
library(mongolite)


## Metrics allowed inside the API
metrics <- c('biome', 'ecouicn', 'faccomp', 'bioticreg', 'tropdryforest', 'param',
             'hum', 'sma', 'protectareas', 'colectareas', 'clc', 'forest', 'rli', 'species',
             'threatspecies', 'evalspecies', 'knownspecies', 'surface', 'test')

## Default geographic coordinate reference system
prj <- "+proj=tmerc +lat_0=4.596200417 +lon_0=-74.07750791700001 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs"

## Maximum km2 allowed for queries
thresholdKm2 <- 75000

## MongoDB fields
mongoFields <- c("kingdom", "phylum", "class", "order", "family", "genus", "species", 
                 "decimalLatitude", "decimalLongitude", "sortID",
                 "uicn_glob", "cites", "invasive", "endemic")





#* @apiTitle BiotableroAPI

#* Testing the API
#* @param None None parameter
#* @get /test
function(){
  list(msg = "Hello. Glad to see you. Endpoint 'test'")
}

#* Print argument 
#* @param x any argument
#* @get /class
function(x){
  print(x)
  print(class(x))
  list(x, class(x))
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
    
    return(list(polsizekm2 = area(wkt)/1000000))
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
#* @param ebvyear The years to estimate the forest metrics. Requierd in 'forest' metric.
#* @param ebvporcrange The threshold range values used to consider the forest cover extent in each pixel as a forest. Requierd in 'forest' metric.
#* @param clclevel The Corine Land Cover level. Required in 'clc' metric.
#* @param dataPath The data folder path.
#* @param spFormat The possible formats for 'species' metric. 
#* @param spRecordsFields Species records fields to extract from MongoDB.
#* @param spRecordsTabulate Tablulating field for species records.
#* @param dataPath The data folder path.
#* @serializer unboxedJSON
#* @get /biotablero

function(metric = NULL, lay = NULL, polID = NULL, pol = NULL, 
         ebvstat = NULL, sour = NULL, ebvyear = NULL, ebvporcrange = NULL,
         spFormat = NULL, spRecordsFields = NULL, spRecordsTabulate = NULL, 
         clclevel = NULL, cellSize = NULL, dataPath = '/data') {
  
  dots <- tryCatch(c(...), error = function(e) NULL)
  tStart <- Sys.time()
  print(paste0('Metric: ', metric, ' - Date: ', Sys.time()  ))
  
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
    load(file = paste0(dataPath, '/rli/rli.RData'))
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
      
      
      ## Default columns into each shapefile
      layerFields <- c('id', 'name')
      
      
      ## Customize fields for some layers.
      if(metric == 'faccomp'){
        layerFields <- c('id', 'name', 'factor')
      } else if(metric == 'ecouicn'){
        layerFields <- c('id', 'name', 'Bioma', 'Fisionomia', 'Paisaje','Biota')
      } else if(metric == 'protectareas'){
        layerFields <- c('id', 'protected', 'id_protect', 'category_p', 'date_resol')
      } else if(metric == 'sma'){
        layerFields <- c('id', 'bosque', 'rbiosfera', 'ramsar', 'aicas', 'comunidade', 'pdet', 'resguardos', 'paramos', 'portafolio', 'zonas')
      }
      
      
      ## Calculate areas
      
      # This approach calculates areas for all polygons
      if(metric %in% c('hum', 'ecouicn')){
        areakm2 <- ogrinfo(datasource_name = paste0(dataPath, '/singleLayers/', metric,'.shp'), dialect = 'SQLite', 
                           sql = paste0("SELECT ", paste0(layerFields, collapse = ', '),", ST_Area(intersection)/1000000 as km2 FROM (SELECT ", paste0(layerFields, collapse = ', '),
                                        ", ST_Intersection(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM ", metric, ")") )
      } else {
      
      # This approach first filter intersecting polygons, and then calculates areas
       areakm2 <- ogrinfo(datasource_name = paste0(dataPath, '/singleLayers/', metric,'.shp'), dialect = 'SQLite', 
                           sql = paste0("SELECT ", paste0(layerFields, collapse = ', '),", ST_Area(intersection)/1000000 as km2 FROM (SELECT ", 
                                        paste0(layerFields, collapse = ', '), ", ST_Intersection(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM ", metric, " WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                        writeWKT(wkt_pcs, byid = F),"')))") )
      }
      
      ## Create a string buffer 
      areakm2 <- c(areakm2, rep('OGR', 10))

      ## Consider GEOS error in topology. Catch GDAL warnings inside the query
      chunks <- sapply(grep('OGRFeature', areakm2, value = FALSE), function(x){
        y <- areakm2[x:(x + length(layerFields)+10)]
        k <- y[ 1: grep('OGR', y)[2]-1 ]
        w <- gsub('^ |^  ', '', paste0( gsub('GEOS error.+|', '', k) , collapse = ''))
        z <- gsub('.+\\= ', '', strsplit(w, paste0(c(layerFields, 'km2'), collapse = ' |'))[[1]])
        gsub('^ |^  | $|  $', '', z[z != ''][-1])
      })
      
      
      ## Process table only if some results are intersected
      if (length(chunks) != 0){
        result <- data.frame(matrix(chunks, ncol = length(layerFields)+1, byrow = TRUE), stringsAsFactors = FALSE)
        colnames(result) <- c(layerFields, 'km2')
        colnames(result) <- gsub('name', metric, colnames(result))
        result$km2 <- as.numeric(as.character(result$km2))
        result$id <- as.numeric(as.character(result$id))
        result <- result[!is.na(result$km2), ]
        result[result == '(null)'] <- NA
        
        
        ## Aggregate areas or post processing
        
        if(metric == 'faccomp'){
          
          result <- with(result, aggregate(km2, list(faccomp, factor), sum))
          colnames(result) <- c('faccomp', 'factor', 'km2')
          result$faccomp <- iconv(result$faccomp, from ='utf8')
          result$factor <- as.numeric(result$factor)
          
        } else if(metric == 'ecouicn'){
          
          result$Bioma <- iconv(result$Bioma, from ='utf8')
          result$Paisaje <- iconv(result$Paisaje, from ='utf8')
          
        } else if(metric == 'bioticreg'){
          
          result$bioticreg <- iconv(result$bioticreg, from ='utf8')
          
        } else if(metric == 'tropdryforest'){
          
          result <- with(result, aggregate(km2, list(id, tropdryforest), function(x) sum(x, na.rm = TRUE)))
          colnames(result) <- c(layerFields, 'km2')
          
        } else if(metric == 'sma'){
          
          smaFields <- subset(layerFields, layerFields != 'id')
          
          categs <- apply(result[, smaFields], 1, function(x){
            paste(smaFields[which(!is.na(x))], collapse = ', ')
          })
          
          result1 <- data.frame(xtabs(result$km2 ~ categs))
          colnames(result1) <- c('categs', 'km2')
          
          entities <- apply(result[, c(smaFields, 'km2')], 1, function(x){
            km2 <- as.numeric(x[length(x)])
            y <- x[-length(x)]
            cbind(cbind(smaFields[which(!is.na(y))], y[which(!is.na(y))]), km2)
          })
          
          if (class(entities) == 'list'){
            result2 <- data.frame(do.call(rbind, entities), stringsAsFactors = FALSE)
          } else if (class(entities) == 'matrix'){
            result2 <- data.frame(entities, stringsAsFactors = FALSE)
            if (nrow(entities) == 3){
              result2 <- data.frame(t(entities), stringsAsFactors = FALSE)
            } 
          }
          
          colnames(result2) <- c('category', 'management', 'km2')
          result2$km2 <- as.numeric(result2$km2)
          result2 <- with(result2, aggregate(km2, list(category, management), function(x) sum(x, na.rm = TRUE)))
          colnames(result2) <- c('category', 'management', 'km2')
          
          result <- list(result1 = result1, result2 = result2)

        }
        
      } else {
        result <- 0
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
                         sql = paste0("SELECT x2009, ST_Area(intersection)/1000000 as km2 FROM (SELECT x2009, ST_Intersection(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM N", clclevel,
                                      "_2005_2009 WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F),"')))"))
      
      clc2012 <- ogrinfo(datasource_name = paste0(dataPath, '/clc/N', clclevel, '_2010_2012.shp'), dialect = 'SQLite', 
                         sql = paste0("SELECT x2012, ST_Area(intersection)/1000000 as km2 FROM (SELECT x2012, ST_Intersection(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM N", clclevel, "_2010_2012 WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                      writeWKT(wkt_pcs, byid = F),"')))"))
      
      clc2002 <- c(clc2002, rep('OGR', 3))
      clc2009 <- c(clc2009, rep('OGR', 3))
      clc2012 <- c(clc2012, rep('OGR', 3))
      
      ## Consider GEOS error in topology. Catch GDAL warnings inside the query
      vals02 <- sapply(grep('OGRFeature', clc2002, value = FALSE), function(x){
        y <- clc2002[(x+1):(x + 3)]
        w <- gsub('^ |^  ', '', paste0( gsub('GEOS error.+|', '',  y[1:(which(y == '')[1]-1)] ) , collapse = ''))
        z <- gsub('.+\\= ', '', strsplit(w, 'x2002 |km2')[[1]])
        gsub('^ | $', '', z[z != ''])
      })
      
      vals09 <- sapply(grep('OGRFeature', clc2009, value = FALSE), function(x){
        y <- clc2009[(x+1):(x + 3)]
        w <- gsub('^ |^  ', '', paste0( gsub('GEOS error.+|', '',  y[1:(which(y == '')[1]-1)] ) , collapse = ''))
        z <- gsub('.+\\= ', '', strsplit(w, 'x2009 |km2')[[1]])
        gsub('^ | $', '', z[z != ''])
      })
      
      vals12 <- sapply(grep('OGRFeature', clc2012, value = FALSE), function(x){
        y <- clc2012[(x+1):(x+3)]
        w <- gsub('^ |^  ', '', paste0( gsub('GEOS error.+|', '',  y[1:(which(y == '')[1]-1)] ) , collapse = ''))
        z <- gsub('.+\\= ', '', strsplit(w, 'x2012 |km2')[[1]])
        gsub('^ | $', '', z[z != ''])
      })
      
      ## Convert in table
      result02 <- as.data.frame(matrix(as.numeric(gsub('^ .+\\= ', '', vals02)), ncol = 2, byrow = TRUE))
      result09 <- as.data.frame(matrix(as.numeric(gsub('^ .+\\= ', '', vals09)), ncol = 2, byrow = TRUE))
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
      
      if (is.null(ebvstat) | is.null(sour)){
        return(paste0("ERROR: forest metric requiere 'ebvstat' and 'sour' arguments"))
        stop()
      }
      
      ## Validate parameters
      if (! ebvstat %in% c('area', 'lsm_l_tafc', landscapemetrics::list_lsm(level = 'landscape')$function_name)){
        return(paste0('ERROR: EBVstat "', ebvstat, '" not "area" or "landscape-level metrics'))
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
        }
      }
      
      ebvporcrangenum <- as.numeric(strsplit(ebvporcrange, ':')[[1]])
      
      if( !all( ebvporcrangenum >= 0 & ebvporcrangenum <= 100 ) ){
        return(paste0('ERROR: ebvporcrange "', ebvporcrange, '" not in 0:100'))
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
      
      tree <- gdalwarp(srcfile = paste0(dataPath, '/forest/tree_', sour ,'_pcs.tif'),
                       dstfile = treeTemp,
                       csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                       cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                       dstnodata = 999,
                       crop_to_cutline = TRUE,
                       overwrite = TRUE,
                       output_Raster = TRUE)
      print(' > Cutting tree cover ...')
      
      loss <- gdalwarp(srcfile = paste0(dataPath, '/forest/loss_', sour ,'_pcs.tif'), 
                       dstfile = lossTemp, 
                       csql = paste0("select ST_GeomFromText('", writeWKT(wkt_pcs, byid = F), "')"),
                       cutline = paste0(dataPath,'/singleLayers/tempSQlite.sqlite'),
                       overwrite = TRUE,
                       dstnodata = 999,
                       crop_to_cutline = TRUE,
                       output_Raster = TRUE)
      
      print(' > Cutting forest loss ...')
      
      ## Apply EBVmetric
      stk <- stack(tree, loss) 
      names(stk) <- c("treecover2000", "lossyear")
      
      del10 <- ifelse(sour == 'ideam', 10, 0)
      
      ## Calculate metric
      if (ebvstat %in% c('area')){
        
        ## Only using pixel counts
        pixelAreas <- prod(res(stk$treecover2000))/1000000
        countForestTime0 <- sum( (Which(stk$treecover2000 >= ebvporcrangenum[1] & stk$treecover2000 <= ebvporcrangenum[2]) * pixelAreas)[] ) 
        countYearLoss <- table(stk$lossyear[]) * pixelAreas
        result <- data.frame(year = as.numeric(names(countYearLoss)) + 2000, 
                             area = countForestTime0 - (cumsum(c(0, countYearLoss[-1])) ))
        
      } else {
        
        ## Use ForestChange package
        fcmask <- forestChange::FCMask(pol = stk, year = (ebvyearnum[1]:ebvyearnum[2]) + del10, 
                                       perc = eval(parse(text = ebvporcrange)), pr.utm = FALSE)
        fcmetric <- forestChange::EBVmetric(fcmask, what = ebvstat)
        fcmetricSubset <- subset(fcmetric , layer %in% ( (ebvyearnum[1]:ebvyearnum[2]) + del10 - 2000) )
        result <- data.frame(year = (ebvyearnum[1]:ebvyearnum[2]) + del10, metric = fcmetricSubset$value, row.names = fcmetricSubset$layer)
        colnames(result)[2] <- ebvstat
        
      }
      
      rownames(result) <- result$year <- result$year - del10
      result <- subset(result, year %in% ebvyearnum[1]:ebvyearnum[2])
      
      file.remove(treeTemp, lossTemp)
      unlink(tempRastDir, recursive = TRUE, force = TRUE)
    } 
    
    
    ## Get into the species metrics
    if (metric %in% c('species', 'threatspecies', 'evalspecies', 'knownspecies')){
      
      # Validate parameters
      if (! sour %in% c('biomod', 'uicn', 'records') & metric %in% 'species'){
        return(paste0('ERROR: Source "', sour, '" not "biomod", "uicn" or "records" for species metric'))
        stop()
      }
      
      if(sour == 'records'){
        
        if (! spFormat %in% c('list', 'count') ){
          return(paste0('ERROR: spFormat "', spFormat, '" not "list" or "count" for species metric'))
          stop()
        }
        
        ## Generate MongoDB connection and query based on spatial extent (square)
        db <<- mongo(db = "biotablero", collection = "species", url ="mongodb://biotablero:admin@biotablerodb/biotablero", verbose = FALSE)
        wkt_ext <- raster::extent(wkt)
        mongoQuery <- paste0('{"decimalLongitude": {"$gte": ', wkt_ext@xmin, ', "$lte": ', wkt_ext@xmax, '}, "decimalLatitude": {"$gte": ', wkt_ext@ymin, ', "$lte": ', wkt_ext@ymax, '}}')
        mongoResult <- db$find(mongoQuery)
        
        ## Select records inside the geometry
        spatialQuery <- raster::extract(wkt, mongoResult[, c("decimalLongitude", "decimalLatitude")])
        mongoResult <- mongoResult[!is.na(spatialQuery$poly.ID), ]
        
        
        ## Return the records
        if (spFormat %in% c('list') ){
          result <- mongoResult
        }
        
        if (spFormat %in% c('count') ){
          if (! spRecordsTabulate %in% c('class', 'uicn_glob', 'endemic', 'invasive', 'cites') ){
            return(paste0('ERROR: spRecordsTabulate "', spRecordsTabulate, '" not "class", "uicn_glob", "endemic", "invasive" or "cites" for species metric'))
            stop()
          }
          
          result <- as.data.frame.matrix(table(unique(mongoResult[, c('class', spRecordsTabulate)]), useNA = 'ifany'))
        }
        
        ## Closing connection not required
        ## rm(db); gc() 
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
        rastID <- gdalwarp(srcfile = paste0(dataPath, '/species/biomod/idRast.tif'),
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
        
        ## Check if is present in the area
        if (spFormat %in% c('list') ){
          uicnSpp <- ogrinfo(datasource_name = paste0(dataPath,'/species/uicn/sp_UICN.shp'),
                             dialect = 'sqlite',
                             sql = paste0("SELECT binomial FROM sp_UICN WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('",
                                          writeWKT(wkt, byid = F),"'))"))
          binomSpp <- grep(' binomial \\(String\\) ', uicnSpp, value = TRUE)
          result <- unique(gsub('^ | binomial | km2 |\\(.+\\= ', '', binomSpp))
        }
        
        ## Calculate area
        if (spFormat %in% c('area') ){
          
          uicnSpp <- ogrinfo(datasource_name = paste0(dataPath,'/species/uicn/sp_UICN_pcs.shp'), dialect = 'sqlite',
                             sql = paste0("SELECT binomial, ST_AREA(intersection)/1000000 as km2 FROM (SELECT binomial, ST_Intersection(GEOMETRY, ST_GeomFromText('",
                                          writeWKT(wkt_pcs, byid = F), "')) AS intersection FROM sp_UICN_pcs WHERE ST_Intersects(GEOMETRY, ST_GeomFromText('", 
                                          writeWKT(wkt_pcs, byid = F),"')))"))
          
          # Consider GEOS error in topology
          chunks <- sapply(grep('OGRFeature', uicnSpp, value = FALSE), function(x){
            y <- uicnSpp[x:(x+8)]
            y <- paste0( gsub('GEOS error.+|', '',  y[1:(which(y == '')[1]-1)] ) , collapse = '') 
            z <- gsub('^ |OGRFeature| binomial | km2 |\\(.+\\= | $|  $|   $', '', strsplit(y, ' km2')[[1]])
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
      result <- list(percentage = mean(Which(msk > 0)[], na.rm = TRUE) * 100)
    }                 
    
  }
  
  ## Load pre-calculated values. A 'metrics' variable is
  ## stored in each RData from forest output/forest/...RData
  if (is.null(pol) & (!is.null(lay) & !is.null(polID) ) ){
    precalculateResults <- tryCatch(load(paste0(dataPath, '/output/', metric, '/', lay, '/', polID,'.RData')),
                                    error = function(e) NULL)
    if (is.null(precalculateResults)){
      return("ERROR: Not previous results for this query")
      stop()
    }
  }
  
  
  print(paste0('Exporting results from -', metric, '- metric'))
  timeElapsed <- Sys.time() - tStart
  
  # Return result and parameters 
  
  return(
    list(result =  result,
    #list(result =  jsonlite::toJSON(as.list(result), auto_unbox = TRUE),
         params = data.frame(params = c(metric = metric, lay = lay, polID = polID,
                                        pol = pol, spFormat = spFormat,
                                        ebvstat = ebvstat, sour = sour,
                                        clclevel = clclevel, ebvyear = ebvyear,
                                        ebvporc = ebvporcrange, dots,
                                        time = paste(timeElapsed, attr(timeElapsed, 'units')) ) ) ))
}

