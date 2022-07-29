## 0. Librerias y rutas
library(rgdal)
library(rgeos)
#library(ecochange)

source('/Users/sputnik/Documents/biotablero_api/biotablero_fun.R') # >>>>>>>> Cambiar ruta

## 1. Cargar un polígono
# pol <- writeWKT(dep[grep('AMAZONAS', dep$NOM_DPTO), ])
dep <- raster::getData('GADM', country = 'COL', level = 0)

# Get a WKT polygon:  https://www.gbif.org/occurrence/search

simplePol <- 'POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))' # Macuira
simplePol <- 'POLYGON((-75.06%202.2,-74.14%202.2,-74.14%203.21,-75.06%203.21,-75.06%202.2))' # Mariquita
polyg <- rgeos::readWKT( gsub('%20', ' ', simplePol))

plot(dep, axes = TRUE, xlim = c(-75, -70), ylim = c(-4, 12))
plot(polyg, add = TRUE, border = 1, col = 2)
writeOGR(SpatialPolygonsDataFrame(polyg, data.frame(id = 1)), '/Users/sputnik/Documents/Biotablero', 'Macuira', driver = 'ESRI Shapefile')

aws <- 'ec2-3-137-83-192.us-east-2.compute.amazonaws.com'
#aws <- 'ec2-3-84-170-26.compute-1.amazonaws.com'
localDataPath <- '/Users/sputnik/Documents/Biotablero/data'

## Understanding the API

###################################################################
##  > Test function. Get a simple message 

test_local <- biotablero(server = 'local', port = ':8000', endpoint = 'biotablero', printURL = TRUE, dataPath = localDataPath, metric = 'test')

(test_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', endpoint = 'biotablero',
                       printURL = TRUE, metric = 'test'))

## > listing files
ls_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                       endpoint = 'lsFiles', path = localDataPath)
ls_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE, 
                     endpoint = 'lsFiles', path = '/data')

## > Test function form 'biotablero' endpoint. Get a simple message 
biotablero(server = 'local', port = ':8000', 
                         endpoint = 'biotablero', 
                         printURL = TRUE, metric = 'test')
biotablero(server = 'web', webURL = aws, port = ':8000',
                       endpoint = 'biotablero',
                       printURL = TRUE, metric = 'test')

## > Get the current working directory
wd_local <- biotablero(server = 'local', port = ':8000', endpoint = 'gwd',
                       printURL = TRUE)
wd_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', 
                     endpoint = 'gwd', printURL = TRUE)

###############################################
## Multiple layers
###############################################

## >> forest change
system.time(ForChange_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                          endpoint = 'biotablero', metric = 'forest', 
                                          sour = 'hansen', ebvstat = 'forest.ext',
                                          dataPath = localDataPath, pol = simplePol))
# http://localhost:8000/biotablero?metric=forest&ebvstat=forest.ext&sour=hansen&dataPath=E:/iavh_biotablero/data&ebvyear=0:17&ebvporc=80&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))

system.time(ForChange_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                        endpoint = 'biotablero', metric = 'forest', 
                                        sour = 'hansen', ebvstat = 'forest.ext',
                                        pol = simplePol))
#http://ec2-3-137-83-192.us-east-2.compute.amazonaws.com:8000/biotablero?metric=forest&ebvstat=forest.ext&sour=hansen&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))
#user  system elapsed 
#0.05    0.00  191.84 


system.time(ForChange_awsFrac <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                        endpoint = 'biotablero', metric = 'forest', 
                                        sour = 'hansen', ebvstat = 'frac.dim.index',
                                        pol = simplePol))
#http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=forest&ebvstat=forest.ext&sour=hansen&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))
#user  system elapsed 
#0.05    0.00  191.84 


plot(ForChange_aws$result)

system.time(ForCh2_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                       endpoint = 'biotablero', metric = 'forest', 
                                       sour = 'ideam', ebvstat = 'forest.ext',
                                       ebvyear = '0:17', ebvporc = 80,
                                       dataPath = localDataPath, pol = simplePol))
# http://localhost:8000/biotablero?metric=forest&ebvstat=forest.ext&sour=ideam&dataPath=E:/iavh_biotablero/data&ebvyear=0:17&ebvporc=80&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))

system.time(ForCh2_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'forest', 
                                     sour = 'ideam', ebvstat = 'forest.ext',
                                     pol = simplePol))
# http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=forest&ebvstat=forest.ext&sour=ideam&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))


## >> Corine Land Cover

system.time(clc1_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'clc', 
                                     clclevel = 1, 
                                     dataPath = localDataPath, pol = simplePol))
#http://localhost:8000/biotablero?metric=clc&dataPath=E:/iavh_biotablero/data&clclevel=1&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))
system.time(clc1_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                   endpoint = 'biotablero', metric = 'clc', 
                                   clclevel = 1, 
                                   pol = simplePol))
#http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=clc&clclevel=1&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))
#user  system elapsed 
#0.01    0.00  115.67 

system.time(clc2_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'clc', 
                                     clclevel = 2, 
                                     dataPath = localDataPath, pol = simplePol))
#http://localhost:8000/biotablero?metric=clc&dataPath=E:/iavh_biotablero/data&clclevel=2&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))


system.time(clc2_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                   endpoint = 'biotablero', metric = 'clc', 
                                   clclevel = 2, 
                                   pol = simplePol))
#user  system elapsed 
#0.00    0.00  120.45

system.time(clc3_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'clc', 
                                     clclevel = 3, 
                                     dataPath = localDataPath, pol = simplePol))
#http://localhost:8000/biotablero?metric=clc&dataPath=E:/iavh_biotablero/data&clclevel=2&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))

system.time(clc3_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                   endpoint = 'biotablero', metric = 'clc', 
                                   clclevel = 3, 
                                   pol = simplePol))
#user  system elapsed 
#0.00    0.00  130.09 

###############################################
## Single layers
###############################################

## >> Tropical dry forest

system.time(TropFor_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                        endpoint = 'biotablero', metric = 'tropdryforest', 
                                        dataPath = localDataPath, pol = simplePol))

system.time(TropFor_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                        endpoint = 'biotablero', metric = 'tropdryforest', 
                                        pol = simplePol))
# http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=tropdryforest&pol=POLYGON((-75.06409%202.20276,-74.14124%202.20276,-74.14124%203.2135,-75.06409%203.2135,-75.06409%202.20276))
# 6.61

## >> Paramos

system.time(params_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                       endpoint = 'biotablero', metric = 'param', 
                                       dataPath = localDataPath, pol = simplePol))
# http://localhost:8000/biotablero?metric=param&dataPath=E:/iavh_biotablero/data&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))

system.time(params_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'param', 
                                     pol = simplePol))
# http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=param&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))
# 1.21

## >> wetlands

system.time(wet_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                       endpoint = 'biotablero', metric = 'hum', 
                                       dataPath = localDataPath, pol = simplePol))
# http://localhost:8000/biotablero?metric=param&dataPath=E:/iavh_biotablero/data&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))

system.time(wet_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'hum', 
                                     pol = simplePol))
# http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=param&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))



## >> Biome

system.time(biome_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                      endpoint = 'biotablero', metric = 'biome', 
                                      dataPath = localDataPath, pol = simplePol))
# http://localhost:8000/biotablero?metric=biome&dataPath=E:/iavh_biotablero/data&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))

system.time(biome_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                    endpoint = 'biotablero', metric = 'biome', 
                                    pol = simplePol))
# http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=biome&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))



## >> Eco UICN

system.time(ecoiucn_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                      endpoint = 'biotablero', metric = 'ecouicn', 
                                      dataPath = localDataPath, pol = simplePol))
# http://localhost:8000/biotablero?metric=biome&dataPath=E:/iavh_biotablero/data&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))

system.time(ecoiucn_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                    endpoint = 'biotablero', metric = 'ecouicn', 
                                    pol = simplePol))
# http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=biome&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))



## >> Biotic region

system.time(biot_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'bioticreg', 
                                     dataPath = localDataPath, pol = simplePol))
# http://localhost:8000/biotablero?metric=bioticreg&dataPath=E:/iavh_biotablero/data&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))

system.time(biot_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                   endpoint = 'biotablero', metric = 'bioticreg', 
                                   pol = simplePol))
# http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=bioticreg&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))



## >> Compensation factor

system.time(comp_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'faccomp', 
                                     dataPath = localDataPath, pol = simplePol))
# http://localhost:8000/biotablero?metric=bioticreg&dataPath=E:/iavh_biotablero/data&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))

system.time(comp_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                   endpoint = 'biotablero', metric = 'faccomp', 
                                   pol = simplePol))
# http://ec2-3-92-160-103.compute-1.amazonaws.com:8000/biotablero?metric=bioticreg&pol=POLYGON((-72.07%2012.04,-71.44%2011.800,-71.25%2012.05,-71.24%2012.16,-71.59%2012.33,-72.07%2012.04))


###############################################
## Administrative
###############################################

## >> Special managment areas

system.time(sma_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                    endpoint = 'biotablero', metric = 'faccomp', 
                                    dataPath = localDataPath, pol = simplePol))

system.time(sma_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                  endpoint = 'biotablero', metric = 'sma', 
                                  pol = simplePol))

## >> Protected areas

system.time(ap_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                   endpoint = 'biotablero', metric = 'faccomp', 
                                   dataPath = localDataPath, pol = simplePol))

system.time(ap_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                 endpoint = 'biotablero', metric = 'protectareas', 
                                 pol = simplePol))

## >> Collective areas

system.time(collec_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                   endpoint = 'biotablero', metric = 'colectareas', 
                                   dataPath = localDataPath, pol = simplePol))

system.time(collec_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                 endpoint = 'biotablero', metric = 'colectareas', 
                                 pol = simplePol))


###############################################
## Species
###############################################

## >> Biomodelos

system.time(spBiom_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                       endpoint = 'biotablero', metric = 'species', 
                                       sour = 'biomod',
                                       dataPath = localDataPath, pol = simplePol))


system.time(spBiom_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'species', 
                                     sour = 'biomod', 
                                     pol = simplePol))

dim(spBiom_aws$result)
barplot(table(spBiom_aws$result$Biomodelos))

system.time(spUicn_local <- biotablero(server = 'local', port = ':8000', printURL = TRUE,
                                       endpoint = 'biotablero', metric = 'species', 
                                       sour = 'uicn',
                                       dataPath = localDataPath, pol = simplePol))

system.time(spUicn_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                     endpoint = 'biotablero', metric = 'species', 
                                     sour = 'uicn', 
                                     pol = simplePol))


###############################################
## Surface
###############################################

system.time(surface1_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                      endpoint = 'biotablero', metric = 'surface', 
                                      cellSize = 1, 
                                      pol = simplePol))

system.time(surface10_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                      endpoint = 'biotablero', metric = 'surface', 
                                      cellSize = 10, 
                                      pol = simplePol))

system.time(surface100_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                                         endpoint = 'biotablero', metric = 'surface', 
                                         cellSize = 100, 
                                         pol = simplePol))


rli <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                  endpoint = 'biotablero', metric = 'rli')
rli <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
                  endpoint = 'biotablero', metric = 'rli')




