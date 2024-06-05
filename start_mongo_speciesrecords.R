
## Load Mongo library
library(mongolite)

## Connect MongoDB
db <<- mongo(db = "biotablero", collection = "species", url ="mongodb://biotablero:admin@biotablerodb/biotablero", verbose = FALSE)

(dbc <- db$count())
if (6345606  != dbc){
  
  db$drop()
  ## Load table
  load('/data/species/records/records_and_flags.RData') # 7009738 original data
  
  ## Add consecutive ID
  occ$sortID <- 1:nrow(occ)  
  
  ## Filter missing coordinates
  occ <- occ[which(!is.na(occ$decimalLongitude) & !is.na(occ$decimalLatitude)), ] # final rows
  gc()
  
  ## Insert table in Mongo
  system.time( db$insert(occ) )
  #   user  system elapsed
  #104.371   3.311 133.753
}



## Drop all the current Mongo information. Clean the database

## First lock for avoid deleting Mongo table
if (FALSE){
  
  ## Second lock for avoid deleting Mongo table
  if (FALSE){
    
    ## Third lock for avoid deleting Mongo table
    if(FALSE){
      # # # # # # # db$drop() ## Delete the table
    }
    
  }
  
}