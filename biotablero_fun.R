## Web parsing for biotablero queries
## Requires 'curl' and 'jsonlite'

## --------------------------------------------------- ##
##    Parameters
## --------------------------------------------------- ##
##
## server = 'local' (default) or 'web'. Indicates the API 
##   location. If 'web' is selected, then must provide the *webURL argument
## port = ':8000' (default). System port opened for API connection. 
## webURL = NULL (default). The API web host. 
## dataPath = NULL (default). Folder containing data. Default value internally is '/data'
## printURL = FALSE (default). Should this function print the corresponding URL?
## endpoint = 'biotablero' (default). Endpoint to call. 'biotablero' is the main function. 
## metric = NULL  (default). Metric to calculate
## ...  Other parameters for customize the query



## ---------------------------------------------------
##  Usage
## ---------------------------------------------------
#
# biotablero(server = 'web', webURL = aws, port = ':8000',
#            endpoint = 'biotablero',
#            printURL = TRUE, metric = 'test')
#
#
# ForChange_aws <- biotablero(server = 'web', webURL = aws, port = ':8000', printURL = TRUE,
#                             endpoint = 'biotablero', metric = 'forest', 
#                             sour = 'hansen', ebvstat = 'area',
#                             ebvyear='2000:2018',
#                             pol = simplePol)




biotablero <- function(server = 'local', port = ':8000', webURL = NULL, dataPath = NULL, printURL = FALSE, 
                       endpoint = 'biotablero',
                       metric = NULL, lay = NULL, polID = NULL, pol = NULL, outformat = NULL,
                       ebvstat = NULL, sour = NULL, cellSize = NULL, ... ) {

  ## Assign the URL
  .webURL <-  ifelse(is.null(webURL), 'http://biotablero.humboldt.org.co/api', webURL)
  host <- ifelse(server == 'web', .webURL, 'http://localhost')
  
  ## Create a data.frame with params
  dfParams <- data.frame(val = c(metric = metric, lay = lay, polID = polID, outformat = outformat,
                                 ebvstat = ebvstat, sour = sour, dataPath = dataPath, 
                                 cellSize = cellSize, c(...), pol = pol))
  
  ## Build the URL
  urlParams <- paste0(apply(cbind(rownames(dfParams), dfParams), 1, function(x) paste0(x, collapse = '=')), collapse = '&')
  (myURL <- paste0(host, port, "/", endpoint, "?", urlParams))
  
  ## Print the URL
  if(printURL) {
    print(myURL)
  }
  
  ## Connect the URL
  con <- curl::curl(url = myURL)
  
  ## Read the URL
  query <- tryCatch(jsonlite::fromJSON(readLines(con, warn = FALSE)), error = function(e) e)
  close(con)
  
  ## Return R object values
  return(query)
}
