FROM rocker/r-ver:latest

# Install the linux libraries needed
RUN apt-get update -qq && apt-get install -y \
  libssl-dev \
  libcurl4-gnutls-dev \
  libgdal-dev \
  libproj-dev  \
  gdal-bin

# Install mongo prerequisites
RUN apt-get install -y libssl-dev libsasl2-dev libudunits2-dev

COPY rgdal_1.6-7.tar.gz /tmp/
COPY rgeos_0.6-4.tar.gz /tmp/ 

# Install R packages
RUN R -e "install.packages('sp')"
RUN R -e "install.packages('/tmp/rgdal_1.6-7.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('raster')"
RUN R -e "install.packages('sf')"
RUN R -e "install.packages('/tmp/rgeos_0.6-4.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('foreign')"
RUN R -e "install.packages('landscapemetrics')"
RUN R -e "install.packages('mongolite')"
RUN R -e "install.packages('aws.s3')"
RUN R -e "install.packages('gdalUtils')"
RUN R -e "install.packages('gdalUtilities')"
RUN R -e "install.packages('rasterDT')"
RUN R -e "install.packages('rvest')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('ecochange')"
RUN R -e "install.packages('devtools')"
RUN R -e "devtools:::install_github('gearslaboratory/gdalUtils')"
RUN R -e "install.packages('ecochange')"


# Assign the temporal folder in the external drive
RUN R -e "write('TMP = "/data/tempR"', file=file.path(Sys.getenv('R_USER'), '.Renviron'))"

# Copy everything from the current directory into the container
COPY / /

# Install forestChange from .tar
#RUN R -e "install.packages('forestChange_1.2.tar.gz', repos = NULL, type = 'source')"
# RUN R -e "source('start_mongo_speciesrecords.R')" # Run externally once to populate mongoDB

# open port 8000 to traffic
EXPOSE 8000

# Call main script
ENTRYPOINT ["Rscript", "main.R"]

