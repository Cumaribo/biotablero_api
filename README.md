# Biotablero API

API for Biotablero at Instituto Alexander von Humboldt/Rsensus Lab @ Temple University 
# Purpose
Set up and maintain the biotablero backend infrastructure. 

1. Data update and expansion.
2. New capabilities/functions 

# Important

The detailed instructions about how to set the instance on Amazon, load the data ,set up the Backend and start it are in the files documentation/doc_backend.Rmd and 

# Main Components

Biotablero is powered by a virtual instance *hosted* in Amazon Web Services (AWS) that receives queries by users and extracts, calculates and returns metrics from a set of ecological and biodiversity variables and indivators (EBV) on custom spatial extents.

It is aimed at two types of users: A first-tier for a general users who interact trough a Graphic User Interface (GUI) displayed on a web browser and a second-tier user who directly interacts with the instance using the R language.
In the second thier it is possible to obtain numeric and mapping (raster) dats for further manipulation, analysis and display.

For the first tier user, it allows to  perform queries to rapidly obtain data in the form tabular and chart data concerning key biodiversity indicators without requiring advanced  programming skills, while more advanced users can access a set of metrics cand tools entralized in a single accesible system.

## Included  Indicators

## Update of information.
 1. Prepare and homologue the new armonized data.
 2. Load into the Instance and store it in the right directory
 3. Update the code (app.R and biotablero_api.r and the other) to deal wit hthe new dataset(s)
 4. Update the existing IDEAM data to add most recent avaiable data.

## Further Development 

Introduction of new capabilities (accept raster data and return graphic outputs) and aiming to finally integrating this with the rest of the biotablero infrastructures managed by Instituto Humboldt.
## 
