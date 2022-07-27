# biotablero_api
API for Biotablero at Instituto Alexander von Humboldt/Rsensus Lab @ Temple University 
# Purpose
Set up and maintain the biotablero backend infrastructure. 

1. Data update and expansion.
2. New capabilities/functions 

# Introduction

# Main Components

Biotablero is powered by a virtual instance *stored* in Amazon Web Services (AWS) that receives queries by users and extracts, calculates and returns metrics from a set of ecological and biodiversity variables and indivators (EBV) on custom spatial extents.

It is aime to two types of users: A first-tier for a general users who interact trouch a Graphic User Interface (GUI) displayed a web browser and a second-tier user who directly interacts with the instance using the R language.
In the second thier it is possible to obtain numeric and mapping (raster) dats for further manipulation, analyisis and display.

For the first tier user, it allows to  perform queries to rapidly obtain data in the form tabular and chart data concerning key biodiversity indicators without requiring advanced  programming skills, while more advanced users can access a set of metrics cand tools entralized in a single accesible system.

## Included  Indicators

## Update of information.
 1. Prepare and homologue the new armonized data to Add the new armonized data
 2. Load into the Instance and store it in the roght directory
 3. Update the code (app.R and biotablero_api.r and the other) to deal wit hthe new dataset(s)
 4. Get it to work
 5. Updatethe existing IDEAM data.

## Further Development 

Introduction of new capabilities (accept raster data and return imagey) and aiming to finally integrating this with the rest of the biotablero infrastructured managed by Instituto Humboldt.
## 
