# Docker configuration

This steps will set up, download, and launch the Docker containers that will host the API.
For this procedure we require the following files:

1. biotablero_bash.sh: Bash instructions explained in this document. 
2. main.R: R script startin API
3. biotablero_api.R: API and indicators functions.
4. start_mongo_speciesrecords.R: R script for populate the Mongo database
5. dockerfile: Instructions for setting-up 'bioablero' docker
6. docker-compose: Instructions for synconrize and 'biotablero' and 'mongo' docker containers
7. init-mongo.js: Mongo data base user creation

An extra file ´readme´ contains final detailes required for the API launching.
The following instructions are contained into ´biotablero_bash.sh´


### Install programs and set up the system host

Volumen mount. Make accesible the external space (50GB) required for data storage. The fodler will be ´/data´

```bash
lsblk
sudo mkdir /data
sudo file -s /dev/xvdb
sudo mkfs -t xfs /dev/xvdb
sudo mount /dev/xvdb /data
```

Install docker and docker-compose, 7zip and tree.

```bash
sudo apt-get update -y
sudo apt install tree -y
sudo apt-get install p7zip-full -y
sudo apt install docker.io -y
```

Run Docker commands without sudo prefix. This may require restart the host connection. That's why some docker commands bellow will use ´sudo´ 

```bash
sudo usermod -aG docker ubuntu
sudo systemctl start docker
sudo systemctl enable docker
```

Download docker-compose
```bash
sudo curl -L "https://github.com/docker/compose/releases/download/1.25.5/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
```

### Download compressed datasets

Let's download compressed datasets hosted in the cloud into ´/data´ folder



#### Species data sets

Biomodelos (Biomod.7z). 6113 species datasets from 1340097 locations (.RData files)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1Qu5H8Z5c91KVjVs_HT_0Qc4ypjwlojLj' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1Qu5H8Z5c91KVjVs_HT_0Qc4ypjwlojLj" -O /data/Biomod.7z && rm -rf /tmp/cookies.txt
```

UICN shapefiles from 3082 species (uicn.7z)
```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1zLzh3TAp0Tz9NdI5JDu4uTSyQt13tRBz' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1zLzh3TAp0Tz9NdI5JDu4uTSyQt13tRBz" -O /data/uicn.7z && rm -rf /tmp/cookies.txt
```

GBIF records (recs.7z). RData file to be uploaded into Mongo data base
```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1Cv5k7YIzgYbR2Uv5DZca_-kY_6IWLfYB' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1Cv5k7YIzgYbR2Uv5DZca_-kY_6IWLfYB" -O /data/recs.7z && rm -rf /tmp/cookies.txt
```


#### Ecosystems


Hansen and IDEAM forest layers (forest.7z). Two .tif layers for forest cover and other two for forest loss year.
```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1fK4QnUZi9uluu0vtUZlNlRVYDJFfu_Xc' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1fK4QnUZi9uluu0vtUZlNlRVYDJFfu_Xc" -O /data/forest.7z && rm -rf /tmp/cookies.txt	
 ```
 
Biomes (biome), Biotic regions (bioticreg) and colective areas (colectareas) shapefiles (SingleLayers1.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1mFQLlTjYQRytldLNWNzEflNMW0smNBFp' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1mFQLlTjYQRytldLNWNzEflNMW0smNBFp" -O /data/singleLayers1.7z && rm -rf /tmp/cookies.txt
```


Contain Tropical dry forest (tropdryforest), Special managment areas (sma), protected areas (protectareas), paramos (params), and wetlands (hum) shapefiles (SingleLayers2.7z).

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1zWa27R5ob5rCvHmCpOKo8dnBVD3oyOS7' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1zWa27R5ob5rCvHmCpOKo8dnBVD3oyOS7" -O /data/singleLayers2.7z && rm -rf /tmp/cookies.txt
```


Ecosystem red list criteria (ecouicn) shapefile (SingleLayers3.7z).

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=16uCVl_2GvDgBj_UEnDh5IcfqUMlfFUmq' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=16uCVl_2GvDgBj_UEnDh5IcfqUMlfFUmq" -O /data/singleLayers3.7z && rm -rf /tmp/cookies.txt
```


Compensation factor (faccomp) shapefile (SingleLayers4.7z)

```bash
wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1TmWXAEK6NOzEXxeok8BZ7-xJTing_-Or' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1TmWXAEK6NOzEXxeok8BZ7-xJTing_-Or" -O /data/singleLayers4.7z && rm -rf /tmp/cookies.txt
```


#### Corine Land cover


2000-2002 Corine land cover, level 1 shapefile (N1_2000_2002.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1H9ZoebF7f4l6T1aUlyB8xFdWWh1tZzqg' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1H9ZoebF7f4l6T1aUlyB8xFdWWh1tZzqg" -O /data/N1_2000_2002.7z && rm -rf /tmp/cookies.txt
```


2005-2009 Corine land cover, level 1 shapefile (N1_2005_2009.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1Q2RA5ZMAmthtVy9Ha837dBkRkUUi-hIR' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1Q2RA5ZMAmthtVy9Ha837dBkRkUUi-hIR" -O /data/N1_2005_2009.7z && rm -rf /tmp/cookies.txt	
```


2010-2012 Corine land cover, level 1 shapefile (N1_2010_2012.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1W7CPQxc2jjbAoNCqx4G7RWrKbTFiTDLi' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1W7CPQxc2jjbAoNCqx4G7RWrKbTFiTDLi" -O /data/N1_2010_2012.7z && rm -rf /tmp/cookies.txt	
```

2000-2002 Corine land cover, level 2 shapefile (N2_2000_2002.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1otea_0veqAsXVFPFAQJR2VVhyItWpKEy' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1otea_0veqAsXVFPFAQJR2VVhyItWpKEy" -O /data/N2_2000_2002.7z && rm -rf /tmp/cookies.txt
```


2005-2009 Corine land cover, level 2 shapefile (N2_2005_2009.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1irMemuqGv6uigmRf31BMXKG3O-tfn76u' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1irMemuqGv6uigmRf31BMXKG3O-tfn76u" -O /data/N2_2005_2009.7z && rm -rf /tmp/cookies.txt	
```


2010-2012 Corine land cover, level 2 shapefile (N2_2010_2012.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=14iojjoWEk2jztE8WvAeLqPQCTNXNGYBv' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=14iojjoWEk2jztE8WvAeLqPQCTNXNGYBv" -O /data/N2_2010_2012.7z && rm -rf /tmp/cookies.txt	
```


2000-2002 Corine land cover, level 3 shapefile (N3_2000_2002.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1b2AfYhRpecBNGj0eUujal3iMFoUNPNkp' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1b2AfYhRpecBNGj0eUujal3iMFoUNPNkp" -O /data/N3_2000_2002.7z && rm -rf /tmp/cookies.txt
```


2005-2009 Corine land cover, level 3 shapefile (N3_2005_2009.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1Cpue_Sk5nJJFHYzMNnaLvp6Zm8niccGP' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1Cpue_Sk5nJJFHYzMNnaLvp6Zm8niccGP" -O /data/N3_2005_2009.7z && rm -rf /tmp/cookies.txt	
```


2010-2012 Corine land cover, level 3 shapefile (N3_2010_2012.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1lYYowTIwYSEyquJY-dCodbCcRI6iTNKD' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1lYYowTIwYSEyquJY-dCodbCcRI6iTNKD" -O /data/N3_2010_2012.7z && rm -rf /tmp/cookies.txt	
```

#### Other data

Species red list index tables, and bilogical records density raster (rli_surface.tar)
```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1s6VDlh5IU_rZBBxYxbXGjjlXWAgVYz5d' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1s6VDlh5IU_rZBBxYxbXGjjlXWAgVYz5d" -O /data/rli_surface.tar && rm -rf /tmp/cookies.txt	

```


Spatial templates shapefiles (templates.7z)

```bash
sudo wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1amBqHS3ymXHSfG2lA_1K7BiN-ozeaIjb' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1amBqHS3ymXHSfG2lA_1K7BiN-ozeaIjb" -O /data/templates.7z && rm -rf /tmp/cookies.txt
```



### Create folders and unzip data

Create directories according to API structure

```bash
sudo mkdir /data/clc
sudo mkdir /data/forest
sudo mkdir /data/rli
sudo mkdir /data/output
sudo mkdir /data/singleLayers
sudo mkdir /data/species
sudo mkdir /data/species/uicn
sudo mkdir /data/species/biomod
sudo mkdir /data/species/records
sudo mkdir /data/surface
sudo mkdir /data/templates
sudo mkdir /data/tempR
```


Extract compressed data into created folders

```bash
sudo 7za x -o/data/clc /data/N1_2000_2002.7z
sudo 7za x -o/data/clc /data/N1_2005_2009.7z
sudo 7za x -o/data/clc /data/N1_2010_2012.7z

sudo 7za x -o/data/clc /data/N2_2000_2002.7z
sudo 7za x -o/data/clc /data/N2_2005_2009.7z
sudo 7za x -o/data/clc /data/N2_2010_2012.7z

sudo 7za x -o/data/clc /data/N3_2000_2002.7z -aos
sudo 7za x -o/data/clc /data/N3_2005_2009.7z -aos
sudo 7za x -o/data/clc /data/N3_2010_2012.7z -aos

sudo 7za x -o/data/singleLayers /data/singleLayers1.7z
sudo 7za x -o/data/singleLayers /data/singleLayers2.7z
sudo 7za x -o/data/singleLayers /data/singleLayers3.7z
sudo 7za x -o/data/singleLayers /data/singleLayers4.7z

sudo 7za x -o/data/species /data/Biomod.7z -y 
sudo 7za x -o/data/species/uicn /data/uicn.7z
sudo 7za x -o/data/species/records /data/recs.7z

sudo 7za x -o/data/forest /data/forest.7z

sudo 7za x -o/data /data/rli_surface.tar
sudo 7za x -o/data /data/templates.7z
```


### Launch docker

Execute docker-compose command to build ´biotablero´ and ´mongo´ docker images and containers

```bash
sudo docker-compose up -d
```


### Populate MongoDB data base

Launch ´start_mongo_speciesrecords.R´ and upload biological records into mongo database

```bash
sudo docker exec -it biotablero Rscript start_mongo_speciesrecords.R
```


### Confirm '/data' folder structure

Check the 
```bash
tree -d /data # -d : List directories only
```


```bash
# /data
# ├── clc
# ├── forest
# ├── output
# ├── rli
# ├── singleLayers
# ├── species
# │   ├── biomod
# │   │   └── spByCell
# │   ├── records
# │   └── uicn
# ├── surface
# ├── tempR
# └── templates 
```


#### Folder size


Check folder size. 

```bash
sudo du /data
```
