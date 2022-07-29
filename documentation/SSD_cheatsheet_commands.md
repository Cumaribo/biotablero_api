# Cheatsheet 

curl "http://localhost:8000/test" ### Chek if is alive

## give sudo granted
```bash
sudo usermod -aG sudo root
```

## Tree -- list files and folders
``` bash
tree -d /data # d only dirs
du ch  *  ## Folder size
du -sh * | sort -h
df -aTh  # check mounted data
df -h . # all disk available/used
```

## Mounting and unmounting external drive 

[reference](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-detaching-volume.html)

```bash
lsblk # check units
lsblk -f # check units (full)
```
### Mount
```bash
sudo mkdir /data  #create /data in the  root direcrory 
```
#### Nitro System
```bash
sudo file -s /dev/nvme1n1 # prepare mount
sudo mkfs -t xfs /dev/nvme1n1 # initialize // work good if unit is empty
sudo mount /dev/nvme1n1 /data # Mount data
```
#### Old system.
```bash
sudo file -s /dev/xvdb # 
sudo mkfs -t xfs /dev/xvdb # initialize // work good if unit is empty
sudo mount /dev/xvdb /data # Mount data
```
### Unmount
#### Nitro System
```bash
sudo mkfs -t xfs -f /dev/nvme1n1 
sudo umount -d -l /dev/nvme1n1 
```
#### Old system
```bash
umount -d /dev/sdh
ls --sort=size -l ## sort by size
sudo mkfs -t xfs -f /dev/xvdg
sudo umount -d -l /dev/xvdg
```
## After reboot -- mount external data
```bash
sudo mount /dev/xvdg /data # Old Instance Type
sudo mount /dev/nvme1n1 /data # Nitro System 
```
## Docker operations

### Start
```bash
sudo docker build -t image_biotablero plumber/ # Create an image from our container
sudo docker run -d -p 8000:8000 --name biotablero -v /data:/data image_biotablero
```
### Check history 
```bash
docker logs biotablero  # Get the container logs
docker ps --all  # Get the running containers
docker service logs biotablero_biotablero-api
docker service logs api_biotablero-api
```
### Get into each docker container for debug --- What does this mean? "Get into each container"?
```bash
sudo docker exec -it biotablero /bin/bash # Get into the container
sudo docker exec -it biotablero_biotablero-api.1.n2rtlbos6mqajn82y49kq76n2 /bin/bash
```
### Stop
```bash
sudo docker stop biotablero # Stop container
sudo docker restart biotablero # restart container
sudo docker image ls # List available images
sudo docker rm biotablero # Remove biotablero container
sudo docker rmi image_biotablero # Remove biotablero image (static, from here the containers are built)
docker service rm $(docker service ls -q) # remove al servicesCheck logs of dockers
```
### Run container
```bash
docker exec -it biotablero_biotablero-api. /bin/bash  
```
**hit tab after the period (.) to get a particular docker name, since has a hash tag** 

## Launch swarm -- usefull for after reboot --------------
```bash
cd ~/plumber/  # go to file location
docker service rm $(docker service ls -q) # stop services
time docker-compose build # Requires a new biotablero_api.R
sudo docker swarm init # Create services
sudo docker stack deploy -c docker-compose.yml biotablero # Init services. take some secods
sleep 30 # Sleep some time before start
curl "http://localhost:8080/test"
curl "http://localhost:8080/polsizekm2?pol=POLYGON((-74.133545%204.144818,-73.817139%203.741479,-74.572998%203.390597,-74.133545%204.144818))"
curl "ec2-3-137-83-192.us-east-2.compute.amazonaws.com:8080/polsizekm2?pol=POLYGON((-74.133545%204.144818,-73.817139%203.741479,-74.572998%203.390597,-74.133545%204.144818))" 
```

## Quick stop and restart
```bash
cd ~/plumber/  # go to file location
docker service rm $(docker service ls -q) # stop services
sudo docker stack deploy -c docker-compose.yml api # init services
```

## Stop all docker services
```bash
docker service rm $(docker service ls -q)
docker service rm $(docker service ls -q)
sudo docker image ls | grep -v rocker | grep -v REPOSITORY | awk '{print $3}' | xargs sudo docker rmi
sudo docker image ls | grep -v rocker | grep -v REPOSITORY | awk '{print $3}' | xargs sudo docker rmi
sudo docker rmi $(sudo docker images -f "dangling=true" -q) # dangling images
sudo docker ps -a | grep Exit | cut -d ' ' -f 1 | xargs sudo docker stop
sudo docker ps -a | cut -d ' ' -f 1 | xargs sudo docker stop
sudo docker ps -a | cut -d ' ' -f 1 | xargs sudo docker rm
#docker rm $(docker ps -aq)
docker rmi biotablero:latest
docker rmi $(docker images -q) # delete all images
docker image ls
sudo docker image prune -a # removes all unused images
sudo docker kill $(docker ps -q) # kill (stop) all running containers
sudo docker rm --force $(docker ps -a -q) #force stop all running containers
```

# Update Shiny (why are there two of these?)
```bash
sudo rm /srv/shiny-server/ssd/*
sudo su - -c "R -e \"shinyParallel::installShinyParallel('/home/ubuntu/ssd/', max.sessions = 25)\"" # home/shinyusername/
sudo rm /srv/shiny-server/ssd2 -R
sudo cp /home/ubuntu/ssd /srv/shiny-server/ssd2 -R
```

# Update Shiny
```bash
sudo rm /srv/shiny-server/gedivis/*
sudo su - -c "R -e \"shinyParallel::installShinyParallel('/home/vmuser/gedivis/', max.sessions = 25)\"" # home/shinyusername/
sudo rm /srv/shiny-server/gedivis2 -R
sudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis2 -R
```

#### File sizes
```bash
ubuntu@ip-172-31-9-164:/$ sudo du -h --max-depth=1 | sort -hr
```
40G     .
22G     ./data
15G     ./var
2.0G    ./usr
1.7G    ./snap
48M     ./boot
24M     ./home
5.7M    ./etc
1.4M    ./run
60K     ./root
56K     ./tmp
16K     ./opt
16K     ./lost+found
4.0K    ./srv
4.0K    ./mnt
4.0K    ./media
4.0K    ./data2
0       ./sys
0       ./proc
0       ./dev

# df ## check size
filesystem      Size  Used Avail Use% Mounted on
/dev/root        30G   30G     0 100% /

#https://linuxize.com/post/how-to-check-disk-space-in-linux-using-the-df-command/
df /
df -h
df -t
df -t ext4
df -x tmpfs
