System deployment
===========

This guide provide the steps the user should follow in the Amazon Web Services (AWS) page in order to set up virtual machine. This procedure will generate the API host.


Using the AWS credentials, please replicate the following instructions.

* Get into [AWS](https://aws.amazon.com) web page.

![](www/01.png)




* Deploy the log-in options.

![](www/02.png)




* Access the log-in page

![](www/03.png)




* Provide the credentials

![](www/04.png)




* Into the AWS managment console, select *Launch a virtual machine*

![](www/05.png)




* On services page, select *EC2*¨option.

![](www/06.png)




* Select *Launch instance* buttom. Now we are gonna to select the virtual machine (VM) properties

![](www/07.png)




* Step 1: On the search bar, search *Ubuntu*. This is the VM operative system (OS)

![](www/08.png)




* Select the first match, *Ubuntu Server 18.04LTS*, withon the 64-bit (x86) 

![](www/09.png)



* Step 2: On the VM capacity, select *General purpose, t2.medium*

![](www/10.png)




* Step 3: Don't modify the values. Move forward to the next step

![](www/11.png)



* Step 4: Add extra storage for input data. Add an *EBS* unit, under the name */dev/sdb*, and the size of 50GB. 

![](www/13.png)



* Step 5: Add tags. Don't modify the values. Move forward to the next step

![](www/15.png)



* Step 6: Configure security groups. Open a new port for API connection. Add a TCP port under the 8000 number.

![](www/17.png)



* Step 7: Review instance. Verify the values and move forward.

![](www/18.png)

![](www/19.png)



* Save the key. A file is generated to access the VM. Its important to save this key and keep in a safe place. Consider make a copy. This is the final step.

![](www/20.png)




* On the services console, select the recenlty created instance. Click on the instance, and copy the public URL

![](www/21.png)




Now, let's get into the VM. For that porpouse we need to install [PuTTY](https://www.putty.org) and [PuTTYgen](https://www.puttygen.com/). Those programs allow us to conect the VM and manage the key.


* Open PuTTYgen and load the recently created and saved key 

![](www/22.png)




* Modify the file filter and navigate to the key location

![](www/23.png)

![](www/24.png)




* Save the generated key as a *private* key. Ignore the warning

![](www/25.png)

![](www/26.png)




* Use the same name to ave the key in the new .ppk format

![](www/27.png)



At this point, the key have been converted in a new format. Now let's get into the VM console.


 * Open PuTTY and use the VM public URL. Also find the new converted key. Ignore the warning

![](www/28.png)

![](www/29.png)

![](www/30.png)




 * Type *ubuntu* and hit Enter
 
![](www/31.png)




 * Link the external storage EBS unit to the VM. This allow the system access and recognize this extra storage space a system folder located in `/data`. This command is already implemented in the scripts for the next step: Set up the system. 

![](www/32.png)

![](www/33.png)



```bash
## Don't run

lsblk
sudo mkdir /data          
sudo file -s /dev/xvdb
sudo mkfs -t xfs /dev/xvdb
sudo mount /dev/xvdb /data
```
