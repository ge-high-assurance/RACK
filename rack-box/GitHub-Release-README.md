<!-- markdownlint-disable first-line-heading -->
<!-- markdownlint-disable line-length -->

## Run the Docker RACK box

Here are very brief instructions how to run the Docker RACK box.  You will find more detailed [instructions](https://github.com/ge-high-assurance/RACK/wiki/02-Install-a-Docker-RACK-Box) in the RACK Wiki.  You will need to give your Docker Hub username to the RACK team so you can be given access to our Docker Hub repository.

1. Open a terminal window where you can run `docker`.
2. Type `docker pull gehighassurance/rack-box:v10.0`
3. Type `docker run --detach -p 80:80 -p 12050-12092:12050-12092 -p 3030:3030 gehighassurance/rack-box:v10.0`
4. Visit <http://localhost/> in your browser to view the RACK box's welcome page.

## Run the Virtual RACK box

Here are very brief instructions how to run the Virtual RACK box.  You will find more detailed [instructions](https://github.com/ge-high-assurance/RACK/wiki/03-Install-a-Virtual-RACK-Box) in the RACK Wiki.

1. Download the split VirtualBox zip files.
2. Concatenate the split VirtualBox zip files together.
3. Unzip the newly concatenated zip file.
4. Start VirtualBox.
5. Import the VirtualBox VM from the rack-box-virtualbox-v10.0 folder.
6. Open the VM's Settings.
7. Click on Network.
8. Change the first network adapter from NAT to Bridged.
9. Bind one of your existing network adapters to the bridged network adapter.
10. Start the VM.

## View the RACK box's welcome page

You will need to know which IP address the VM is using after it starts up.  If you can't find the IP address any other way, you can use the VM window to log into the VM and print its IP address:

1. Click in the VM's window.
2. Type "ubuntu" at the username prompt.
3. Type "ubuntu" at the password prompt.
4. Type "ip a" at the shell prompt.
5. Look for the IP address in the second adapter.

Type that IP address in your web browser's address bar, hit Enter, and you should see the RACK box's welcome page appear in your browser.

---
Copyright (c) 2021, General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
