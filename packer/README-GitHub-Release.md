## Run the Docker RACK box

Here are very brief instructions how to run the Docker RACK box.  You will find more detailed [instructions](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Docker-RACK-Box) in the RACK Wiki.  You will need to give your Docker Hub username to the RACK team so you can be given access to our Docker Hub repository.

1. Open a terminal window where you can run `docker`.
2. Type `docker pull interran/rack-box:v2.0`
3. Type `docker run --detach -p 80:80 -p 12050-12092:12050-12092 interran/rack-box:v2.0`
4. Visit <http://localhost/> in your browser to view the RACK box's welcome page.

## Run the Virtual RACK box

Here are very brief instructions how to run the Virtual RACK box.  You will find more detailed [instructions](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Virtual-RACK-Box) in the RACK Wiki.

### Hyper-V

1. Download the split Hyper-V zip files.
2. Recombine the split Hyper-V zip files into an unsplit zip file.
3. Unpack the unsplit zip file (creating a rack-box-hyperv folder).
4. Start Hyper-V Manager.
5. Import the Hyper-V VM from the rack-box-hyperv folder.
6. If the Hyper-V VM is connected to the Default Switch, do steps 7 and 8.
7. Create an External Switch.
8. Connect the Hyper-V VM to the External Switch.
9. Start the VM.

### VirtualBox

1. Download the split VirtualBox zip files.
2. Recombine the split VirtualBox zip files into an unsplit zip file.
3. Unpack the unsplit zip file (creating a rack-box-virtualbox folder).
4. Start VirtualBox.
5. Import the VirtualBox VM from the rack-box-virtualbox folder.
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
