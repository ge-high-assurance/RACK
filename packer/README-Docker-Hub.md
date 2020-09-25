# Install a Docker RACK Box

You will need to increase the resources given to Docker in order to
run a RACK box.  Click the right mouse button on Docker's whale icon
in the system tray and select "Settings".  When the Settings window
appears, click on Resources and make the following changes to the
resource settings:

1. Increase the number of CPUs to 4 if you have enough CPUs (2 may be
   enough if you don't have many CPUs).
2. Increase the amount of Memory to 4.00 GB (or more if you have
   plenty of RAM).
3. Click the Apply & Restart button to restart Docker with the new
   resource settings.

Now you are ready to start your RACK box.  Type the following commands to
download the Docker RACK box and run it on your computer:

```shell
docker pull interran/rack-box:v2.0
docker run --detach -p 80:80 -p 12050-12092:12050-12092 interran/rack-box:v2.0
```

Type "localhost" in your web browser's address bar, hit Enter, and you
should see your RACK box's welcome page appear in your browser.  The
welcome page will tell you some things you can do with your RACK box.
