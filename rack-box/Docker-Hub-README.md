<!-- markdownlint-disable line-length -->

# Install a Docker RACK Box

You may need to increase the resources given to Docker in order to run a RACK box.  Click the right mouse button on Docker's whale icon in the system tray and select "Settings".  When the Settings window appears, click on Resources and see whether the following resource settings appear.  If you don't see these resource settings, it means you don't need to change anything; Docker can use all of your computer's CPUs and 80% of your computer's RAM if it needs to.

If you do see these resource settings, make the following changes:

1. Increase the number of CPUs to 4 if you have enough CPUs (2 may be enough if you don't have many CPUs).
2. Increase the amount of Memory to 4.00 GB (or more if you have plenty of RAM).
3. Click the Apply & Restart button to restart Docker with the new resource settings.

Now you are ready to start your RACK box.  Type the following commands to download the Docker RACK box and run it on your computer:

```shell
docker pull gehighassurance/rack-box:v6.0
docker run --detach -p 80:80 -p 12050-12092:12050-12092 -p 3030:3030 gehighassurance/rack-box:v6.0
```

Type "localhost" in your web browser's address bar, hit Enter, and you should see your RACK box's welcome page appear in your browser.  The welcome page will tell you some things you can do with your RACK box.

---
Copyright (c) 2021, General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
