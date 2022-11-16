<!-- markdownlint-disable first-line-heading -->
<!-- markdownlint-disable line-length -->

## Run a RACK box using a Linux container

Here are very brief instructions how to run a RACK box using a Linux container.  You will find more detailed [instructions](https://github.com/ge-high-assurance/RACK/wiki/02-Run-a-RACK-Box-container) in the RACK Wiki.  You will need to give your Docker Hub username to the RACK team so you can be given access to our Docker Hub repository.

1. Open a terminal window where you can run `docker`.
2. Type `docker pull gehighassurance/rack-box:v11`
3. Type `docker run --detach -p 3030:3030 -p 8050:8050 -p 8080:80 -p 12050-12091:12050-12091 gehighassurance/rack-box:v11`
4. Visit <http://localhost:8080> in your browser to view the RACK box's welcome page.

## Run a RACK box using a virtual machine

Here are very brief instructions how to run a RACK box using a virtual machine.  You will find more detailed [instructions](https://github.com/ge-high-assurance/RACK/wiki/03-Run-a-RACK-Box-VM) in the RACK Wiki.

1. Download the split VirtualBox zip files.
2. Concatenate the split VirtualBox zip files together.
3. Unzip the newly concatenated zip file.
4. Start VirtualBox.
5. Import the VirtualBox VM from the rack-box-virtualbox-v11 folder.
6. Start the VM.
7. Visit <http://localhost:8080> in your browser to view the RACK box's welcome page.

---
Copyright (c) 2022, General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
