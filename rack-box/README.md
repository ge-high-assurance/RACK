<!-- markdownlint-disable line-length -->

# How to build rack-box images

This README is for RACK developers and RACK-in-a-Box power users who
want to build their own rack-box images manually instead of
downloading already-built images from our [Docker
Hub](https://hub.docker.com/repository/docker/gehighassurance/rack-box)
or [GitHub
Releases](https://github.com/ge-high-assurance/RACK/releases) pages.
Note that our RACK repository's continuous integration and release
workflows build development/v\<NUMBER\> release rack-box images
automatically.  Please see these [workflows](../.github/workflows) for
the most up to date way to build rack-box images since the
instructions in this README may be out of date.

## Environment variables needed before building

If you are building on a local area network behind a corporate proxy,
you will need to set some proxy environment variables before building
your rack-box images.  That should be all you need to do; Packer will
inject `http_proxy`, `https_proxy`, and `no_proxy` environment
variables into each box's build if these variables are defined in your
environment.  However, if you run Packer in an Ubuntu shell on Windows
Subsystem for Linux (WSL1), Packer may fail to inject these
environment variables automatically.  If so, you will have to pass the
variables explicitly in your packer build command like this:

`packer build -var "http_proxy=${http_proxy}" -var "https_proxy=${https_proxy}" -var "no_proxy=${no_proxy}" rack-box-docker.json`

## Files needed before building

You will need to download some files into the `files` subdirectory
before building your rack-box images.  Please see the
[commands](../.github/workflows/actions/download/action.yml) used by
our workflows for the most up to date way to download these files,
although we will mention each file here as well:

- `files/fuseki.tar.gz`: Download latest Fuseki tarball from
  <https://jena.apache.org/download/>, renaming it to `fuseki.tar.gz`

- `files/jena.tar.gz`: Download latest Jena tarball from the
  same page, <https://jena.apache.org/download/>, renaming it to
  `jena.tar.gz`

- `files/semtk.tar.gz`: Download latest SemTK tarball from
  <https://github.com/ge-semtk/semtk/releases>, renaming it to
  `semtk.tar.gz`

- `files/style.css`: Download latest CSS stylesheet (`style.css`) for
  rendering markdown from
  [markdown-to-html-github-style](https://github.com/KrauseFx/markdown-to-html-github-style)

- `files/systemctl3.py`: Download latest systemctl script
  (`files/docker/systemctl3.py`) from
  [docker-systemd-replacement](https://github.com/gdraheim/docker-systemctl-replacement)

- `files/rack-assist.tar.gz`: Package the RACK ASSIST (`tar cfz
  RACK/rack-box/files/rack-assist.tar.gz RACK/assist`)

- `files/rack-cli.tar.gz`: Package the RACK CLI (`tar cfz
  RACK/rack-box/files/rack-cli.tar.gz
  RACK/cli/{*.sh,wheels}`), see [Build the RACK
  CLI](#Build-the-RACK-CLI) for build instructions first

- `files/rack-ui.tar.gz`: Package the RACK UI (`tar cfz
  RACK/rack-box/files/rack-ui.tar.gz RACK/rack-ui`)

- `files/{documentation.html,index.html}`: Package the RACK
  documentation, see [Package RACK
  documentation](#Package-RACK-documentation) for instructions

- `files/rack.tar.gz`: Generate OWL/CDR files (see [instructions
  below](#Generate-OWL-CDR-files)) and package the RACK ontology and
  data (`tar cfz RACK/rack-box/files/rack.tar.gz --exclude=.git
  --exclude=.github --exclude=assist --exclude=cli --exclude=rack-box
  --exclude=tests --exclude=tools RACK`)

- `jammy64\*`: Download current Ubuntu 22.04 vagrant box from
  <https://cloud-images.ubuntu.com/jammy/current>, unpacking it in a
  `jammy64` folder (`tar -xf jammy-server-cloudimg-amd64-vagrant.box
  -C RACK/rack-box/jammy64`)

Once you have put these files into the `files` subdirectory, skip to
[Build the rack-box images](#Build-the-rack-box-images) for the next
step.

## Build the RACK CLI

You will need to install these Ubuntu and Python packages before
building the RACK CLI:

    sudo apt install python3-pip
    python3 -m pip install --upgrade pip setuptools wheel

In the cli directory of your checkout of the RACK repository, run
these commands to build the RACK CLI:

    pip3 wheel --wheel-dir=wheels .

If you encounter a `Double requirement given` error, make sure to
clear obsolete files from your wheels directory.

## Package RACK documentation

You will need to install two programs, the [Github Wikito
Converter](https://github.com/yakivmospan/github-wikito-converter) and
the [markdown-to-html](https://github.com/cwjohan/markdown-to-html):

    sudo npm install -g github-wikito-converter markdown-to-html

In the directory above your checkouts of the RACK and RACK.wiki
repositories, run these commands:

    cp RACK.wiki/_Footer.md RACK.wiki/Copyright.md
    gwtc -t RACK-in-a-Box RACK.wiki
    rm RACK.wiki/Copyright.md
    markdown -t RACK-in-a-box -s style.css RACK.wiki/_Welcome.md > index.html
    sed -i -e 's/>NodeGroupService/ onclick="javascript:event.target.port=12058">NodeGroupService/' index.html
    mv documentation.html index.html RACK/rack-box/files

## Generate OWL/CDR files

You will need a running rack-box dev image in order to generate OWL
and CDR files.  Start a rack-box running in the background, then run
these commands, and finally stop the rack-box that was running in the
background once you're done:

    RACK/cli/setup-owl.sh -b
    pip3 install --no-dependencies RACK/cli/wheels/*.whl
    tar xfz RACK/rack-box/files/semtk.tar.gz
    semtk-opensource/standaloneExecutables/target/standaloneExecutables-jar-with-dependencies.jar
    RACK/nodegroups/generate-cdrs.sh semtk-opensource/standaloneExecutables/target/standaloneExecutables-jar-with-dependencies.jar

## Build the rack-box images

You will need to install [Packer](https://www.packer.io/) if you don't
have it.  Packer will read instructions from the rack-box JSON files
and use files in the `files`, `http`, and `scripts` directories to
build the rack-box images.  The following Packer commands will build
Docker and VirtualBox rack-box images:

    packer build rack-box-docker.json
    packer build rack-box-virtualbox.json

When Packer finishes these build commands, it will save the Docker
rack-box image in your local Docker image cache and save the
VirtualBox rack-box image to a new subdirectory called
`output-virtualbox-ovf`.  Your VirtualBox application can import the
image from that subdirectory to create a new virtual machine.

---
Copyright (c) 2021-2023, General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
