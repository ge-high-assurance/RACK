# RACK-box

This README is for RACK developers and RACK-in-a-Box power users who
want to know how to build rack-box images.  Normally we let our RACK
repository's GitHub Actions workflow build rack-box images and push or
upload them to our [Docker
Hub](https://hub.docker.com/repository/docker/interran/rack-box) or
[GitHub Releases](https://github.com/ge-high-assurance/RACK/releases)
pages automatically.

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

You will need to copy the following files into the `files`
subdirectory before building your rack-box images:

- `files/fuseki.tar.gz`: latest Fuseki release (download
  `apache-jena-fuseki-3.16.0.tar.gz` from
  <https://jena.apache.org/download/> and rename it)

- `files/rack.tar.gz`: A binary distribution of the RACK CLI, see
  [Build the RACK CLI](#Build-the-RACK-CLI).

- `files/documentation.html`: RACK documentation (clone RACK.wiki, run
  `gwtc -t RACK-in-a-Box RACK.wiki/` using [Github Wikito
  Converter](https://github.com/yakivmospan/github-wikito-converter),
  and copy `documentation.html`)

- `files/index.html`: RACK box welcome page (clone RACK.wiki, run
  `markdown -t RACK-in-a-box -s style.css RACK.wiki/Welcome.md >
  index.html` using
  [markdown-to-html](https://github.com/cwjohan/markdown-to-html), and
  copy `index.html`)

- `files/semtk.tar.gz`: A binary distribution of SemTK (download
  `distribution-2.2.2-SNAPSHOT.tar.gz` from
  <https://oss.sonatype.org/service/local/artifact/maven/content?r=snapshots&g=com.ge.research.semtk&a=distribution&v=2.2.2-SNAPSHOT&c=bin&e=tar.gz>
  and rename it)

- `files/style.css`: stylesheet for index.html (visit
  [markdown-to-html-github-style](https://github.com/KrauseFx/markdown-to-html-github-style)
  and download `style.css` under the MIT License)

- `files/systemctl3.py`: entrypoint and init daemon (visit
  [docker-systemd-replacement](https://github.com/gdraheim/docker-systemctl-replacement)
  and download `files/docker/systemctl3.py` under the European Union
  Public Licence)

Our GitHub Actions workflow automatically downloads these files into
the GitHub runner's RACK/rack-box/files directory.

## Build the RACK CLI

The RACK team has written a RACK [command-line
interface](https://github.com/ge-high-assurance/RACK/tree/master/RACK-Ontology/cli)
and a setup-rack.sh script which will build the latest RACK database
using that command-line interface.  To build a binary distribution of
the RACK CLI, clone the RACK git repository in your home directory
($HOME) and run the following commands:

<!--
Note for documentation authors: These instructions should be kept in sync with
the RACK CLI README.
-->
```shell
sudo apt update
sudo apt install python3-pip python3-virtualenv
cd $HOME
git clone git@github.com:ge-high-assurance/RACK.git
cd RACK/RACK-Ontology/cli
virtualenv venv
source venv/bin/activate
pip3 install -r requirements.txt
python3 setup.py --quiet install
find venv/bin -type f | xargs perl -p -i -e "s|$HOME|/home/ubuntu|g"
cd $HOME
tar cfz RACK/rack-box/files/rack.tar.gz \
  --exclude=.git \
  --exclude=.github \
  --exclude=.gitignore \
  --exclude=.project \
  --exclude=LICENSE \
  --exclude=README.md \
  --exclude=docs \
  --exclude=rack-box \
  --exclude=tools \
  RACK
```

The reason for the find | xargs commands is to allow the isolated
Python virtual environment used by the RACK CLI to be copied safely
from your home directory to the rack-box's home directory.

## Build the rack-box images

You will need to install [Packer](https://www.packer.io/) if you don't
have it.  Packer will read instructions from the rack-box JSON files
and use files in the `files`, `http`, and `scripts` directories to
build each rack-box image.  The following Packer commands will build
Docker, Hyper-V, and VirtualBox rack-box images:

```shell
packer build rack-box-docker.json
packer build rack-box-hyperv.json
packer build rack-box-virtualbox.json
```

Following these commands, the Docker rack-box image will be added to
your local Docker images and the Hyper-V & VirtualBox rack-box images
will be exported to output-hyperv-iso and output-virtualbox-iso
subdirectories.  You can directly import these subdirectories into new
virtual machines in your Hyper-V and VirtualBox GUI windows.

## Release process

To make a new release, we will need to perform the following steps:

1. Update version numbers in some files (see the next section).
2. Tag the RACK wiki with the version tag name since our GitHub
   Actions workflow will check out the wiki using the same tag.
3. Click the `Draft a new release` button in the GitHub Releases page,
   enter the release name, version tag name, and description, and
   click the `Publish release` button.
4. Our GitHub Actions release workflow will build and push or upload
   the rack-box images to Docker Hub and GitHub automatically.

## Update documentation pages

Before making a new release, we will need to update version numbers in
the following places:

### RACK Box

- [ ] [README.md](README.md)
- [ ] [README-Docker-Hub.md](README-Docker-Hub.md)
- [ ] [README-GitHub-Release.md](README-GitHub-Release.md)

### RACK Wiki

- [ ] [Install-a-Docker-RACK-Box](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Docker-RACK-Box)
- [ ] [Install-a-Virtual-RACK-Box](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Virtual-RACK-Box)
- [ ] [Welcome](https://github.com/ge-high-assurance/RACK/wiki/Welcome)

### Download pages

- [ ] [Docker Hub](https://hub.docker.com/repository/docker/interran/rack-box)
- [ ] [GitHub Releases](https://github.com/ge-high-assurance/RACK/releases)

Our GitHub Actions release workflow automates building and uploading
the rack-box images to Docker Hub and GitHub, but we need to update
the description of these rack-box images with the new release version,
changes, etc.

Note that creating a GitHub release tags the RACK repo automatically,
but we need to manually tag the RACK wiki with the same tag in advance
since its pages must go into the rack-box image too.
