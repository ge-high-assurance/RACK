# RACK-box

These instructions are for manually building VM and Docker images containing a
RACK distribution. These are normally built by the RACK CI system, and are
published to [the corresponding Github Releases page](https://github.com/ge-high-assurance/RACK/tags).

Install [Packer](https://www.packer.io/) if you don't have it.  Next,
use it to build Docker and Virtual RACK boxes for ARCOS technical
performers from an Ubuntu 20.04 Docker image or ISO file.

## Environment variables needed before building

If you are building on a local area network behind a corporate proxy,
you will need to set some proxy environment variables before building
your RACK boxes.  That should be all you need to do; Packer will
inject `http_proxy`, `https_proxy`, and `no_proxy` into each box's
build if these variables are defined and present in your environment.
However, if you run Packer in an Ubuntu shell on Windows Subsystem for
Linux (WSL), Packer may fail to inject these environment variables
automatically.  If so, you will have to pass the variables explicitly
in your packer build commands like this:

`packer build -var "http_proxy=${http_proxy}" -var "https_proxy=${https_proxy}" -var "no_proxy=${no_proxy}" rack-box-docker.json`

## Files needed before building

You will need to manually download or copy the following files into
the `files` subdirectory before building:

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

- `files/semtk.tar.gz`: latest SemTK distribution (download
  `distribution-2.2.2-SNAPSHOT.tar.gz` from <https://oss.sonatype.org/service/local/artifact/maven/content?r=snapshots&g=com.ge.research.semtk&a=distribution&v=2.2.2-SNAPSHOT&c=bin&e=tar.gz> and rename it)

- `files/style.css`: stylesheet for index.html (visit
  [markdown-to-html-github-style](https://github.com/KrauseFx/markdown-to-html-github-style)
  and download `style.css` under the MIT License)

- `files/systemctl3.py`: entrypoint and init daemon (visit
  [docker-systemd-replacement](https://github.com/gdraheim/docker-systemctl-replacement)
  and download `files/docker/systemctl3.py` under the European Union
  Public Licence)

The CI release workflow automates downloading these files into the
GitHub runner's RACK-box/files directory.

## Build the RACK CLI

The RACK team has written a RACK [command-line
interface](https://github.com/ge-high-assurance/RACK/tree/master/RACK-Ontology/cli)
and a setup-rack.sh script which will call that command-line interface
to build the latest RACK database.  To use it, clone the RACK git
repository on your computer and run the following commands to set up
an isolated Python virtual environment with all the dependencies
needed to run the RACK CLI:

<!--
Note for documentation authors: These instructions should be kept in sync with
the RACK CLI README.
-->
```shell
sudo apt update
sudo apt install python3-pip python3-virtualenv
git clone git@github.com:ge-high-assurance/RACK.git
cd RACK/RACK-Ontology/cli
virtualenv venv
source venv/bin/activate
pip3 install -r requirements.txt
python3 setup.py --quiet install
find venv/bin -type f | xargs perl -p -i -e 's|${{ github.workspace }}|/home/ubuntu|g'
cd ${{ github.workspace }}
tar cfz RACK/rack-box/files/rack.tar.gz --exclude=.git --exclude=.github --exclude=RACK-Ontology/assist --exclude=ci --exclude=packer --exclude=rack-box --exclude=tools RACK
```

## Build the RACK boxes

Packer will use instructions in each JSON file and files in the
`files`, `http`, and `scripts` directories to build each RACK box.
The following commands will build Docker, Hyper-V, and VirtualBox RACK
boxes, although you probably will have to toggle your hypervisor
launch type setting and reboot your Windows 10 computer between the
Hyper-V and VirtualBox builds:

```shell
packer build rack-box-docker.json
packer build rack-box-hyperv.json
packer build rack-box-virtualbox.json
```

Packer will build, commit, and tag the Docker RACK box image itself.
We could make Packer push the Docker image to our DockerHub repository
too, but the push usually fails after 5 minutes so we do that push
manually ourselves.  We nearly always have to retry the push several
times until Docker eventually pushes the image to our Docker Hub
repository successfully.  (Docker, why does your push to Docker Hub
fail so often??)

Packer will build and export the Hyper-V and VirtualBox RACK boxes to
output-hyperv-iso and output-virtualbox-iso directories itself.
Renaming both directories, creating split zip files, and uploading the
split zip files to make a new GitHub release are currently manual
steps that we may automate with a CI workflow in the future:

```shell
mv output-hyperv-iso rack-box-hyperv-v3.0
mv output-virtualbox-iso rack-box-virtualbox-v3.0
zip rack-box-hyperv-v3.0.zip -s 1500m -r rack-box-hyperv-v3.0
zip rack-box-virtualbox-v3.0.zip -s 1500m -r rack-box-virtualbox-v3.0
<upload split zip files to GitHub Release page>
```

## Release process

When preparing a release, we will need to perform the following steps:

1. Update version numbers in some files (see the next section).
2. Tag the RACK wiki with the release tag name since the CI workflow will use the tag name when checking out the wiki files.
3. Create a release in the GitHub Release page via the `Draft a new release` button, enter the release title, tag name, and description, and click `Publish`.
4. The CI release workflow will build and attach/push the rack-box images automatically.

## Update documentation pages

When preparing a release, we will need to update version numbers in
the following files or documentation pages before building the
RACK-in-a-Box release images:

### RACK Box

- [ ] [README.md](README.md)
- [ ] [README-Docker-Hub.md](README-Docker-Hub.md)
- [ ] [README-GitHub-Release.md](README-GitHub-Release.md)
- [ ] [rack-box-docker.json](rack-box-docker.json)
- [ ] [rack-box-hyperv.json](rack-box-hyperv.json)
- [ ] [rack-box-virtualbox.json](rack-box-virtualbox.json)

### RACK Wiki

- [ ] [Home](https://github.com/ge-high-assurance/RACK/wiki)
- [ ] [Install-a-Docker-RACK-Box](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Docker-RACK-Box)
- [ ] [Install-a-Virtual-RACK-Box](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Virtual-RACK-Box)
- [ ] [Welcome](https://github.com/ge-high-assurance/RACK/wiki/Welcome)

### Downloads

- [ ] [Docker Hub](https://hub.docker.com/repository/docker/interran/rack-box)
- [ ] [GitHub Release](https://github.com/ge-high-assurance/RACK/releases)

The CI release workflow automates building and uploading the
RACK-in-a-Box release images to Docker Hub and GitHub, but we need to
update the description of these release images (version number,
changes, etc.).

Note that creating a GitHub release tags the RACK repo automatically,
but we should manually tag the RACK wiki since its pages go into the
RACK box too.
