# RACK-box

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

- `files/RACK.nq`: latest RACK database (see build instructions below)

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
  `distribution-2.2.1-SNAPSHOT.tar.gz` from
  <https://oss.sonatype.org/#nexus-search;quick~semtk> and rename it)

- `files/style.css`: stylesheet for index.html (visit
  [markdown-to-html-github-style](https://github.com/KrauseFx/markdown-to-html-github-style)
  and download `style.css` under the MIT License)

- `files/systemctl3.py`: entrypoint and init daemon (visit
  [docker-systemd-replacement](https://github.com/gdraheim/docker-systemctl-replacement)
  and download `files/docker/systemctl3.py` under the European Union
  Public Licence)

The CI release workflow automates downloading these files into the
GitHub runner's RACK-box/files directory.

## Build the latest RACK database

The RACK team has written a RACK [command-line
interface](https://github.com/ge-high-assurance/RACK/tree/master/RACK-Ontology/cli)
and a setup-rack.sh script which will call that command-line interface
to build the latest RACK database.  To use it, clone the RACK git
repository on your computer and run the following commands to set up
an isolated Python virtual environment with all the dependencies
needed to run the RACK CLI:

```shell
sudo apt update
sudo apt install python3-pip python3-virtualenv
git clone git@github.com:ge-high-assurance/RACK.git
cd RACK/RACK-Ontology/cli
virtualenv venv
source venv/bin/activate
pip3 install -r requirements.txt
python3 setup.py install
```

Start a RACK-in-a-Box instance running in a Docker container on your
computer, re-enter the isolated virtual environment if you aren't
still in it, and run setup-rack.sh to build the latest RACK database
on the RACK box:

```shell
$ docker run -detach -p 22:22 -p 80:80 -p 12050-12092:12050-12092 interran/rack-box:v3.0
1dd2747edeb9197b7f439172b7ef60f42aad7c1581d31996be788277545daa90
$ cd RACK/RACK-Ontology/cli
$ source venv/bin/activate
(venv) $ ./setup-rack.sh
Clearing graph
Success Update succeeded
Ingesting ../OwlModels/AGENTS.owl...               OK
Ingesting ../OwlModels/ANALYSIS.owl...             OK
Ingesting ../OwlModels/CONFIDENCE.owl...           OK
Ingesting ../OwlModels/DOCUMENT.owl...             OK
Ingesting ../OwlModels/HAZARD.owl...               OK
Ingesting ../OwlModels/PROCESS.owl...              OK
Ingesting ../OwlModels/PROV-S.owl...               OK
Ingesting ../OwlModels/REQUIREMENTS.owl...         OK
Ingesting ../OwlModels/REVIEW.owl...               OK
Ingesting ../OwlModels/SACM-S.owl...               OK
Ingesting ../OwlModels/SOFTWARE.owl...             OK
Ingesting ../OwlModels/SYSTEM.owl...               OK
Ingesting ../OwlModels/TESTING.owl...              OK
Deleting ingest08 language...                      OK
Deleting ingest09 compiler...                      OK
Deleting ingest10 packager...                      OK
Deleting ingest11 agent...                         OK
Deleting ingest12 code file...                     OK
Deleting ingest13 object file...                   OK
Deleting ingest14 library...                       OK
Deleting ingest15 executable...                    OK
Deleting ingest16 config file...                   OK
Deleting ingest17 package...                       OK
Deleting ingest18 package file...                  OK
Deleting ingest19 compile...                       OK
Deleting ingest01 system...                        OK
Deleting ingest02 interface...                     OK
Deleting ingest03 hazard...                        OK
Deleting ingest04 requirement...                   OK
Deleting ingest05 data dict...                     OK
Deleting ingest06 test...                          OK
Deleting ingest07 test results...                  OK
Deleting ingest08 agent...                         OK
Deleting ingest09 package...                       OK
Deleting ingest10 compile...                       OK
Deleting ingest11 format...                        OK
Deleting ingest12 file...                          OK
Deleting ingest13 component...                     OK
Deleting ingest14 confidence...                    OK
Deleting query Compilation Inputs...               OK
Deleting query Control Flow From Function...       OK
Deleting query Files of a Given Format...          OK
Deleting query Requirements without Tests...       OK
Deleting query Testcase without requirement...     OK
Deleting query System Structure...                 OK
Deleting query Requirements with failed test result...OK
Deleting query Requirement decomposition...        OK
Deleting query Interface structure...              OK
Deleting query Requirements with Tests...          OK
Deleting query Trace Hazards to Tests...           OK
Deleting query Trace Requirements to Tests...      OK
Deleting query Hazard structure...                 OK
Deleting query Terms consumedBy Requirement...     OK
Storing nodegroups...                                       OK
Storing nodegroups...                                       OK
Clearing graph
Success Update succeeded
Loading ingest01 system...                         OK Records: 8       Failures: 0
Loading ingest02 interface...                      OK Records: 4       Failures: 0
Loading ingest03 hazard...                         OK Records: 4       Failures: 0
Loading ingest04 requirement...                    OK Records: 22      Failures: 0
Loading ingest05 data dict...                      OK Records: 29      Failures: 0
Loading ingest06 test...                           OK Records: 8       Failures: 0
Loading ingest07 test results...                   OK Records: 16      Failures: 0
Loading ingest08 agent...                          OK Records: 1       Failures: 0
Loading ingest09 package...                        OK Records: 3       Failures: 0
Loading ingest10 compile...                        OK Records: 14      Failures: 0
Loading ingest11 format...                         OK Records: 6       Failures: 0
Loading ingest12 file...                           OK Records: 19      Failures: 0
Loading ingest13 component...                      OK Records: 4       Failures: 0
Loading ingest14 confidence...                     OK Records: 2       Failures: 0
Ingesting ARP-4754A.owl                   OK
Ingesting DO-178C.owl                     OK
Ingesting DO-330.owl                      OK
Ingesting MIL-STD-881D.owl                OK
Ingesting MIL-STD-881D-AppxB.owl          OK
Ingesting MIL-STD-881D-AppxD.owl          OK
Ingesting MIL-STD-881D-AppxA.owl          OK
Ingesting MIL-STD-881D-AppxC.owl          OK
```

Next, use the Docker Dashboard to open a CLI terminal with a shell
inside the Docker container and run the following commands to back up
the Fuseki triplestore to a new RACK.nq file:

```shell
# curl -Ss -d '' 'http://localhost:3030/$/backup/RACK'
{
  "taskId" : "1" ,
  "requestId" : 505
}
# cd /etc/fuseki/backups
# gunzip RACK*
# mv RACK* RACK.nq
# ls -al
total 1288
drwxr-xr-x 1 fuseki fuseki    4096 Aug 25 15:58 .
drwxr-xr-x 1 fuseki fuseki    4096 Aug 21 19:56 ..
-rw-r--r-- 1 fuseki fuseki 1305526 Aug 25 15:57 RACK.nq
```

Once you have created the new RACK.nq file, copy it into the files
subdirectory in this directory so you can build some new RACK boxes:

```shell
cd files
docker cp <CONTAINER_NAME>:/etc/fuseki/backups/RACK.nq .
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
