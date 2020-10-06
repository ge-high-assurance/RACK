# RACK-in-a-Box

The tooling in this directory is used to build Docker containers and VM images
containing RACK for ARCOS technical performers. You'll need
[Packer](https://www.packer.io/) to use it.

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

- `files/apache-jena-fuseki-3.16.0.tar.gz`: latest Fuseki release
  (download it from <https://jena.apache.org/download/>)

- `files/documentation.html`: RACK documentation (clone RACK.wiki, run
  `gwtc -t RACK-in-a-Box RACK.wiki/` using [Github Wikito
  Converter](https://github.com/yakivmospan/github-wikito-converter),
  and copy `documentation.html`)

- `files/index.html`: RACK box welcome page (clone RACK.wiki, run
  `markdown -t RACK-in-a-box -s style.css RACK.wiki/Welcome.md >
  index.html` using
  [markdown-to-html](https://github.com/cwjohan/markdown-to-html), and
  copy `index.html`)

- `files/semtk-opensource-bin.tar.gz`: latest SemTK distribution (clone
  semtk-opensource, run ./build.sh, and copy/rename
  `distribution/target/*.tar.gz`)

- `files/style.css`: stylesheet for index.html (visit
  [markdown-to-html-github-style](https://github.com/KrauseFx/markdown-to-html-github-style)
  and download `style.css` under the MIT License)

- `files/systemctl3.py`: entrypoint and init daemon (visit
  [docker-systemd-replacement](https://github.com/gdraheim/docker-systemctl-replacement)
  and download `files/docker/systemctl3.py` under the European Union
  Public Licence)

The script `ci/build-rack-in-a-box.sh` (which is used in the RACK CI pipeline)
automatically downloads some of these, but creates empty documentation files.

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
mv output-hyperv-iso rack-box-hyperv-v2.0
mv output-virtualbox-iso rack-box-virtualbox-v2.0
zip rack-box-hyperv-v2.0.zip -s 1500m -r rack-box-hyperv-v2.0
zip rack-box-virtualbox-v2.0.zip -s 1500m -r rack-box-virtualbox-v2.0
<upload split zip files to GitHub Release page>
```

## Release Process

When preparing a release, additionally follow these steps before building the
final RACK-in-a-Box image.

### Update documentation pages

We will need to update version numbers in the following files or
documentation pages whenever we make a new release:

#### RACK Box

- [ ] [README.md](README.md)
- [ ] [README-Docker-Hub.md](README-Docker-Hub.md)
- [ ] [README-GitHub-Release.md](README-GitHub-Release.md)
- [ ] [rack-box-docker.json](rack-box-docker.json)
- [ ] [rack-box-hyperv.json](rack-box-hyperv.json)
- [ ] [rack-box-virtualbox.json](rack-box-virtualbox.json)

#### RACK Wiki

- [ ] [Home](https://github.com/ge-high-assurance/RACK/wiki)
- [ ] [Install-a-Docker-RACK-Box](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Docker-RACK-Box)
- [ ] [Install-a-Virtual-RACK-Box](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Virtual-RACK-Box)
- [ ] [Welcome](https://github.com/ge-high-assurance/RACK/wiki/Welcome)

### Downloads

The new image should be uploaded to Docker hub, and a new Github release of RACK
should be tagged:

- [ ] [Docker Hub](https://hub.docker.com/repository/docker/interran/rack-box)
- [ ] [GitHub Release](https://github.com/ge-high-assurance/RACK/releases)

Note that creating a GitHub release tags the RACK repo automatically,
but we should manually tag the RACK wiki since its pages go into the
RACK box too.
