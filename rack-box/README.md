<!-- markdownlint-disable line-length -->

# RACK-box

This README is for RACK developers and RACK-in-a-Box power users who
want to know how to build rack-box images.  Normally we let our RACK
repository's GitHub Actions workflow build rack-box images and push or
upload them to our [Docker
Hub](https://hub.docker.com/repository/docker/gehighassurance/rack-box) or
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

- `files/fuseki.tar.gz`: latest Fuseki release (download tarball from
  <https://jena.apache.org/download/> and rename it)

- `files/rack-cli.tar.gz`: A binary distribution of the RACK CLI, see
  [Build the RACK CLI](#Build-the-RACK-CLI).

- `files/rack.tar.gz`: A copy of the RACK ontology and data (clone
  this repo and run `tar cfz RACK/rack-box/files/rack.tar.gz
  --exclude=.git --exclude=.github --exclude=assist --exclude=cli
  --exclude=rack-box --exclude=tests --exclude=tools RACK`)

- `files/documentation.html`: RACK documentation (clone RACK.wiki, run
  `gwtc -t RACK-in-a-Box RACK.wiki/` using [Github Wikito
  Converter](https://github.com/yakivmospan/github-wikito-converter),
  and copy `documentation.html`)

- `files/index.html`: RACK box welcome page (clone RACK.wiki, run
  `markdown -t RACK-in-a-box -s style.css RACK.wiki/Welcome.md >
  index.html` using
  [markdown-to-html](https://github.com/cwjohan/markdown-to-html), and
  copy `index.html`)

- `files/semtk.tar.gz`: latest SemTK binary distribution (download
  tarball from <https://github.com/ge-semtk/semtk/releases> and rename
  it)

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
interface](https://github.com/ge-high-assurance/RACK/tree/master/cli)
and a setup-rack.sh script which will build the latest RACK database
using that command-line interface.  To build a binary distribution of
the RACK CLI, clone the RACK git repository in your home directory
($HOME) and run the following commands:

<!--
Note for documentation authors: These instructions should be kept in sync with
the Github Actions workflows.
-->

```shell
sudo apt update
sudo apt install python3-pip
cd $HOME
git clone git@github.com:ge-high-assurance/RACK.git
cd RACK/cli
python3 -m pip install --upgrade pip setuptools wheel
pip3 wheel --wheel-dir=wheels -r requirements.txt
pip3 wheel --wheel-dir=wheels .
# If you want to install the RACK CLI on your machine...
#python3 -m pip install wheels/*.whl
cd $HOME
tar cfz RACK/rack-box/files/rack-cli.tar.gz RACK/cli/{setup-rack.sh,wheels}
```

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

Before making a new release, we will need to update version numbers or
instructions in the following places:

### RACK Box

- [ ] [Docker-Hub-README.md](Docker-Hub-README.md)
- [ ] [GitHub-Release-README.md](GitHub-Release-README.md)
- [ ] [README.md](README.md)

### RACK Wiki

- [ ] [Install-a-Docker-RACK-Box](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Docker-RACK-Box)
- [ ] [Install-a-Virtual-RACK-Box](https://github.com/ge-high-assurance/RACK/wiki/Install-a-Virtual-RACK-Box)
- [ ] [Welcome](https://github.com/ge-high-assurance/RACK/wiki/_Welcome)

Note that creating a GitHub release tags the RACK repo automatically,
but we need to manually tag the RACK wiki with the same tag in advance
since its pages must go into the rack-box image too.

## Using `act` to Run CI Locally

The [`act`][act] tool can be used to run (an
approximation of) the Github Actions workflow locally:

- Download a binary release of Packer for Ubuntu, and place the `packer`
  executable in the `rack-box/` directory.
- Install `act`
- Generate a Github [personal access token][PAT] (PAT)
- Create a `.secrets` file containing `GITHUB_TOKEN=<your-github-PAT-here>`
- Run `act --secret-file .secrets -P ubuntu-latest=nektos/act-environments-ubuntu:18.04`

The Docker image `nektos/act-environments-ubuntu:18.04` is quite large
(approximately 18GB), so (1) you'll need enough free disk space to store it and
(2) the first execution of `act` takes a while because it downloads this image.

Unfortunately, `act` [does not yet support Ubuntu
20.04](https://github.com/nektos/act-environments/issues/4).

### Troubleshooting

#### "volume is in use"

If you see a message like this:

```text
Error: Error response from daemon: remove act-Build-Lint-shell-scripts-and-the-RACK-CLI: volume is in use
```

You can forcibly stop and remove the `act` Docker containers and their volumes:

```bash
docker stop $(docker ps -a | grep "nektos/act-environments-ubuntu:18.04" | awk '{print $1}')
docker rm $(docker ps -a | grep "nektos/act-environments-ubuntu:18.04" | awk '{print $1}')
docker volume rm $(docker volume ls --filter dangling=true | grep -o -E "act-.+$")
```

There may also be a more precise solution to this issue, but the above works.

#### "permission denied while trying to connect to the Docker daemon socket"

`act` needs to be run with enough privileges to run Docker containers. Try
`sudo -g docker act ...` (or an equivalent invocation for your OS/distro).

[act]: (https://github.com/nektos/act)
[PAT]: https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token

---
Copyright (c) 2021, General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
