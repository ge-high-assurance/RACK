<!-- markdownlint-disable line-length -->

# How to build rack-box images

This README is for RACK developers and RACK-in-a-Box power users who
want to build their own rack-box images manually instead of
downloading already-built images from our [Docker
Hub](https://hub.docker.com/repository/docker/gehighassurance/rack-box)
or [GitHub
Releases](https://github.com/ge-high-assurance/RACK/releases) pages.
Note that our RACK repository's continuous integration and release
workflows built these rak-box images automatically.  Please see these
[workflows](../.github/workflows) for the most up to date way to build
rack-box images since the instructions in this README may be out of
date.

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

You will need to download 9 files into the `files` subdirectory before
building your rack-box images.  Please see the
[commands](../.github/workflows/actions/download/action.yml) used by
our workflows for the most up to date way to download these files,
although we will mention each file here as well:

- `files/fuseki.tar.gz`: Download latest Fuseki release tarball from
  <https://jena.apache.org/download/> and rename it (note we still are
  using version 3.16.0 instead of the latest release, though)

- `files/semtk.tar.gz`: Download latest SemTK release tarball from
  <https://github.com/ge-semtk/semtk/releases> and rename it

- `files/style.css`: Download latest CSS stylesheet (`style.css`) for
  rendering markdown from
  [markdown-to-html-github-style](https://github.com/KrauseFx/markdown-to-html-github-style)

- `files/systemctl3.py`: Download latest systemctl script
  (`files/docker/systemctl3.py`) from
  [docker-systemd-replacement](https://github.com/gdraheim/docker-systemctl-replacement)

- `files/rack.tar.gz`: Package the RACK ontology and data (`tar cfz
  RACK/rack-box/files/rack.tar.gz --exclude=.git --exclude=.github
  --exclude=assist --exclude=cli --exclude=rack-box --exclude=tests
  --exclude=tools RACK`)

- `files/rack-assist.tar.gz`: Package the RACK ASSIST (`tar cfz
  RACK/rack-box/files/rack-assist.tar.gz RACK/assist`)

- `files/rack-cli.tar.gz`: Package the RACK CLI (`tar cfz
  RACK/rack-box/files/rack-cli.tar.gz
  RACK/cli/{*.sh,wheels}`), see [Build the RACK
  CLI](#Build-the-RACK-CLI) for build instructions first

- `files/{documentation.html,index.html}`: Package the RACK
  documentation, see [Package RACK
  documentation](#Package-RACK-documentation) for instructions

Once you have put these 9 files into the `files` subdirectory, skip to
[Build the rack-box images](#Build-the-rack-box-images) for the next
step.

## Build the RACK CLI

You will need to install these Ubuntu and Python packages before
building the RACK CLI:

    sudo apt install python3-pip
    python3 -m pip install --upgrade pip setuptools wheel

In the cli directory of your checkout of the RACK repository, run
these commands to build the RACK CLI:

    pip3 wheel --wheel-dir=wheels -r requirements.txt
    pip3 wheel --wheel-dir=wheels .
    
If you encounter a `Double requirement given` error, make sure to
clear your wheels directory from obsolete files.

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

## Build the rack-box images

You will need to install [Packer](https://www.packer.io/) if you don't
have it.  Packer will read instructions from the rack-box JSON files
and use files in the `files`, `http`, and `scripts` directories to
build the rack-box images.  The following Packer commands will build
Docker, Hyper-V, and VirtualBox rack-box images:

    packer build rack-box-docker.json
    packer build rack-box-hyperv.json
    packer build rack-box-virtualbox.json

When Packer finishes these build commands, it will save the Docker
rack-box image in your local Docker image cache and save the Hyper-V &
VirtualBox rack-box images to new subdirectories called
`output-hyperv-iso` and `output-virtualbox-iso`.  Your Hyper-V and
VirtualBox GUI program can import these subdirectories directly into
newly created virtual machines.

### Troubleshooting

### Using `act` to run CI locally

The [act](https://github.com/nektos/act) tool can be used to run (an
approximation of) the Github Actions workflows locally:

- Download a binary release of Packer for Ubuntu, and place the
  `packer` executable in the `rack-box/` directory
- Install `act`
- Generate a Github [personal access
  token](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token)
- Create a `.secrets` file containing
  `GITHUB_TOKEN=<your-github-PAT-here>`
- Run `act --secret-file .secrets -P
  ubuntu-latest=nektos/act-environments-ubuntu:18.04`

The Docker image `nektos/act-environments-ubuntu:18.04` is quite large
(approximately 18GB), so (1) you'll need enough free disk space to
store it and (2) the first execution of `act` takes a while because it
downloads this image.

Unfortunately, `act` [does not yet support Ubuntu
20.04](https://github.com/nektos/act-environments/issues/4).

#### "volume is in use"

If you see a message like this:

    Error: Error response from daemon: remove act-Build-Lint-shell-scripts-and-the-RACK-CLI: volume is in use

You can forcibly stop and remove the `act` Docker containers and their volumes:

    docker stop $(docker ps -a | grep "nektos/act-environments-ubuntu:18.04" | awk '{print $1}')
    docker rm $(docker ps -a | grep "nektos/act-environments-ubuntu:18.04" | awk '{print $1}')
    docker volume rm $(docker volume ls --filter dangling=true | grep -o -E "act-.+$")

There may also be a more precise solution to this issue, but the above works.

#### "permission denied while trying to connect to the Docker daemon socket"

`act` needs to be run with enough privileges to run Docker containers. Try
`sudo -g docker act ...` (or an equivalent invocation for your OS/distro).

---
Copyright (c) 2021, General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
