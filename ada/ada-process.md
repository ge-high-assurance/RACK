# Install Linux

We need a Linux distribution in which to install NixOS.
[Debian](https://www.debian.org/download) is a perfectly good choice.

The Ada compiler toolchain is only supported on x86/x86\_64 platforms,
sorry Apple Silicon users.

```
apt install git curl docker
```

# Get RACK running locally

RACK gets used to construct the ScrapingToolKit, so we'll need it
running. This service needs to be running while the Python dependencies
are being installed below. It doesn't need to run to do Ada code
processing afterward.

```shell
docker pull gehighassurance/rack-box:dev
docker run --rm --detach -p 80:80 -p 12050-12092:12050-12092 \
    -p 3030:3030 -p8050:8050 gehighassurance/rack-box:dev
```

# Install NixOS

Building the libadalang library ecosystem is not simple task. We've
automated the construction of this environment using Nix, which gives us
complete control over the environment.

[NixOS](https://nixos.org/download.html#download-nix)

```shell
sh <(curl -L https://nixos.org/nix/install) --daemon
```

# Get RACK and install the ada script processor

These commands will get RACK, prepare the Ada toolchain environment, and install
all the Python libraries. The Ada tools have a vary particular build procedure.
We use Nix to reproduce the exact environment that these tools require to load.

```shell
git clone https://github.com:ge-high-assurance/RACK
cd RACK/ada
nix-shell
```

The libadalang Ada library is a dependency for the libadalang Python library.
Once the Ada tool-chain and libraries are installed we can configure the
environment that the extraction script runs in.

```shell
python3 -m venv venv
. venv/bin/activate
pip install -r requirements.txt
```

The above steps are needed the first time to set up the environment. **To
activate a previously configured environment** you can run this subset of
commands:

```shell
cd RACK/ada
nix-shell
. venv/bin/activate
```

# Prepare some Ada code

The libadalang tooling uses the GPRbuild tool to provide the settings needed to
parse a source code directory. Here's a sample `default.gpr` that will likely
need to be tweaked depending on the code you're processing.

Libadalang expects to find a complete set of source files. It understands Ada
syntax and understands how to map between specification and implementation
files.

[GPRbuild](https://learn.adacore.com/courses/GNAT_Toolchain_Intro/chapters/gprbuild.html)

```
project Default is
   for Source_Dirs use ("Code/**");
   package Naming is
     for Casing               use "lowercase";
     for Dot_Replacement      use ".";
     for Spec_Suffix ("Ada")  use ".1.ada";
     for Body_Suffix ("Ada")  use ".2.ada";
   end Naming;
end Default;
```

# Anaylze the code

run the analyzer from the directory with the default.gpr file on the
specification files and then list the outputs. These files can be included in
your RITE workspace.

This generates the CSV files and the `data.yaml` that indexes them.

```shell
~/RACK/ada/run_analysis --gpr default.gpr */*/*.1.ada
ls RACK-DATA
```

# Incorporate extracted dataset into RITE

- Add a new Eclipse project
- Set the project references
- Regenerate manifests

# Build and load ingestion package

Building the manifest consolidates all the projects into a single ZIP file that
can ingested into a RACK instance.

```shell
rack manifest build Package/manifest.yaml package
rack manifest import package.zip
```
