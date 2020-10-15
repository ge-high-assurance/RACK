# CI Scripts

These scripts are used in CI.

## Using `act` to Run CI Locally

The [`act`][act] tool can be used to run (an
approximation of) the Github Actions workflow locally:

- Download a binary release of Packer for Ubuntu, and place the `packer`
  executable in the `packer/` directory.
- Install `act`
- Generate a Github [personal access token][PAT] (PAT)
- Create a `.secrets` file containing `GITHUB_TOKEN=<your-github-PAT-here>`
- Run `act --secret-file .secrets -P ubuntu-latest=nektos/act-environments-ubuntu:18.04`

The Docker image `nektos/act-environments-ubuntu:18.04` is quite large
(approximately 18GB), so (1) you'll need enough free disk space to store it and
(2) the first execution of `act` takes a while because it downloads this image.

### Troubleshooting

#### "volume is in use"

If you see a message like this:
```
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
Copyright (c) 2020, General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).
