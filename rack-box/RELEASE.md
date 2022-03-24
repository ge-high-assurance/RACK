# Process to make a new RACK release

When it is time to make a new release of RACK, perform the following
steps:

1. Update version numbers or instructions in the following files
   within the RACK and RACK.wiki repositories:

   RACK/

   - [ ] [Docker-Hub-README.md](Docker-Hub-README.md)
   - [ ] [GitHub-Release-README.md](GitHub-Release-README.md)
   - [ ] [RELEASE.md](RELEASE.md)
   - [ ] [setup-owl.sh](../cli/setup-owl.sh)

   RACK.wiki/

   - [ ] [01-Release-Schedule](https://github.com/ge-high-assurance/RACK/wiki/01-Release-Schedule)
   - [ ] [02-Run-a-RACK-Box-container](https://github.com/ge-high-assurance/RACK/wiki/02-Run-a-RACK-Box-container)
   - [ ] [03-Run-a-RACK-Box-VM](https://github.com/ge-high-assurance/RACK/wiki/03-Run-a-RACK-Box-VM)
   - [ ] [Home](https://github.com/ge-high-assurance/RACK/wiki/Home)
   - [ ] [Welcome](https://github.com/ge-high-assurance/RACK/wiki/_Welcome)

   After you save these files, commit and push those changes to both
   the RACK and RACK.wiki repositories:

   ```shell
   cd RACK.wiki
   git commit -a -m "Fix markdownlint warnings and update versions for next release"
   git push
   cd ../RACK
   git commit -a -m "Update versions for next release"
   git push
   ```

2. Manually tag the RACK.wiki repository with the new version number
   as a tag name since our GitHub Actions release workflow will check
   out both the RACK and RACK.wiki repositories with the same tag
   name.  Note that publishing a GitHub release in step 3 tags the
   RACK repository automatically, but we need to manually tag the
   RACK.wiki repository with the same tag in advance:

   ```shell
   cd RACK.wiki
   git tag v10.0
   git push --tag
   ```

3. Click the `Draft a new release` button in the GitHub Releases page,
   enter the release name, version tag name, and description, and
   click the `Publish release` button.

4. Monitor our GitHub Actions release workflow to make sure it builds,
   pushes, and uploads the rack-box images to Docker Hub and GitHub
   successfully.

---
Copyright (c) 2021, General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced
Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in
this material are those of the author(s) and do not necessarily
reflect the views of the Defense Advanced Research Projects Agency
(DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution
Unlimited)
