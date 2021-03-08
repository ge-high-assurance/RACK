# How to generate a changelog for the next release

Download git-chglog from [https://github.com/git-chglog/git-chglog].

Put git-chglog in PATH.

Run `git-chglog --init` and accept all defaults (doesn't need to be
repeated after first time unless you want to overwrite
`.chglog/CHANGELOG.tpl.md` and `.chglog/config.yml` with new
versions).

Run `git-chglog --next-tag v6.0 -o .chglog/CHANGELOG.md`, edit
`.chglog/CHANGELOG.tpl.md` to tweak the output if necessary, and
repeat if needed.

Find out automatically generated text isn't that useful.  Individual
git commit messages are too fine-grained and merge commits don't
include any text from the pull requests' descriptions.

End up creating a changelog for the next release using
`.chglog/CHANGELOG.md` only as a guide to find pull requests and
writing the actual changelog manually after looking at pull requests'
descriptions and changes in GitHub web pages.
