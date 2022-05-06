# How to generate a changelog for the next release

Download git-chglog from [https://github.com/git-chglog/git-chglog].

Put git-chglog in PATH.

Already done: Run `git-chglog --init` and accept all defaults (doesn't
need to be repeated unless you want to overwrite
`.chglog/CHANGELOG.tpl.md` and `.chglog/config.yml` with new
versions).

Run `git-chglog --next-tag v11.0 -o .chglog/CHANGELOG-new.md`, edit
`.chglog/CHANGELOG.tpl.md` to tweak the output if necessary, and
repeat if needed.

Find out automatically generated changelog isn't that useful.
Individual git commit messages are too fine-grained and merge commits
don't include any text from the pull requests' descriptions.

End up creating a changelog for the next release using
`.chglog/CHANGELOG-new.md` only as a supplement.  Navigate to the last
RACK release on GitHub, click on the "N commits to master since this
release" link, and look at the commits' descriptions and changes.
Write new changelog manually and insert new changelog into
`.chglog/CHANGELOG.md` and `RACK.wiki/_Welcome.md`.
