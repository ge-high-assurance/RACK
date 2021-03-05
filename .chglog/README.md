# How to generate an initial CHANGELOG

Download git-chglog from [https://github.com/git-chglog/git-chglog].

Put git-chglog in PATH.

Run `git-chglog --init` and accept all defaults.

Run `git-chglog --next-tag v5.0 -o .chglog/CHANGELOG.md` (note that
you can edit `.chglog/CHANGELOG.tpl.md` to change output).

Run `git diff .chglog/CHANGELOG.md` to find any new text.

Find out new text isn't that useful.  Individual git commit messages
are too fine-grained and merge commits don't include any text from the
pull request descriptions.

End up creating `CHANGELOG.md` in root directory using only version
numbers and merge commit urls from `.chglog/CHANGELOG.md` and looking
at pull requests' descriptions and diffs to write actual changelog
messages.
