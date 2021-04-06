# Copyright (c) 2021, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

# NOTE (val): I tried using the GitPython library, and it was **extremely** slow!

from dataclasses import dataclass
import fileinput
import logging
import os
import subprocess
import sys
from typing import Callable, List, NoReturn, Set

from custom_formatter import stream_handler

CRAWLER_LOGGER_ID = "rack_crawl"
logger = logging.getLogger(CRAWLER_LOGGER_ID)
logger.addHandler(stream_handler)
logger.propagate = False

logging.basicConfig(level=logging.INFO)

@dataclass
class CrawlerConfiguration:
    abort_or_done_commit_ref: str
    old_commit_ref: str
    repository_path: str
    new_commit_ref: str


def abort(reason: str) -> NoReturn:
    """
    Quit the program, indicating why.
    """
    logger.warn(f"Aborting: {reason}")
    sys.exit(1)


def run_git(commands: List[str]) -> str:
    """
    Runs git with the given git commands.  Does **not** catch the errors:
    callers should handle as they see fit.
    """
    res = (
        subprocess.run(
            ["git"] + commands,
            capture_output=True,
            check=True,
            # stdout=subprocess.DEVNULL,
            text=True,
        ).stdout.strip()
        # .stdout.decode(sys.stdout.encoding)
        # .strip()
    )
    return res


def is_ancestor(ancestor: str, descendant: str) -> bool:
    """
    Checks whether commit 'ancestor' is an ancestor of commit 'descendant'.
    """
    try:
        run_git(["merge-base", "--is-ancestor", ancestor, descendant])
        return True
    except subprocess.CalledProcessError:
        return False


def get_parent_commit_ids(commit_ref: str) -> List[str]:
    """
    Returns the list of all parent commits of a given commit.  Exactly one for
    usual commits, but can be any number for n-way merge commits.
    """
    try:
        res = run_git(["log", "--pretty=%P", "-n", "1", commit_ref]).split()
        return res
    except subprocess.CalledProcessError as e:
        abort(f"{get_parent_commit_ids.__name__} failed with error: {e}")


def get_commit_id(
    commit_ref: str,
) -> str:
    """
    Returns the commit ID corresponding to a commit reference (e.g. "master" ->
    "f0e01dba...").
    """
    try:
        return run_git(["rev-parse", commit_ref])
    except subprocess.CalledProcessError as e:
        abort(f"{get_commit_id} failed with error: {e}")


def get_changed_files(commit_id: str) -> List[str]:
    """
    Returns the list of files that were modified by the given commit.
    """
    changed_files = run_git(["diff", "--name-only", commit_id, f"{commit_id}^"])
    return changed_files.splitlines()


def instantiate_template(commit_id: str) -> None:
    """
    Copies the template file to a file with the name of the commit, and replaces
    some strings in that file with the appropriate values.
    """

    commits_directory = "rack/commits"
    template = f"{commits_directory}/template.py"
    file = f"{commits_directory}/commit{commit_id}.py"
    logger.info(f"Creating file {file}")
    subprocess.check_call(["cp", template, file])

    # calling sed here is slightly different based on what OS you're on... so
    # let's just replace in Python
    for line in fileinput.input(file, inplace=True):
        if "<COMMIT_ID>" in line:
            sys.stdout.write(f'    number="{commit_id}",\n')
        elif "<COMMAND>" in line:
            sys.stdout.write(f'# git diff {commit_id}^ {commit_id} -- ../RACK-Ontology/ontology\n')
        elif "pylint: disable=unreachable" in line:
            # the warning is disabled for the template, but should not be in the
            # real file
            pass
        else:
            sys.stdout.write(line)

    add_commit_to_list_of_all_commits(
        f"{commits_directory}/__init__.py",
        commit_id,
    )


def add_commit_to_list_of_all_commits(commits_file: str, commit_id: str) -> None:
    """
    Adds the given commit to the list of all commits found in the
    'commits/__init__.py'.  We have a special marker in the file for where
    commits can be added, however, they need to be placed in the right order,
    and that's left to the user at the moment.
    """
    for line in fileinput.input(commits_file, inplace=True):
        sys.stdout.write(line)
        if "<CHANGE_CRAWLER_IMPORTS>" in line:
            sys.stdout.write(f"    commit{commit_id},\n")
        if "<CHANGE_CRAWLER_COMMITS>" in line:
            sys.stdout.write(f"    commit{commit_id}.commit,  # FIXME: ORGANIZE ME\n")


def is_ontology_file(file: str) -> bool:
    """
    Checks whether the given filename is one of the RACK ontology files.
    """
    return (
        file.startswith("RACK-Ontology/ontology/")
        and file.endswith(".sadl")
        # don't care about this one
        and file != "RACK-Ontology/ontology/GeneratePropInfoCSV.sadl"
    )


def commit_touches_ontology(commit_id: str) -> bool:
    """
    Returns 'True' if the given commit seems to touch one of the files we care
    about in the ontology.
    """
    return any([is_ontology_file(file) for file in get_changed_files(commit_id)])


def traverse_commits(
    new_commit: str,
    old_commit: str,
    on_commit: Callable[[str], None],
) -> None:

    """
    Traverses the commits of the current repository in reverse chronological
    order, starting from 'new_commit' and then its first parent, until we either
    reach 'old_commit', or end up on a path that did not originate from
    'old_commit'.

    Even though we do check initially that 'old_commit' is an ancestor of
    'new_commit', we don't check that it is an ancestor on the "main" pathway,
    and so we may miss it if the 'old_commit' came from a merged branch.

    This is still useful in practice, so we run the tool up to the diverging
    point, and warn the users that we stopped short of the wanted destination.
    """

    visited: Set[str] = set()

    def traverse(current_commit: str) -> None:

        if current_commit in visited:
            return

        visited.add(current_commit)
        on_commit(current_commit)

        # we must check **only** the first parent, as the merge commit should
        # contain the diff of the other, merged branches
        first_parent = get_parent_commit_ids(current_commit)[0]

        # we do need to check that we are still on our way to encountering the
        # 'old_commit' so as to avoid going all the way to the original commit
        # when the provided 'old_commit' is not on the main path.
        if first_parent == old_commit:
            logger.info(f"Succesfully reached target commit {old_commit}, done.")
            return
        elif is_ancestor(old_commit, first_parent):
            traverse(first_parent)
        else:
            logger.warn(
                f"Abandoning path ending at {first_parent} since it does not have {old_commit} as ancestor."
            )
            return

    traverse(new_commit)


def crawl_repository(
    configuration: CrawlerConfiguration,
) -> None:

    if not (is_ancestor(configuration.old_commit_ref, configuration.new_commit_ref)):
        abort("old_commit_ref is NOT and ancestor of new_commit_ref, please fix.")

    def on_commit(commit_id: str) -> None:
        if commit_touches_ontology(commit_id) and not os.path.isfile(
            f"rack/commits/commit{commit_id}.py"
        ):
            logger.info(f"{commit_id} touches the ontology and has no associated file!")
            instantiate_template(commit_id)

    traverse_commits(
        new_commit=get_commit_id(configuration.new_commit_ref),
        old_commit=get_commit_id(configuration.old_commit_ref),
        on_commit=on_commit,
    )
