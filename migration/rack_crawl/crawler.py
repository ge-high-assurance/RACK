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

from custom_formatter import stream_handler
import git_helpers as git


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
            sys.stdout.write(
                f"# git diff {commit_id}^ {commit_id} -- ../RACK-Ontology/ontology\n"
            )
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
            sys.stdout.write(f"    commit{commit_id}.commit, # FIXME: ORGANIZE ME\n")


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
    return any([is_ontology_file(file) for file in git.get_changed_files(commit_id)])


def crawl_repository(
    configuration: CrawlerConfiguration,
) -> None:

    if not (git.is_ancestor(configuration.old_commit_ref, configuration.new_commit_ref)):
        git.abort("old_commit_ref is NOT and ancestor of new_commit_ref, please fix.")

    def on_commit(commit_id: str) -> None:
        if commit_touches_ontology(commit_id) and not os.path.isfile(
            f"rack/commits/commit{commit_id}.py"
        ):
            logger.info(f"{commit_id} touches the ontology and has no associated file!")
            instantiate_template(commit_id)

    git.traverse_commits(
        new_commit=git.get_commit_id(configuration.new_commit_ref),
        old_commit=git.get_commit_id(configuration.old_commit_ref),
        on_commit=on_commit,
    )
