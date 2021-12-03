import logging
import subprocess
import sys
from typing import Callable, List, NoReturn, Set

from custom_formatter import stream_handler


GIT_HELPERS_LOGGER_ID = "git_helpers"
logger = logging.getLogger(GIT_HELPERS_LOGGER_ID)
logger.addHandler(stream_handler)
logger.propagate = False


logging.basicConfig(level=logging.INFO)


def abort(reason: str) -> NoReturn:
    """
    Quit the program, indicating why.
    """
    logger.warn(f"Aborting: {reason}")
    sys.exit(1)


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


def is_ancestor(ancestor: str, descendant: str) -> bool:
    """
    Checks whether commit 'ancestor' is an ancestor of commit 'descendant'.
    """
    try:
        run_git(["merge-base", "--is-ancestor", ancestor, descendant])
        return True
    except subprocess.CalledProcessError:
        return False


def get_changed_files(commit_id: str) -> List[str]:
    """
    Returns the list of files that were modified by the given commit.
    """
    changed_files = run_git(["diff", "--name-only", commit_id, f"{commit_id}^"])
    return changed_files.splitlines()


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

        logger.debug(f"Traversing {current_commit}")

        if current_commit in visited:
            logger.debug(f"Already visited, abandoning path")
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


def get_commit_subject(commit_id: str) -> str:
    return run_git(["log", "--format=%s", "-n", "1", commit_id])


def get_merge_commit_subjects(merge_commit_id: str) -> List[str]:
    """Returns a list of "<HASH> Commit subject for that hash" lines."""
    subjects = run_git(
        ["log", "--format=%H %s", f"{merge_commit_id}~1..{merge_commit_id}"]
    ).splitlines()
    # Subjects come from most recent to oldest, turning to chronological
    return list(reversed(subjects))
