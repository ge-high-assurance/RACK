# Copyright (c) 2020, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

from typing import List

from ontology_changes.ontology_change import Commit
from rack.commits import (
    # <CHANGE_CRAWLER_IMPORTS> DO NOT EDIT OR MOVE THIS LINE
    commitae0a7660b0afdd53ff334577fbdea7749abe6cf6,
    commit2e079bb2a32b3cc1b3153d44ad0c21e27507937f,
    commit9af9030fe191d564875c067f6e0319ca6b52b798,
    commit05a03cd687e3bdce425794763e0957d3ccaf8ff0,
    commitfa603aad886439eb6a94e44e2c6f4851af16c9a3,
    commit10da69db606ebdc721fd3f8e003ef2099a5fdc43,
    commitd48e208669c589d070c7c5fb7e3129ababbb9193,
    commit90f2d4f55668786ffa01bba2a646c7468849c97d,
    commit833ef18f5024fee255f77887de2c8e9bc136e56d,
    commitd8271d216704351cf0007a04abac47f4abc993ad,
    commit13ed266ba5730cebe75c0c48f6ba83af69429122,
    commitff31a28051a5e348fd2474fce5360195999ddb3a,
    commit44393cc30bb0ba7482acd21b2e68576b577179f9,
    commit581f1820855eee2445d9e8bfdbb639e169e9391e,
    commitb721c16f0f7420a8ccd92bda0d98a96c16dc62b8,
    commitc6692fed3e150e7df53d4a2a8f8c84f760087420,
    commit4f9fce78e36a6dc75f1702ab50da6a4ac801dd5e,
    commit643839e7d8036731ba1da767942c8e74c2876e2e,
    commitbdfef3d7ea9b3c9fc085defa8e26256f646097d9,
    commit78eaae3db5ed184c90f4f14d34a4fc000f04bdac,
    commitb25d07626e4693cd370a2070e17f6baa825a1d43,
)

# WARNING: commit dates may not reflect the history order, and are not even the
# same on Github and in git, use them as vague hints of where a commit could be.
#
# You can use:
#
#   git log | grep "commit1\|commit2\|commit3..."
#
# to figure out the respective order of some commits, and use the reverse of
# that order in this list.

commits_in_chronological_order: List[Commit] = [
    # <CHANGE_CRAWLER_COMMITS> DO NOT EDIT OR MOVE THIS LINE

    # oldest (in history)

    # v4.0
    commitae0a7660b0afdd53ff334577fbdea7749abe6cf6.commit,  # 2020 Dec 15
    # v4.1
    commit2e079bb2a32b3cc1b3153d44ad0c21e27507937f.commit,  # 2021 Jan 5
    commit9af9030fe191d564875c067f6e0319ca6b52b798.commit,  # 2021 Jan 5
    commit05a03cd687e3bdce425794763e0957d3ccaf8ff0.commit,  # 2020 Dec 22
    commitfa603aad886439eb6a94e44e2c6f4851af16c9a3.commit,  # 2020 Dec 22
    commit10da69db606ebdc721fd3f8e003ef2099a5fdc43.commit,  # 2020 Dec 22
    commitd48e208669c589d070c7c5fb7e3129ababbb9193.commit,  # 2021 Jan 5
    commit90f2d4f55668786ffa01bba2a646c7468849c97d.commit,  # 2020 Dec 22
    commit833ef18f5024fee255f77887de2c8e9bc136e56d.commit,  # 2021 Jan 12
    commitd8271d216704351cf0007a04abac47f4abc993ad.commit,  # 2020 Dec 22
    commit13ed266ba5730cebe75c0c48f6ba83af69429122.commit,  # 2021 Jan 12
    commitff31a28051a5e348fd2474fce5360195999ddb3a.commit,  # 2021 Jan 12
    commit44393cc30bb0ba7482acd21b2e68576b577179f9.commit,  # 2020 Dec 22
    commit581f1820855eee2445d9e8bfdbb639e169e9391e.commit,  # 2021 Jan 15
    commitb721c16f0f7420a8ccd92bda0d98a96c16dc62b8.commit,  # 2021 Jan 15
    commitc6692fed3e150e7df53d4a2a8f8c84f760087420.commit,  # 2021 Jan 19
    commit4f9fce78e36a6dc75f1702ab50da6a4ac801dd5e.commit,  # 2021 Jan 19
    commit643839e7d8036731ba1da767942c8e74c2876e2e.commit,  # 2021 Jan 21
    commitbdfef3d7ea9b3c9fc085defa8e26256f646097d9.commit,  # 2021 Feb 4
    # v4.8
    commit78eaae3db5ed184c90f4f14d34a4fc000f04bdac.commit,  # FIXME: ORGANIZE ME
    # v5.0
    commitb25d07626e4693cd370a2070e17f6baa825a1d43.commit,  # 2021-03-15

    # most recent (in history)
]

commits_in_git_log_order: List[Commit] = list(
    reversed(commits_in_chronological_order)
)
