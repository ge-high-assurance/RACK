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
    commit05a03cd687e3bdce425794763e0957d3ccaf8ff0,
    commit13ed266ba5730cebe75c0c48f6ba83af69429122,
    commit182d0483a73cdc221c692c71e0d4e64f92f5079a,
    commit1c25a1b83e76eb9ca94b86402bcd335f79f80528,
    commit404e3e78ff5f554d8edbc4238f64bd3797d8829f,
    commit404e3e78ff5f554d8edbc4238f64bd3797d8829f,
    commit49db2186193711fa5d2609af0c6c30f56ea6ebbd,
    commit643839e7d8036731ba1da767942c8e74c2876e2e,
    commit6d141fa6699de1aa48b8ccd55f6942f791872ff0,
    commit7d88c0285a32107a18e439242b9ba326d6a02210,
    commit833ef18f5024fee255f77887de2c8e9bc136e56d,
    commit90f2d4f55668786ffa01bba2a646c7468849c97d,
    commitd69285ee059a6c2c2b3ec793aa4abe71bce79189,
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

commits_in_git_log_order: List[Commit] = [
    # most recent (in history)
    commit6d141fa6699de1aa48b8ccd55f6942f791872ff0.commit,  # 2021-02-25
    commit49db2186193711fa5d2609af0c6c30f56ea6ebbd.commit,  # 2021-02-03
    commit643839e7d8036731ba1da767942c8e74c2876e2e.commit,  # 2021-01-21
    commit13ed266ba5730cebe75c0c48f6ba83af69429122.commit,  # 2021-01-12
    commit833ef18f5024fee255f77887de2c8e9bc136e56d.commit,  # 2021-01-12
    commit90f2d4f55668786ffa01bba2a646c7468849c97d.commit,  # 2020-12-22
    commit404e3e78ff5f554d8edbc4238f64bd3797d8829f.commit,  # 2021-01-05
    commit05a03cd687e3bdce425794763e0957d3ccaf8ff0.commit,  # 2020-12-22
    commit7d88c0285a32107a18e439242b9ba326d6a02210.commit,  # 2021-12-22
    commit1c25a1b83e76eb9ca94b86402bcd335f79f80528.commit,  # 2020-12-22
    commit182d0483a73cdc221c692c71e0d4e64f92f5079a.commit,  # 2020-12-22
    commitd69285ee059a6c2c2b3ec793aa4abe71bce79189.commit,  # 2020-12-21
    # oldest (in history)
]

commits_in_chronological_order: List[Commit] = list(
    reversed(commits_in_git_log_order)
)
