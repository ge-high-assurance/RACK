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

from migration_helpers.name_space import NameSpace


def rack(name_space: str) -> NameSpace:
    return NameSpace(uri_prefix="http://arcos.rack/", name_space=name_space)


AGENTS = rack("AGENTS")
ANALYSIS = rack("ANALYSIS")
BASELINE = rack("BASELINE")
CONFIDENCE = rack("CONFIDENCE")
DOCUMENT = rack("DOCUMENT")
FILE = rack("FILE")
HAZARD = rack("HAZARD")
MODEL = rack("MODEL")
PROCESS = rack("PROCESS")
PROV_S = rack("PROV-S")
REQUIREMENTS = rack("REQUIREMENTS")
REVIEW = rack("REVIEW")
SOFTWARE = rack("SOFTWARE")
SYSTEM = rack("SYSTEM")
TESTING = rack("TESTING")
