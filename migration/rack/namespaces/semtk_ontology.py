# Copyright (c) 2023, Galois, Inc.
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

def semtk(name_space: str) -> NameSpace:
    return NameSpace(uri_prefix="http://research.ge.com/semtk", name_space=name_space)

ENTITY_RESOLUTION = semtk("EntityResolution")
