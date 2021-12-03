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

from dataclasses import dataclass
from typing import Callable


@dataclass
class NameSpace:
    uri_prefix: str
    name_space: str


def get_uri(name_space: NameSpace, item: str) -> str:
    return f"{name_space.uri_prefix}{name_space.name_space}#{item}"


def make_namespaced_renamer(
    name_space: NameSpace,
    from_name: str,
    to_name: str,
) -> Callable[[str], str]:
    def namespaced_renamer(current_value: str) -> str:
        if current_value == get_uri(name_space, from_name):
            return get_uri(name_space, to_name)
        else:
            return current_value

    return namespaced_renamer
