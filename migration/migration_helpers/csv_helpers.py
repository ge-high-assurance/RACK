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

# from typing import Optional

# from ontology_changes.ontology_change import Header, Line


# def get_header_index(header: Header, identifier: str) -> Optional[int]:
#     """
#     Checks whether the given identifier is one of the headers, and returns
#     its index if so.
#     """
#     try:
#         return header.index(identifier)
#     except ValueError:
#         return None


# def safe_index(line: Line, index: int) -> str:
#     if len(line) <= index:
#         return ""
#     return line[index]
