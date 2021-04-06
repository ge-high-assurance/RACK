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

from migration_helpers.name_space import rack
from ontology_changes import Commit, RenameClass

SOFTWARE = rack("SOFTWARE")

commit = Commit(
    number="78eaae3db5ed184c90f4f14d34a4fc000f04bdac",
    changes=[
        # SOFTWARE.sadl
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="SOURCE_FUNCTION",
            to_name_space=SOFTWARE,
            to_name="SourceFunction",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="BINARY_FUNCTION",
            to_name_space=SOFTWARE,
            to_name="BinaryFunction",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="SOURCE_GLOBAL_VARIABLE",
            to_name_space=SOFTWARE,
            to_name="SourceGlobalVariable",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="BINARY_GLOBAL_VARIABLE",
            to_name_space=SOFTWARE,
            to_name="BinaryGlobalVariable",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="BINARY_BASIC_BLOCK",
            to_name_space=SOFTWARE,
            to_name="BinaryBasicBlock",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="CLASS_DEFINITION",
            to_name_space=SOFTWARE,
            to_name="ClassDefinition",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="CLASS_METHOD",
            to_name_space=SOFTWARE,
            to_name="ClassMethod",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="CLASS_MEMBER_VARIABLE",
            to_name_space=SOFTWARE,
            to_name="ClassMemberVariable",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="CLASS_CONSTRUCTOR",
            to_name_space=SOFTWARE,
            to_name="ClassConstructor",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="MODULE",
            to_name_space=SOFTWARE,
            to_name="Module",
        ),
        RenameClass(
            from_name_space=SOFTWARE,
            from_name="NAMESPACE",
            to_name_space=SOFTWARE,
            to_name="Namespace",
        ),
    ],
)
