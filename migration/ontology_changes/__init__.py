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

from ontology_changes.add_class import AddClass
from ontology_changes.add_property_is_a_type_of import AddPropertyIsATypeOf
from ontology_changes.cardinality import AtMost, Cardinality, SingleValue, Unconstrained
from ontology_changes.change_cardinality import ChangeCardinality
from ontology_changes.change_class_is_a_type_of import ChangeClassIsATypeOf
from ontology_changes.change_property_is_a_type_of import ChangePropertyIsATypeOf
from ontology_changes.change_property_range import ChangePropertyRange
from ontology_changes.create_class import CreateClass
from ontology_changes.create_property import CreateProperty
from ontology_changes.delete_class import DeleteClass
from ontology_changes.delete_property import DeleteProperty
from ontology_changes.ontology_change import Commit
from ontology_changes.rename_class import RenameClass
from ontology_changes.rename_property import RenameProperty
from ontology_changes.remove_is_a_type_of import RemoveIsATypeOf
