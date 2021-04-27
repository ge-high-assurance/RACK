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

import semtk

from ontology_changes.ontology_change import OntologyChange

@dataclass
class FreeformNotes(OntologyChange):
    """
    Allows us to put arbitrary text in the output of the tool, for generating
    changelogs.
    """

    text: str

    def text_description(self) -> str:
        return self.text

    def migrate_json(self, json: semtk.SemTKJSON) -> None:
        # Nothing to do
        pass
