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
from ontology_changes import Commit, RenameProperty

DOCUMENT = rack("DOCUMENT")
PROV_S = rack("PROV-S")

commit = Commit(
    number="10da69db606ebdc721fd3f8e003ef2099a5fdc43",
    changes=[
        # DOCUMENT.sadl
        # AddedClass DOCUMENT that replaces a lot of classes!
        # DOCUMENT#DESCRIPTION
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="DESCRIPTION",
            from_name="dateOfIssue",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="dateOfIssue",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="DESCRIPTION",
            from_name="status",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="status",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="DESCRIPTION",
            from_name="issuingOrganization",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="issuingOrganization",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="DESCRIPTION",
            from_name="references",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="references",
        ),
        # DOCUMENT#PLAN
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="PLAN",
            from_name="dateOfIssue",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="dateOfIssue",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="PLAN",
            from_name="status",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="status",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="PLAN",
            from_name="issuingOrganization",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="issuingOrganization",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="PLAN",
            from_name="approvalAuthority",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="approvalAuthority",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="PLAN",
            from_name="references",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="references",
        ),
        # DOCUMENT#PROCEDURE
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="PROCEDURE",
            from_name="dateOfIssue",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="dateOfIssue",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="PROCEDURE",
            from_name="status",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="status",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="PROCEDURE",
            from_name="issuingOrganization",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="issuingOrganization",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="PROCEDURE",
            from_name="approvalAuthority",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="approvalAuthority",
        ),
        # DOCUMENT#REPORT
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="REPORT",
            from_name="dateOfIssue",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="dateOfIssue",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="REPORT",
            from_name="status",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="status",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="REPORT",
            from_name="issuingOrganization",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="issuingOrganization",
        ),
        # DOCUMENT#REQUEST
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="REQUEST",
            from_name="dateOfInitiation",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="dateOfIssue",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="REQUEST",
            from_name="status",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="status",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="REQUEST",
            from_name="originatorOfRequest",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="issuingOrganization",
        ),
        # DOCUMENT#SPECIFICATION
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="SPECIFICATION",
            from_name="dateOfIssue",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="dateOfIssue",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="SPECIFICATION",
            from_name="status",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="status",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="SPECIFICATION",
            from_name="issuingOrganization",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="issuingOrganization",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="SPECIFICATION",
            from_name="approvalAuthority",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="approvalAuthority",
        ),
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="SPECIFICATION",
            from_name="references",
            to_name_space=DOCUMENT,
            to_class="DOCUMENT",
            to_name="references",
        ),
        # DOCUMENT#SECTION
        RenameProperty(
            from_name_space=DOCUMENT,
            from_class="SECTION",
            from_name="title",
            to_name_space=PROV_S,
            to_class="THING",
            to_name="title",
        ),
    ],
)
