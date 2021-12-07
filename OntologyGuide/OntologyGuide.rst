####################
Ontology Users Guide
####################

.. This is redundant in the HTML output.  Is it necessary in the PDF output?
..
.. .. contents:: Table of Contents
..     :backlinks: none
..     :depth: 2
..     :local:

******
PROV-S
******

The PROV-S ontology is a specialization of `W3C Prov
<https://www.w3.org/TR/prov-overview/>`_. It brings is a subset of the classes
and relations defined there as the larger ontology provides a lot of flexibility
in how data is to be modeled.


THING
=====

All addressable data in the ontology is a ``THING``.

This class has all the generic properties for providing human-readable
descriptions of data stored in RACK.

identifier
----------

Identifiers are intended to identify data in RACK enough to link different
pieces of data together.

SADL definition::

    identifier describes THING with values of type string.
    identifier describes THING with at most 1 value.

title
-----

The title is intended to be a short, human-readable label for data stored in
RACK. This name should be suitable for making lists. It doesn't need to be
unique. It is intended to help with human understanding and user interfaces.

SADL Definition::

    title describes THING with values of type string.
    title describes THING with at most 1 value.

.. _THING#description:

description
-----------

This property provides a place to store long-form, human-readable text.

SADL Definition::

    description describes THING with values of type string.
    description describes THING with at most 1 value.

dataInsertedBy
--------------

This relation allows us to capture the provenance of the data itself. This
relation allows us to capture attribution for the process that encoded the
project data into RACK CSV files and triples.

Consider the example of a source file imported into RACK using an automatic
ingestion tool. The process of writing the code itself would be captured with a
`wasGeneratedBy`_ relation to a `CODE_DEVELOPMENT`_ activity. The process of
ingesting that source code into a RACK CSV file would be captured by a
`dataInsertedBy`_ relation to some other `ACTIVITY`_.

SADL Definition::

    dataInsertedBy describes THING with values of type ACTIVITY.
    dataInsertedBy describes THING with at most 1 value of type ACTIVITY.

ENTITY
======

wasDerivedFrom
--------------

This relation captures an abstract concept of derivation. It will typically be
used through a specialization like `wasRevisionOf`_ or `wasImpactedBy`_. It can
be used as a generic relation when performing queries to find related entities,
but it will typically not be instantiated directly in the source data.

wasRevisionOf
-------------

This relation captures the relationship to previous versions of the same
abstract entity. These will typically be entities that are logically the same
but with different version numbers.

SADL Definition::

    wasRevisionOf describes ENTITY with values of type ENTITY.
    wasRevisionOf is a type of wasDerivedFrom.

wasImpactedBy
-------------

This relation captures the relationship between an `ENTITY`_  and some other,
supposedly distinct `ENTITY`_ that it depended upon in some way, other than
being a revision of it, which would be captured by `wasRevisionOf`_ instead.
This is rarely, if ever, used as is, and instead most classes define their
specializations of this relationship for different intents of the "dependency".

SADL Definition::

    wasImpactedBy describes ENTITY with values of type ENTITY.
    wasImpactedBy is a type of wasDerivedFrom.

wasGeneratedBy
--------------

This relation captures the provenance of an `ENTITY`_ by associating it to an
`ACTIVITY`_ that created it.  It is frequently specialized by other classes to
capture different flavors of semantics for this generative act.

SADL Definition::

    wasGeneratedBy describes ENTITY with values of type ACTIVITY.
    wasGeneratedBy describes ENTITY with at most 1 value.

wasAttributedTo
---------------

TODO

SADL Definition::

    wasAttributedTo describes ENTITY with values of type AGENT.

generatedAtTime
---------------

Captures a date and time at which the entity being represented as `ENTITY`_ was
generated.  To be clear, this is meant as the time the **"real"** entity was
generated, and **not** as the time its datum was registered as an `ENTITY`_.

SADL Definition::

    generatedAtTime describes ENTITY with values of type dateTime.
    generatedAtTime describes ENTITY with at most 1 value.

invalidatedAtTime
-----------------

TODO

SADL Definition::

    invalidatedAtTime describes ENTITY with values of type dateTime.
    invalidatedAtTime describes ENTITY with at most 1 value.

entityURL
---------

Provides a location where the entity itself can be found and downloaded.

SADL Definition::
    entityURL describes ENTITY with values of type string.

COLLECTION
==========

A `COLLECTION`_ is a generic wrapper of a set of `ENTITY`_ items into one
`ENTITY`_ item.  Unless you intend to create a collection of arbitrary `ENTITY`_
items, you should likely want to specialize `COLLECTION`_ to your needs.

Current examples of `COLLECTION`_ include `DOCUMENT`_ and `BASELINE`_.

SADL Definition::

    COLLECTION is a type of ENTITY.

content
-------

The content relation is used to enumerate the elements of a collection.

SADL Definition::

    content describes COLLECTION with values of type ENTITY.

AGENT
=====

A person, moral person, or other such concept that can bear the responsibility
for an activity being initiated.

SADL Definition::

    AGENT is a type of THING.

+-------------------+
| Known subclasses  |
+-------------------+
| `ORGANIZATION`_   |
+-------------------+
| `PERSON`_         |
+-------------------+
| `TOOL`_           |
+-------------------+

actedOnBehalfOf
---------------

TODO

SADL Definition::

    actedOnBehalfOf describes AGENT with values of type AGENT.

ACTIVITY
========

TODO

SADL Definition::

    ACTIVITY is a type of THING.

wasAssociatedWith
-----------------

TODO

SADL Definition::

    wasAssociatedWith describes ACTIVITY with values of type AGENT.

wasInformedBy
-------------

TODO

SADL Definition::

    wasInformedBy describes ACTIVITY with values of type ACTIVITY.

startedAtTime
-------------

TODO

SADL Definition::

    startedAtTime describes ACTIVITY with values of type dateTime.
    startedAtTime describes ACTIVITY with at most 1 value.

endedAtTime
-----------

TODO

SADL Definition::

    endedAtTime describes ACTIVITY with values of type dateTime.
    endedAtTime describes ACTIVITY with at most 1 value.

used
----

TODO

SADL Definition::

    used describes ACTIVITY with values of type ENTITY.

******
AGENTS
******

Agents captures people, groups, or even tools that perform actions, being the
reason why activities come into existence.

ORGANIZATION
============

TODO

SADL definition::

    ORGANIZATION is a type of AGENT.

PERSON
======

General class representing a physical person.  It can be used as is, or
specialized if you'd like to identify some key roles that categorize persons for
your domain.

SADL Definition::

    PERSON is a type of AGENT.

emailAddress
------------

Valid email addresses for this person.

SADL definition::

    emailAddress describes PERSON with values of type string.


employedBy
----------

Supposedly an `ORGANIZATION`_ that employs this person.

SADL definition::

    employedBy describes PERSON with values of type AGENT.
    employedBy describes PERSON with at most 1 value of type AGENT.
    employedBy is a type of actedOnBehalfOf.


TOOL
====

toolVersion
-----------

A freeform string to indicate the version of a tool.  Each tool should
supposedly come with its own semantics for the values of this field.

SADL definition::

    toolVersion describes TOOL with values of type string.
    toolVersion describes TOOL with at most 1 value.

********
ANALYSIS
********

ANALYSIS_OUTPUT
===============

Usually a `DOCUMENT`_ that describes the output of running some `ANALYSIS`_.

SADL Definition::

    ANALYSIS_OUTPUT is a type of ENTITY.

result
------

TODO

SADL Definition::

    result describes ANALYSIS_OUTPUT with values of type ANALYSIS_RESULT.
    result describes ANALYSIS_OUTPUT with at most 1 value.

metric
------

TODO

SADL Definition::

    metric describes ANALYSIS_OUTPUT with values of type float.

analyzes
--------

TODO

SADL Definition::

    analyzes describes ANALYSIS_OUTPUT with values of type ENTITY.
    analyzes is a type of wasImpactedBy.

producedBy
----------

TODO

SADL Definition::

    producedBy describes ANALYSIS_OUTPUT with values of type ACTIVITY.
    producedBy is a type of wasGeneratedBy.

ANALYSIS_RESULT
===============

TODO

SADL Definition::

    ANALYSIS_RESULT is a type of THING,
    must be one of {Passed, Failed, Indeterminate}.

Passed
======

TODO

SADL Definition::

    Passed is an ANALYSIS_RESULT with identifier "Passed".

Failed
======

TODO

SADL Definition::

    Failed is an ANALYSIS_RESULT with identifier "Failed".

Indeterminate
=============

TODO

SADL Definition::

    Indeterminate is an ANALYSIS_RESULT with identifier "Indeterminate".

.. _ANALYSIS:

ANALYSIS
========

`ACTIVITY`_ of running some analysis to produce an `ANALYSIS_OUTPUT`_.  You
likely want to specialize this for different classes sorts of analyses.

SADL Definition::

    ANALYSIS is a type of ACTIVITY.

performedBy
-----------

Identifies the `AGENT`_ who performed the analysis.

SADL Definition::

    performedBy describes ANALYSIS with values of type AGENT.
    performedBy is a type of wasAssociatedWith.

ANALYSIS_ANNOTATION_TYPE
========================

DEPRECATED (will soon be replaced).

SADL Definition::

    ANALYSIS_ANNOTATION_TYPE is a type of THING.

ANALYSIS_ANNOTATION
===================

TODO

SADL Definition::

    ANALYSIS_ANNOTATION is a type of ENTITY.

fromReport
----------

TODO

SADL Definition::

    fromReport describes ANALYSIS_ANNOTATION with a single value of type ANALYSIS_OUTPUT.

annotationType
--------------

TODO

SADL Definition::

    annotationType describes ANALYSIS_ANNOTATION with a single value of type ANALYSIS_ANNOTATION_TYPE.


********
BASELINE
********

.. _BASELINE:

BASELINE
========

TODO

SADL Definition::

    BASELINE is a type of COLLECTION.


**********
CONFIDENCE
**********

CONFIDENCE_ASSESSMENT
=====================

TODO

SADL Definition::

    CONFIDENCE_ASSESSMENT is a type of THING.

assesses
--------

TODO

SADL Definition::

    assesses describes CONFIDENCE_ASSESSMENT with values of type ENTITY.
    assesses describes CONFIDENCE_ASSESSMENT with at most 1 value.
    assesses is a type of wasImpactedBy.

createBy
--------

TODO

SADL Definition::

    createBy describes CONFIDENCE_ASSESSMENT with values of type ACTIVITY.
    createBy is a type of wasGeneratedBy.

BDU_CONFIDENCE_ASSESSMENT
=========================

TODO

SADL Definition::

    BDU_CONFIDENCE_ASSESSMENT is a type of CONFIDENCE_ASSESSMENT.

belief
------

TODO

SADL Definition::

    belief describes CONFIDENCE_ASSESSMENT with a single value of type float [0,1].

disbelief
---------

TODO

SADL Definition::

    disbelief describes CONFIDENCE_ASSESSMENT with a single value of type float [0,1].

uncertainty
-----------

TODO

SADL Definition::

    uncertainty describes CONFIDENCE_ASSESSMENT with a single value of type float [0,1].

ASSESSING_CONFIDENCE
====================

TODO

SADL Definition::

    ASSESSING_CONFIDENCE is a type of ACTIVITY.

performedBy
-----------

TODO

SADL Definition::

    performedBy describes ASSESSING_CONFIDENCE with values of type AGENT.
    performedBy is a type of wasAssociatedWith.

********
DOCUMENT
********

.. _DOCUMENT:

DOCUMENT
========

This is a generic notion of document, as a collection of items that make it up.
You would likely want to use a specialized version of `DOCUMENT`_ to indicate
what type of document you are intending, and give it specific properties that do
not fit this generic model.

+------------------+
| Known subclasses |
+------------------+
| `DESCRIPTION`_   |
+------------------+
| `PLAN`_          |
+------------------+
| `PROCEDURE`_     |
+------------------+
| `REPORT`_        |
+------------------+
| `REQUEST`_       |
+------------------+
| `SPECIFICATION`_ |
+------------------+

SADL Definition::

    DOCUMENT is a type of COLLECTION.

versionNumber
-------------

TODO

SADL Definition::

    versionNumber describes DOCUMENT with values of type string.
    versionNumber describes DOCUMENT with at most 1 value.

dateOfIssue
-----------

TODO

SADL Definition::

    dateOfIssue describes DOCUMENT with values of type date.
    dateOfIssue describes DOCUMENT with at most 1 value.

status
------

TODO

SADL Definition::

    status describes DOCUMENT with values of type DOC_STATUS.
    status describes DOCUMENT with at most 1 value.

issuingOrganization
-------------------

TODO

SADL Definition::

    issuingOrganization describes DOCUMENT with values of type AGENT.
    issuingOrganization describes DOCUMENT with at most 1 value.
    issuingOrganization is a type of wasAttributedTo.

approvalAuthority
-----------------

TODO

SADL Definition::

    approvalAuthority describes DOCUMENT with values of type AGENT.

references
----------

TODO

SADL Definition::

    references describes DOCUMENT with values of type ENTITY.
    references is a type of wasImpactedBy.

DESCRIPTION
===========

TODO

SADL Definition::

    DESCRIPTION is a type of DOCUMENT.

PLAN
====

TODO

SADL Definition::

    PLAN is a type of DOCUMENT.

PROCEDURE
=========

TODO

SADL Definition::

    PROCEDURE is a type of DOCUMENT.

REPORT
======

TODO

SADL Definition::

    REPORT is a type of DOCUMENT.

REQUEST
=======

TODO

SADL Definition::

    REQUEST is a type of DOCUMENT.

SPECIFICATION
=============

TODO

SADL Definition::

    SPECIFICATION is a type of DOCUMENT.

SECTION
=======

An aggregate of items that form an identifiable section of a larger document.
The items can be other `SECTION`_ items, if you want a notion of sub-sections,
or can be arbitrary document items, say an `OBJECTIVE`_ if the document is
describing a `PROCESS`_.

SADL Definition::

    SECTION is a type of `COLLECTION`_.

DOC_STATUS
==========

TODO

SADL Definition::

    DOC_STATUS is a type of THING,
    must be one of {In_Development, Released, Withdrawn}.

****
FILE
****

definedIn
=========

TODO

SADL Definition::

    definedIn describes THING with values of type FILE.

FILE
====

TODO

SADL Definition::

    FILE is a type of ENTITY.

filename
--------

TODO

SADL Definition::

    filename describes FILE with a single value of type string.

fileFormat
----------

TODO

SADL Definition::

    fileFormat describes FILE with values of type FORMAT.

satisfies
---------

TODO

SADL Definition::

    satisfies describes FILE with values of type ENTITY.
    satisfies is a type of wasImpactedBy.

createBy
--------

TODO

SADL Definition::

    createBy describes FILE with values of type FILE_CREATION.
    createBy describes FILE with at most 1 value.
    createBy is a type of wasGeneratedBy.

fileHash
--------

TODO

SADL Definition::

    fileHash describes FILE with values of type FILE_HASH.

FORMAT
======

TODO

SADL Definition::

    FORMAT is a type of THING.

FILE_CREATION
=============

TODO

SADL Definition::

    FILE_CREATION is a type of ACTIVITY.

FILE_HASH
=========

TODO

SADL Definition::

    FILE_HASH is a type of THING.

hashType
--------

TODO

SADL Definition::

    hashType describes FILE_HASH with a single value of type HASH_TYPE.

hashValue
---------

TODO

SADL Definition::

    hashValue describes FILE_HASH with a single value of type string.

HASH_TYPE
=========

TODO

SADL Definition::

    HASH_TYPE is a type of THING,
    must be one of { MD5, SHA1, SHA2_256, SHA2_512 }.


******
HAZARD
******

.. _HAZARD:

HAZARD
======

TODO

SADL Definition::

    HAZARD is a type of ENTITY.

definition
----------

TODO

SADL Definition::

    definition describes HAZARD with values of type string.
    definition describes HAZARD with at most 1 value.

H:source
--------

TODO

SADL Definition::

    H:source describes HAZARD with values of type ENTITY.
    H:source is a type of wasImpactedBy.

identified
----------

TODO

SADL Definition::

    identified describes HAZARD with values of type HAZARD_IDENTIFICATION.

effect
------

TODO

SADL Definition::

    effect describes HAZARD with values of type string.
    effect describes HAZARD with at most 1 value.

severity
--------

TODO

SADL Definition::

    severity describes HAZARD with values of type float. // [0,1].
    severity describes HAZARD with at most 1 value.

likelihood
----------

TODO

SADL Definition::

    likelihood describes HAZARD with values of type float. // [0,1].
    likelihood describes HAZARD with at most 1 value.

H:mitigates
-----------

TODO

SADL Definition::

    H:mitigates describes OP_PROCEDURE with values of type HAZARD.

HAZARD_IDENTIFICATION
=====================

TODO

SADL Definition::

    HAZARD_IDENTIFICATION is a type of ACTIVITY.

H:author
--------

TODO

SADL Definition::

    H:author describes HAZARD_IDENTIFICATION with values of type AGENT.
    H:author is a type of wasAssociatedWith.


*****
MODEL
*****

MODEL
=====

TODO

SADL Definition::


    MODEL is a type of ENTITY.

models
------

TODO

SADL Definition::

    models describes MODEL with a single value of type THING.


*******
PROCESS
*******

OBJECTIVE
=========

TODO

SADL Definition::

    TODO

satisfiedBy
-----------

TODO

SADL Definition::

    TODO


************
REQUIREMENTS
************

The REQUIREMENTS ontology captures both the definition of requirements as well
as the activity of developing the requirements. These requirements will
typically be subtyped to capture project-specific requirements structure.

REQUIREMENT
===========

==================================== =======
Property                             Convention
==================================== =======
`identifier`_                        SRS [X.Y:Example Title] v1.2
`title`_                             SRS [X.Y:Example Title]
`description <THING#description_>`_  The example should clearly demonstate the use of the properties...
==================================== =======

governs
-------

This relates requirements to the entities that they govern.

SADL Definition::

    governs describes REQUIREMENT with values of type ENTITY.
    governs is a type of wasImpactedBy.

satisfies
---------

This relates a lower-level requirement to the high-level `REQUIREMENT`_ that it
satisfies.

SADL Definition::

    satisfies describes REQUIREMENT with values of type ENTITY.
    satisfies is a type of wasImpactedBy.

mitigates
---------

This relates a requirement to the `HAZARD`_ entities that it is intended to mitigate.

SADL Definition::

    mitigates describes REQUIREMENT with values of type ENTITY.
    mitigates is a type of wasImpactedBy.


******
REVIEW
******

REVIEW_LOG
----------

TODO

SADL Definition::

    REVIEW_LOG is a type of ENTITY.

reviews
-------

TODO

SADL Definition::

    reviews describes REVIEW_LOG with values of type ENTITY.
    reviews is a type of wasImpactedBy.

reviewResult
------------

TODO

SADL Definition::

    reviewResult describes REVIEW_LOG with values of type REVIEW_STATE.

createBy
--------

TODO

SADL Definition::

    createBy describes REVIEW_LOG with values of type ACTIVITY.
    createBy is a type of wasGeneratedBy.

REVIEW_STATE
============

TODO

SADL Definition::

    REVIEW_STATE is a type of THING,
    must be one of {Passed, ReviseWithoutReview, ReviseWithReview}.

Passed
======

TODO

SADL Definition::

    Passed is a REVIEW_STATE with identifier "Passed".

RevisedWithoutReview
====================

TODO

SADL Definition::

    ReviseWithoutReview is a REVIEW_STATE with identifier "Revise Without Review".

RevisedWithReview
=================

TODO

SADL Definition::

    ReviseWithReview is a REVIEW_STATE with identifier "Revise With Review".

REVIEW
======

TODO

SADL Definition::

    REVIEW is a type of ACTIVITY.

author
------

TODO

SADL Definition::

    author describes REVIEW with values of type AGENT.
    author is a type of wasAssociatedWith.

reviewer
--------

TODO

SADL Definition::

    reviewer describes REVIEW with values of type AGENT.
    reviewer is a type of wasAssociatedWith.

reviewed
--------

TODO

SADL Definition::

    reviewed describes REVIEW with values of type ENTITY.
    reviewed is a type of used.

goverenedBy
-----------

TODO

SADL Definition::

    governedBy describes REVIEW with values of type ENTITY.
    governedBy is a type of used.

********
SOFTWARE
********

CODE_DEVELOPMENT
================

TODO

SADL Definition::

    CODE_DEVELOPMENT is a type of FILE_CREATION.


author
------

TODO

SADL Definition::

    author describes CODE_DEVELOPMENT with values of type AGENT.
    author is a type of wasAssociatedWith.


referenced
----------

TODO

SADL Definition::

    referenced describes CODE_DEVELOPMENT with values of type ENTITY.
    referenced is a type of used.


governedBy
----------

TODO

SADL Definition::

    governedBy describes CODE_DEVELOPMENT with values of type ENTITY.
    governedBy is a type of used.


BUILD
=====

TODO

SADL Definition::

    BUILD is a type of FILE_CREATION.

step
----

TODO

SADL Definition::

    step describes BUILD with values of type ACTIVITY.
    step is a type of wasInformedBy.


CODE_GEN
========

TODO

SADL Definition::

    CODE_GEN is a type of FILE_CREATION.

sw:performedBy
--------------

TODO

SADL Definition::

    sw:performedBy describes CODE_GEN with values of type AGENT.
    sw:performedBy is a type of wasAssociatedWith.


COMPILE
=======

TODO

SADL Definition::

    COMPILE is a type of FILE_CREATION.

sw:performedBy
--------------

TODO

SADL Definition::

    sw:performedBy describes COMPILE with values of type AGENT.
    sw:performedBy is a type of wasAssociatedWith.

compiledBy
----------

TODO

SADL Definition::

    compiledBy describes COMPILE with values of type TOOL.
    compiledBy is a type of wasAssociatedWith.

compileInput
------------

TODO

SADL Definition::

    compileInput describes COMPILE with values of type FILE.
    compileInput is a type of used.

PACKAGE
=======

TODO

SADL Definition::

    PACKAGE is a type of FILE_CREATION.

sw:performedBy
--------------

TODO

SADL Definition::

    sw:performedBy describes PACKAGE with values of type AGENT.
    sw:performedBy of PACKAGE must be one of {Ag:PERSON, Ag:ORGANIZATION}.
    sw:performedBy is a type of wasAssociatedWith.

packagedBy
----------

TODO

SADL Definition::

    packagedBy describes PACKAGE with values of type TOOL.
    packagedBy is a type of wasAssociatedWith.

packageInput
------------

TODO

SADL Definition::

    packageInput describes PACKAGE with values of type FILE.
    packageInput is a type of used.

COMPONENT_TYPE
==============

This is essentially an open enumeration for the range of `componentType`_.  We
provide a few common instances for some regular types of software components,
but you are free to extend this enumeration if you deal with more unusual types
of software components.

SADL Definition::

    COMPONENT_TYPE is a type of THING.

.. Do we want to list all common instances here?

SWCOMPONENT
===========

TODO

SADL Definition::

    SWCOMPONENT is a type of ENTITY.

componentType
-------------

TODO

SADL Definition::

    componentType describes SWCOMPONENT with a single value of type COMPONENT_TYPE.

valueType
---------

TODO

SADL Definition::

    valueType describes SWCOMPONENT with values of type string.

mentions
--------

TODO

SADL Definition::

    mentions describes SWCOMPONENT with values of type SWCOMPONENT.

subcomponentOf
--------------

TODO

SADL Definition::

    subcomponentOf describes SWCOMPONENT with values of type SWCOMPONENT.

instantiates
------------

TODO

SADL Definition::

    instantiates describes SWCOMPONENT with values of type ENTITY.


******
SYSTEM
******

SYSTEM
======

TODO

SADL Definition::

    SYSTEM is a type of ENTITY.

partOf
------

TODO

SADL Definition::

    partOf describes SYSTEM with values of type SYSTEM.

producedBy
----------

TODO

SADL Definition::

    producedBy describes SYSTEM with values of type ACTIVITY.
    producedBy describes SYSTEM with at most 1 value.
    producedBy is a type of wasGeneratedBy.

provides
--------

TODO

SADL Definition::

    provides describes SYSTEM with values of type FUNCTION.

requires
--------

TODO

SADL Definition::

    requires describes SYSTEM with values of type FUNCTION.

function
--------

TODO

SADL Definition::

    function describes SYSTEM with values of type FUNCTION.
    function is a type of wasImpactedBy.

INTERFACE
=========

TODO

SADL Definition::

    INTERFACE is a type of ENTITY.

commodity
---------

TODO

SADL Definition::

    commodity describes INTERFACE with values of type string.
    commodity describes INTERFACE with at most 1 value.

source
------

TODO

SADL Definition::

    source describes INTERFACE with values of type SYSTEM.
    source is a type of wasImpactedBy.

destination
-----------

TODO

SADL Definition::

    destination describes INTERFACE with values of type SYSTEM.
    destination is a type of wasImpactedBy.

identifiedBy
------------

TODO

SADL Definition::

    identifiedBy describes INTERFACE with values of type ACTIVITY.

SYSTEM_DEVELOPMENT
==================

TODO

SADL Definition::

    SYSTEM_DEVELOPMENT is a type of ACTIVITY.

developedBy
-----------

TODO

SADL Definition::

    developedBy describes SYSTEM_DEVELOPMENT with values of type AGENT.
    developedBy is a type of wasAssociatedWith.

FUNCTION
========

TODO

SADL Definition::

    FUNCTION is a type of ENTITY.

parentFunction
--------------

TODO

SADL Definition::

    parentFunction describes FUNCTION with values of type FUNCTION.
    parentFunction describes FUNCTION with at most 1 value.

OP_ENV
======

TODO

SADL Definition::

    OP_ENV is a type of THING. // should this be an ENTITY?

OP_PROCEDURE
============

TODO

SADL Definition::

    OP_PROCEDURE is a type of PROCEDURE.


*******
TESTING
*******

TEST
====

TODO

SADL Definition::

    TEST is a type of ENTITY.

verifies
--------

TODO

SADL Definition::

    verifies describes TEST with values of type ENTITY.
    verifies is a type of wasImpactedBy.

producedBy
----------

TODO

SADL Definition::

    producedBy describes TEST with values of type ACTIVITY.
    producedBy is a type of wasGeneratedBy.

TEST_STATUS
===========

TODO

SADL Definition::

    TEST_STATUS is a type of THING,
    must be one of {Passed, Failed, Indeterminate}.

TODO: list `Passed`, `Failed`, `Indeterminate`

TEST_RESULT
===========

TODO

SADL Definition::

    TEST_RESULT is a type of ENTITY.

result
------

TODO

SADL Definition::

    result describes TEST_RESULT with a single value of type TEST_STATUS.

confirms
--------

TODO

SADL Definition::

    confirms describes TEST_RESULT with values of type TEST.
    confirms is a type of wasImpactedBy.

executedBy
----------

TODO

SADL Definition::

    executedBy describes TEST_RESULT with values of type ACTIVITY.
    executedBy is a type of wasGeneratedBy.

TEST_DEVELOPMENT
================

TODO

SADL Definition::

    TEST_DEVELOPMENT is a type of ACTIVITY.

developedBy
-----------

TODO

SADL Definition::

    developedBy describes TEST_DEVELOPMENT with values of type AGENT.
    developedBy is a type of wasAssociatedWith.

TEST_EXECUTION
==============

TODO

SADL Definition::

    TEST_EXECUTION is a type of ACTIVITY.

executedOn
----------

TODO

SADL Definition::

    executedOn describes TEST_EXECUTION with values of type AGENT.
    executedOn is a type of wasAssociatedWith.
