# RACK Ontology

This ontology definition is intended to define how ARCOS RACK models data. Thus
this definition will drive our data ingest API as well as the things to which
queries can refer. Most of what is here so far has to do with the structure of
software, but we also include some assurance artifacts found in the ARCOS
kick-off presentations by TA1 and TA3 teams.

Rules we aim to follow to enforce an E-R model over the allowed syntax constructs in SADL:

- A thing is a top-level class if it can have either concrete instances or
  sub-classes, but also cannot itself be a sub-class
- A thing is a sub-class "is a type of" if it can only have concrete instances.
  A sub-class may have additional attributes beyond its parent class
- A thing is an instance of a class ("is a") if it has no sub-classes, and has
  exactly the attribute set of its class
- Instances are disjoint (singleton) sets - that is, each instance is distinct
  from others
- A thing that describes a class is a relationship instance if it is an instance
  of a defined class
- A thing that describes a class is an attribute if it is an instance of a base
  datatype
