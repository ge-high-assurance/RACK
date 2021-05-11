RACK ASSIST
===========

# Introducing RACK ASSIST

(Automated Software Structure Ingestion and System Testing)

The ASSIST toolkit contains several tools that can be used with RACK.

 * ASSIST-DC  -- Data Collection
 * ASSIST-DI  -- Data Ingestion
 * ASSIST-DV  -- Data Verification
 * ASSIST-SADL  -- SADL translation
 * ASSIST-RACKLIB -- RACK library

## ASSIST-DC  -- Data Collection

  This portion of the toolkit represents a case study in the
  integration of ARCOS data collection into the development process
  that might be utilized by a software development contractor
  providing input to ARCOS.  The tools here are designed to integrate
  with a typical development process and gather information related to
  the build as the build is being performed, including:

    - Who performed the build?
    - When was the build performed?
    - What build tools were used (compiler, linker, etc.)?
    - What output files were generated from what input files, via what activity?
    - What versions of the source files were used?

  It should be noted that the ASSIST-DC tools are a POC and represent
  one way to capture a typical build process.  The ASSIST-DC does not
  represent the only way to capture this data, and it has not been
  used with ARCOS program-provided input (which is not buildable).

  These tools are used by adding the `assist/databin` directory to the
  current `PATH` before performing development processes; no
  additional work is needed to enable ASSIST-DC data collection.  The
  information collection can be enhanced easily by adding new
  collectors to this directory.

  The intent of the ASSIST-DC set of tools is to model how a
  development process might be captured for RACK in a deployed
  program.


## ASSIST-DI  -- Data Ingestion

  The ASSIST-DC subset of tools provides a set of tools that are used
  to generate local data during the build process.  That local data
  exists only in the development workspace, and the ASSIST-DI tools
  are used to ingest that collected data and upload it into a RACK store.

  The ASSIST-DI utilizes a flexible data recognition system and logic
  programming techniques (written in Prolog) to automatically generate
  correlations and relationships in the RACK data based on the defined
  ontology and the data extracted from the artifacts being described.
  This extracted data can be in any number of forms (the canonical
  example is the ASSIST-DC-generated data), and ASSIST-DI provides the
  ability to recognize those input forms and convert them with proper
  ontology-based relationships for either direct upload to RACK or
  output as OWL files that can later be uploaded to RACK.

  One particular advantage of ASSIST-DI is that it does not need
  explicit code updates for changes in the ontology.  There are a
  lightweight set of data recognizers that can perform the conversion
  of the extracted data into specific objects or fields, but the
  relationships between these objects and the set of fields associated
  with each object is dictated by the ontology itself, which is
  automatically used by ASSIST-DI for this purpose.  This allows the
  ASSIST-DI to be flexible in the face of changes to the ontology, and
  easily extensible to support newly extracted data.

  The ASSIST-DI tools provide a parallel solution to the SemTK tooling
  when the latter is used for ingesting data from CSV files.  The
  SemTK approach requires conversion of extracted data into multiple
  CSV files which SemTK can then ingest into the RACK dataset, whereas
  ASSIST-DI operates on the extracted data itself to recognize and
  ingest that data into the RACK dataset.  The choice of which tool to
  use should be made based on the type of data extraction being
  performed, the level of manual adjustment/inspection of that data
  that is needed, and the preferences of the parties managing the
  ingestion; both are valid and parallel methods to utilize.

  This is a POC of fully automated ingestion.  The primary tool in the
  ASSIST-DI toolkit is the `bin/ingest_data` tool.


## ASSIST-DV  -- Data Verification

 The ASSIST-DV set of tools provides an automated verification process
 to be used on the data uploaded into RACK.  This toolset provides the
 ability to analyze the uploaded data relative to ontology rules and
 other semantics to determine the validity of the data.  This toolset
 does *not* provide any mechanism for correction of invalid data, only
 detection.

 There are various types of data validity checks that can be performed,
 including:

  * validity  - does the data map to valid ontology elements and have valid values

  * consistency - does the data integrate with existing data cleanly

  * semantics - is the data sensical from an interpretation perspective

 The https://github.com/ge-high-assurance/RACK/wiki/Data-Verification
 page provides a detailed list of the various verifications performed
 by ASSIST-DV (and those performed by SemTK).

 The ASSIST-DV is used as part of the RACK CI process, and is also
 designed to be used by RACK users to validate the data that they are
 ingesting into RACK.  The primary tool for ASSIST-DV is the
 `bin/check` tool, which utilizes the same core Prolog code that is
 used in ASSIST-DI; there is a companion `bin/analyze` tool that
 generates a human-readable report of the full set of data.

## ASSIST-SADL  -- SADL translation

 The ASSIST-SADL is an experimental implementation of a POC for
 offline translation of SADL into OWL.  The canonical translation
 method is via the SADL Eclispe plugin, but this method does not lend
 itself to use in automated, non-interactive, and non-GUI environments.

 ASSIST-SADL is experimental, supports only a subset of static SADL,
 and is not currently actively utilized.  It is primarily represented
 by the `bin/sadl2owl` tool.

## ASSIST-RACKLIB -- RACK library

 The ASSIST-RACKLIB is a library of (primarily Prolog) functionality
 that can be used to work with RACK RDF, Owl, and triple-store
 databases to develop related functionality.  The ASSIST-RACKLIB is
 used by ASSIST-DI and ASSIST-DV and could be used for development of
 other tools in this space.

# Usage

This toolset provides for the ability to do various automated tasks,
guided by the core RACK ontology.  These tools are designed to be run
directly from the command-line or as part of the build process of the
software being certified.

These tools additionally provide an alternate data ingestion method to
the CSV file + RACK cli tooling.  Either is appropriate to use, based
on the needs at the time.

* `databin`

   This directory contains Linux wrappers for various tools that can
   be used to emit auxiliary data into `.rack` files during the build
   process. This data can then be used to establish instance data in
   the RACK database for that built software tree.

   Add the `databin` directory to the beginning of the PATH variable
   before running a software build under Linux.

* `bin`

   This directory provides various tools to ingest the data written to
   the `.rack` files by the `databin` tools or other tools, as well as
   generate analysis reports and automated checks on the ingested
   data.

   These tools require SWI Prolog to be installed, and can be run in
   any environment (Linux, MacOS, and Windows) by adding them to the
   `PATH` variable.

   See the [bin/README.md](bin/README.md) for more details.

---
Copyright (c) 2020, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited).
