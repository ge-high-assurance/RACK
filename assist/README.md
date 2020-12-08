RACK ASSIST
===========

(Automated Software Structure Ingestion and System Testing)

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
