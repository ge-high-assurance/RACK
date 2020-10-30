# GrammaTech Mapping

## Top Questions

Set-valued relations can be stored by inserting multiple CSV rows.
These rows are stored additively. Multiple rows will be returned
from a "SELECT DISTINT" query containing data with multiple
set-valued relations.

The underlying ontology does try to be very loose in
the constraints placed on relations. To avoid types in results
your system doesn't expect you can make more narrowly defined
nodegroups both for ingestion and query.

## Treatment of files

- Files can be tracked using the new `FILE` type.
- Hashes on these files can be tracked using `fileHash`
- Relative paths and filesnames can be stored in `fileName`
- Volume membership can be tracked through `fileParent`

## Treatment of metadata

When tools are used to extract semantics from application artifacts
the resulting data can be attributed to a creation ACTIVITY using
the `dataInsertedBy` relation. This allows the time, users, and tools
used to generate the artifact analysis information to be linked together.

## Treatment of collections

Many types in RACK are subtypes of COLLECTION. These have a multi-valued
relation called `content` that allows a collection to contain a potentially
overlapping set of heterogeneous values.

## Relational mappings

### SPECIFICATION

- fileHash: Mapped to `fileHash`
- title: Mapped to `title`
- dataLake/src: Mapped to `fileName` and `fileParent`
- modDate: Each modification can be treated as a new file linked with `wasDerivedFrom`
- author: All ENTITYs can be linked to a creating AGENT with `wasAttributedTo` or
  a creation activity with `wasGeneratedBy`

### SECTION

- parentHash: Contained in other things via `content`

### REQUIREMENT

ACERT specific properties will be in the `AcertRequirement` subtype
of `Requirement`.

- confidence: Mapped to textConfidence
- Extra fields for field specific text confidence: ifTextConfidence,
  whenTextConfidence, givenTextConfidence
- createdBy: All ENTITYs can be linked to a creating AGENT with `wasAttributedTo` or
  a creation activity with `wasGeneratedBy`. Creation of the extracted data can
  be tracked with `dataInsertedBy`

### TEST\_DEVELOPMENT

- used: Already set-valued

### TEST

Added AcertTest subtype

- verifies: Already set-valued
- description: Mapped to `description` on THING
- scenario: Open question but currently maps to `testBDDScenarioFile`

### ANALYSIS

- used: Already set-valued

### ANALYSIS\_REPORT

- analyzes: already set valued

### COMPONENT

- All requested set-valued attributes are.

### EXECUTABLE

This indeed maps to a FILE

- sha256: Mapped to `fileHash`
- derivedFrom, builtBy: Mapped to a `COMPILE` activity linked by `createBy`
- contains: Components can be related to this file using `fileParent`

### MUTANT

Not mapped.

We are willing to map this if it's determined that TA3 would find value in
access to it.

### TEST\_EXECUTION

used: Already set-valued

### TEST\_RESULT

Added `AcertTestResult` subtype

- testCase: Mapped to `confirms`
- mutant: Not mapped
- failureReason: Mapped to `failureReason`
- cpuTime: Mapped to `cpuTime`
- memory: Mapped to `memory`
- coverage: Mapped to `coverageFile`, open question about a more structured view
- outputs: Mapped to `outputFile`, open question about a more structured view
- stateSnapshots: Mapped to `snapshotFile`, open question about a more structured view
