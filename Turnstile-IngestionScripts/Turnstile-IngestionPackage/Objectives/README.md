# Certification Objectives for Turnstile
The process we used to generate the certification objectives is slightly different from the other Turnstile instance data.
- Created nodegroups to relate ACTIVITYs from the GE-Ontology overlay to OBJECTIVEs from the [DO-178C ontology](https://github.com/ge-high-assurance/RACK/blob/TurnstileUpdate/RACK-Ontology/ontology/DO-178C/DO-178C.sadl). 
	- The purpose of these nodegroups is to create CSV files with instance data for ingestion into RACK. These nodegroups customize the header names so that they are compliant with the ingest_OBJECTIVE CDR. For example, OBJECTIVE identifiers are saved under the header "identifier"; ACTIVITY identifiers are saved under the header "satisfiedBy".
- Created a shell script (Create-SoftwareObjectives.sh) that loads these nodegroups and runs each nodegroup as a query. Exported the data and saved as CSV formatted files.
- Loaded the exported data back into RACK via Load-TurnstileData.sh, using the ingest_OBJECTIVE nodegroup. (See import.yaml.)

The following table summarizes the identified OBJECTIVEs `satisfiedBy` the ACTIVITYs. (Note: the listed ACTIVITYs are part of the GE-Ontology overlay.)

|OBJECTIVE|ACTIVITY|
|---|---|
|A-2-1, A-2-2|SoftwareRequirementsDefinition|
|A-2-4, A-2-5|SoftwareDesign|
|A-2-6|SoftwareCoding|
|A-2-7|SoftwareIntegration|
|A-3-1 to A-3-7|SoftwareRequirementsReview|
|A-4-1 to A-5-13|SoftwareDesignReview|
|A-5-1 to A-5-9|SoftwareCodeReview|
|A-6-1, A-6-2, A-6-5|SoftwareComponentTestExecution|
|A-6-3, A-6-4|SoftwareUnitTestExecution|
|A-7-1|DevelopUnitTests; DevelopComponentTests|
|A-7-2|SoftwareComponentTestExecution; SoftwareUnitTestExecution; ProblemReporting|
|A-7-3|SoftwareComponentTestExecution; SoftwareUnitTestExecution|
|A-7-5 to A-7-7|StructuralCoverageAnalysis|
|A-7-8|DataCouplingAnalysis; ControlCouplingAnalysis|
