# RACK Change Log


## [v5.0] - 2021-02-27

### Ada

- use ScrapingToolKit as output ([#300](https://github.com/ge-high-assurance/RACK/issues/300))
- do not use "aux" filename for Windows users
- update wrt. ontology changes
- auto-generate stubs
- fix incorrect type annotation
- list files in output
- add regression for CallStmt support
- improve shell.nix
- CallStmt support
- fix input to file provider
- split apart dev requirements
- update niv
- add 'files' file to regression
- attempt at escaping special characters for URIs
- update README
- emit warnings but continue on unresolved nodes
- lint pass
- add colorama to requirements
- continue analysis upon unresolved references
- move test files into own directory
- importing future must happen first
- update format for github requirements
- copyrights and docstrings
- improve handling of filenames
- add unit method to AdaNode typings
- match ontology in naming for "definedIn"
- parenthesize name location
- support DottedName nodes
- add more typings
- implement some pylint suggestions
- switch from pipenv to pip+venv
- fix incorrect mypy annotation
- allow a file arg for listing project files
- have bash present in the shell
- documentation
- move folder out of RACK-Ontology
- simplify treatment of files vs. bodies
- connect static call graph to rdf outputter
- add .gitignore
- add missing regression.gpr
- add definition site for the caller
- factor out local visitor and call graph merge
- properly remove params everywhere
- describe regression in README
- add higher-order function to regression.adb
- remove unused params, finish pydoc
- replace comments with pydoc
- separate printing from call graph calculation
- uniform quoting style
- make rack_rdf.py executable
- more typings for rdflib
- stubs for rdflib
- rename analyze_libadalang to run_analysis
- update README
- implement unit provider
- add regression.adb and type stubs
- handle namespaces better
- actually add Python stubs and more types
- fix definition site for SubpBody declarations
- fix missing visit of declarations in SubpBody
- better display of found static calls
- add some documentation
- prototype static call graph explorer
- add type stubs and some Python typings
- add colored indentation to Ada AST printer
- fix bug where nodes were displayed twice
- remove safeguards no longer needed
- generic AST visitor and printer
- add basic python script based on Eric's
- add environment var pointing to adb files
- add pylint and uniform pythonPackages
- add env variable pointing to HTML doc
- libadalang now working
- lots of small fixes to derivations
- unify gnat, attempt to build ASIS
- working on supporting gnat_util
- cosmetic change
- remove unused in gnatcoll-core
- update README
- fix markdown
- working environment to build libadalang
- derivations now working up to gnatcoll-core
- add gnatcore-coll derivation
- remove xmlada-source-configured
- gprbuild needs a configured xmlada source

### Cli

- append ellipsis only once ([#286](https://github.com/ge-high-assurance/RACK/issues/286))
- use dev requirements from the dev/ directory
- emit a warning guessing command to correct mistake
- add wheel for nix users
- fix some pylint warnings
- set epoch in shell.nix to avoid zip errors
- add requirements-dev for dev dependencies

### WIP

- libadalang nix expressions

### Reverts

- cli: add requirements-dev for dev dependencies

### Pull Requests

- Merge pull request [#311](https://github.com/ge-high-assurance/RACK/issues/311) from ge-high-assurance/am/ts-sadl-queries
- Merge pull request [#310](https://github.com/ge-high-assurance/RACK/issues/310) from ge-high-assurance/em/sw-naming-convention
- Merge pull request [#279](https://github.com/ge-high-assurance/RACK/issues/279) from ge-high-assurance/ks/queryNodegroups
- Merge pull request [#253](https://github.com/ge-high-assurance/RACK/issues/253) from ge-high-assurance/turnstile_assist
- Merge pull request [#250](https://github.com/ge-high-assurance/RACK/issues/250) from ge-high-assurance/ontology-review/agents
- Merge pull request [#263](https://github.com/ge-high-assurance/RACK/issues/263) from ge-high-assurance/ontology-review/testing
- Merge pull request [#249](https://github.com/ge-high-assurance/RACK/issues/249) from ge-high-assurance/assist_file_updates
- Merge pull request [#251](https://github.com/ge-high-assurance/RACK/issues/251) from ge-high-assurance/ji/composite-action


## [v4.1] - 2020-12-17

### Pull Requests

- Merge pull request [#248](https://github.com/ge-high-assurance/RACK/issues/248) from ge-high-assurance/em/wasDerivedFrom
- Merge pull request [#247](https://github.com/ge-high-assurance/RACK/issues/247) from ge-high-assurance/em/str-uri
- Merge pull request [#237](https://github.com/ge-high-assurance/RACK/issues/237) from ge-high-assurance/em/str-overlay


## [v4.0] - 2020-12-07

### Assist

- update documentation
- update documentation

### Pull Requests

- Merge pull request [#242](https://github.com/ge-high-assurance/RACK/issues/242) from ge-high-assurance/glguy-patch-1


## [v3.9] - 2020-12-03

### Assist

- remove generated Prolog files, update doc

### Ci

- Test RACK with pytest
- Don't allow failure in ontology linting step
- Package RACK CLI and requirements as wheels
- Always build RACK-box image, only publish from master branch

### Databin

- fix bash loop over empty strings
- gcc wrapper support other extensions

### Pull Requests

- Merge pull request [#241](https://github.com/ge-high-assurance/RACK/issues/241) from ge-high-assurance/em/restore-components
- Merge pull request [#239](https://github.com/ge-high-assurance/RACK/issues/239) from ge-high-assurance/ji/combine-workflows
- Merge pull request [#236](https://github.com/ge-high-assurance/RACK/issues/236) from ge-high-assurance/em/when-then
- Merge pull request [#235](https://github.com/ge-high-assurance/RACK/issues/235) from ge-high-assurance/am-assuranceUpdate
- Merge pull request [#234](https://github.com/ge-high-assurance/RACK/issues/234) from ge-high-assurance/em/provenance-example
- Merge pull request [#232](https://github.com/ge-high-assurance/RACK/issues/232) from ge-high-assurance/vr/gcc-wrapper
- Merge pull request [#233](https://github.com/ge-high-assurance/RACK/issues/233) from ge-high-assurance/em/integrate-grammatech
- Merge pull request [#227](https://github.com/ge-high-assurance/RACK/issues/227) from ge-high-assurance/TurnstileUpdates
- Merge pull request [#230](https://github.com/ge-high-assurance/RACK/issues/230) from ge-high-assurance/AM-HotFix
- Merge pull request [#228](https://github.com/ge-high-assurance/RACK/issues/228) from ge-high-assurance/em/gramma-tech-failure-reason
- Merge pull request [#225](https://github.com/ge-high-assurance/RACK/issues/225) from ge-high-assurance/am-assurance
- Merge pull request [#222](https://github.com/ge-high-assurance/RACK/issues/222) from ge-high-assurance/AmbigousPropertiesHotFix
- Merge pull request [#217](https://github.com/ge-high-assurance/RACK/issues/217) from ge-high-assurance/Issue[#157](https://github.com/ge-high-assurance/RACK/issues/157)-TurnstileUpdates
- Merge pull request [#218](https://github.com/ge-high-assurance/RACK/issues/218) from ge-high-assurance/ji/tweak-ci
- Merge pull request [#207](https://github.com/ge-high-assurance/RACK/issues/207) from ge-high-assurance/em/identified-enums
- Merge pull request [#211](https://github.com/ge-high-assurance/RACK/issues/211) from ge-high-assurance/MasterCleanUp
- Merge pull request [#210](https://github.com/ge-high-assurance/RACK/issues/210) from ge-high-assurance/em/directory-cleanup
- Merge pull request [#208](https://github.com/ge-high-assurance/RACK/issues/208) from ge-high-assurance/em/grammatech
- Merge pull request [#203](https://github.com/ge-high-assurance/RACK/issues/203) from ge-high-assurance/am/queryStr
- Merge pull request [#199](https://github.com/ge-high-assurance/RACK/issues/199) from ge-high-assurance/lb/always-build-rack-box


## [v3.0] - 2020-10-23

### Ci

- Lint every branch, not just master
- Remove duplicated Packer build configuration
- Use the Prolog "check" tool to lint the ontology

### Pull Requests

- Merge pull request [#184](https://github.com/ge-high-assurance/RACK/issues/184) from ge-high-assurance/add_assist_to_dist
- Merge pull request [#186](https://github.com/ge-high-assurance/RACK/issues/186) from ge-high-assurance/fix_copyright_notices


## [v2.9] - 2020-10-20

### Ci

- Minimize the source tarball for the Packer image
- Use `type -P`, not `command -v`
- Require manual download of Packer for use with `act`
- Document running Github Actions workflows locally with `act`

### Packer

- Update documentation to reflect the new build process
- reuse setup-rack.sh

### Prolog

- add graphviz-based visualization script
- add missing utils/float_equality
- add BDU checks
- fix pldoc problem
- use a prolog:message in write_ontology
- add generated ontology/confidence.pl
- document transitive_closure/3
- add utils to documentation
- carve out and generalize zip_by_key
- print out written files in write_ontology
- auto-generate ontology classes/predicates
- fix pldoc comment
- reorganize codebase, more abstract paths
- improve help message
- use module rack_model in common_opts
- add paths.pl and a hazard query

### Pull Requests

- Merge pull request [#180](https://github.com/ge-high-assurance/RACK/issues/180) from ge-high-assurance/ji/add-rack-box
- Merge pull request [#175](https://github.com/ge-high-assurance/RACK/issues/175) from ge-high-assurance/em/copyright
- Merge pull request [#178](https://github.com/ge-high-assurance/RACK/issues/178) from ge-high-assurance/updateCSV
- Merge pull request [#174](https://github.com/ge-high-assurance/RACK/issues/174) from ge-high-assurance/am/uniqueIdentifier
- Merge pull request [#173](https://github.com/ge-high-assurance/RACK/issues/173) from ge-high-assurance/sadl2owl
- Merge pull request [#130](https://github.com/ge-high-assurance/RACK/issues/130) from ge-high-assurance/file_creation_activity
- Merge pull request [#154](https://github.com/ge-high-assurance/RACK/issues/154) from ge-high-assurance/dataproc
- Merge pull request [#156](https://github.com/ge-high-assurance/RACK/issues/156) from ge-high-assurance/em/clear-nodegroups-on-setup
- Merge pull request [#155](https://github.com/ge-high-assurance/RACK/issues/155) from ge-high-assurance/load-order
- Merge pull request [#151](https://github.com/ge-high-assurance/RACK/issues/151) from ge-high-assurance/em/bdu-notes


## [v2.0] - 2020-08-27


## [v1.9] - 2020-08-25

### Cli

- don't require a command
- Update semtk-python3 in requirements.txt

### Ontology

- add confidence meta-model

### Pull Requests

- Merge pull request [#120](https://github.com/ge-high-assurance/RACK/issues/120) from ge-high-assurance/vr/confidence
- Merge pull request [#139](https://github.com/ge-high-assurance/RACK/issues/139) from ge-high-assurance/Issue[#109](https://github.com/ge-high-assurance/RACK/issues/109)
- Merge pull request [#134](https://github.com/ge-high-assurance/RACK/issues/134) from ge-high-assurance/assessment
- Merge pull request [#133](https://github.com/ge-high-assurance/RACK/issues/133) from ge-high-assurance/lb/software-queries
- Merge pull request [#137](https://github.com/ge-high-assurance/RACK/issues/137) from ge-high-assurance/hazard_identified
- Merge pull request [#136](https://github.com/ge-high-assurance/RACK/issues/136) from ge-high-assurance/class_def
- Merge pull request [#135](https://github.com/ge-high-assurance/RACK/issues/135) from ge-high-assurance/turnstile_make_targets
- Merge pull request [#126](https://github.com/ge-high-assurance/RACK/issues/126) from ge-high-assurance/em/ingest-component
- Merge pull request [#123](https://github.com/ge-high-assurance/RACK/issues/123) from ge-high-assurance/em/cli-script
- Merge pull request [#122](https://github.com/ge-high-assurance/RACK/issues/122) from ge-high-assurance/em/compile-subtyping
- Merge pull request [#118](https://github.com/ge-high-assurance/RACK/issues/118) from ge-high-assurance/am/reqstr
- Merge pull request [#93](https://github.com/ge-high-assurance/RACK/issues/93) from ge-high-assurance/vr/delete-nodegroups
- Merge pull request [#92](https://github.com/ge-high-assurance/RACK/issues/92) from ge-high-assurance/vr/list-nodegroups
- Merge pull request [#110](https://github.com/ge-high-assurance/RACK/issues/110) from ge-high-assurance/em/flat-file
- Merge pull request [#83](https://github.com/ge-high-assurance/RACK/issues/83) from ge-high-assurance/em/qualified-usage
- Merge pull request [#99](https://github.com/ge-high-assurance/RACK/issues/99) from ge-high-assurance/em/metadata


## [v1.0] - 2020-08-01


## [v0.9] - 2020-07-29

### TurnstileSystem

- Fix typo (LANGAUGE -> LANGUAGE)
- Revert changes to INTERFACE.csv
- Update CSV files
- Executable uses createBy, not wasGeneratedBy
- Add CSV files
- Add software structure classes to GenerateCSV
- Add CounterApplicationUnitTesting
- Add software sources

### TurnstileSystem

- Add uniqueIdentifier to every class
- Add structure of counter application to SADL

### Pull Requests

- Merge pull request [#91](https://github.com/ge-high-assurance/RACK/issues/91) from ge-high-assurance/vr/fix-type-of-with-status
- Merge pull request [#88](https://github.com/ge-high-assurance/RACK/issues/88) from ge-high-assurance/vr/with-status-decorator
- Merge pull request [#87](https://github.com/ge-high-assurance/RACK/issues/87) from ge-high-assurance/vr/nicer-output
- Merge pull request [#86](https://github.com/ge-high-assurance/RACK/issues/86) from ge-high-assurance/vr/colorful-output
- Merge pull request [#85](https://github.com/ge-high-assurance/RACK/issues/85) from ge-high-assurance/vr/trim-slash-in-base-url
- Merge pull request [#82](https://github.com/ge-high-assurance/RACK/issues/82) from ge-high-assurance/branchForIssue47
- Merge pull request [#80](https://github.com/ge-high-assurance/RACK/issues/80) from ge-high-assurance/lb/control-flow
- Merge pull request [#77](https://github.com/ge-high-assurance/RACK/issues/77) from ge-high-assurance/lb/script-improvements
- Merge pull request [#65](https://github.com/ge-high-assurance/RACK/issues/65) from ge-high-assurance/em/temporal
- Merge pull request [#51](https://github.com/ge-high-assurance/RACK/issues/51) from ge-high-assurance/em/agents
- Merge pull request [#60](https://github.com/ge-high-assurance/RACK/issues/60) from ge-high-assurance/lb/functions
- Merge pull request [#41](https://github.com/ge-high-assurance/RACK/issues/41) from ge-high-assurance/em/software-ingestion-templates
- Merge pull request [#30](https://github.com/ge-high-assurance/RACK/issues/30) from langston-barrett/lb/turnstile
- Merge pull request [#36](https://github.com/ge-high-assurance/RACK/issues/36) from ge-high-assurance/em/software-ingestion
- Merge pull request [#50](https://github.com/ge-high-assurance/RACK/issues/50) from ge-high-assurance/em/remove-properties-sadl


## [v0.1] - 2020-07-02


## v0.0 - 2020-06-29

### TurnstileSystem

- Remove Sample2.sadl
- Fix typo (TurnStiles -> Turnstiles)

### Pull Requests

- Merge pull request [#33](https://github.com/ge-high-assurance/RACK/issues/33) from ge-high-assurance/em/project-file
- Merge pull request [#26](https://github.com/ge-high-assurance/RACK/issues/26) from ge-high-assurance/em/uniqueIdentifier
- Merge pull request [#15](https://github.com/ge-high-assurance/RACK/issues/15) from ge-high-assurance/AssuranceOntologyModel
- Merge pull request [#2](https://github.com/ge-high-assurance/RACK/issues/2) from ge-high-assurance/lb/merge


[Unreleased]: https://github.com/ge-high-assurance/RACK/compare/v5.0...HEAD
[v5.0]: https://github.com/ge-high-assurance/RACK/compare/v4.1...v5.0
[v4.1]: https://github.com/ge-high-assurance/RACK/compare/v4.0...v4.1
[v4.0]: https://github.com/ge-high-assurance/RACK/compare/v3.9...v4.0
[v3.9]: https://github.com/ge-high-assurance/RACK/compare/v3.0...v3.9
[v3.0]: https://github.com/ge-high-assurance/RACK/compare/v2.9...v3.0
[v2.9]: https://github.com/ge-high-assurance/RACK/compare/v2.0...v2.9
[v2.0]: https://github.com/ge-high-assurance/RACK/compare/v1.9...v2.0
[v1.9]: https://github.com/ge-high-assurance/RACK/compare/v1.0...v1.9
[v1.0]: https://github.com/ge-high-assurance/RACK/compare/v0.9...v1.0
[v0.9]: https://github.com/ge-high-assurance/RACK/compare/v0.1...v0.9
[v0.1]: https://github.com/ge-high-assurance/RACK/compare/v0.0...v0.1
