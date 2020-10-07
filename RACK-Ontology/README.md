This folder contains information about the ontology model used for RACK-in-a-Box.

It contains the following sub-folders:

- **Graphs** - support vector graphics (SVG) files showing the ontology model
- **ImplicitModel** - the implicit models used by SADL
- **Models** - demos such as the TurnstileSystem
- **ontology** - the ontology model SADL files
- **OwlModels** - the owl translation of both the model SADL files in **ontology** and additional instance data from demos
- **databin** - contains wrappers for common build tools to generate software structure .rack output during the build process.  Prefix this directory to PATH before running the build.
- **bin** - contains import and analysis tools to consume the .rack output from **databin** or similar tools and ingest that to create data instances according to the ontology model.
