In a hurry? Skip this introduction and just [learn how to install and use RACK](https://github.com/ge-high-assurance/RACK/wiki/home).

# Introducing RACK

RACK (Rapid Assurance Curation Kit) is a research-grade database that uses a structured semantic data model tuned to the domain of the DARPA ARCOS (Automated Rapid Certificaiton Of Software) program.

<img src="https://github.com/ge-high-assurance/RACK/wiki/images/RACK_cartoon.jpg" alt="RACK Overview Diagram" width="300" align="middle">

As shown above, RACK takes in evidence in the form of technical documents, test cases and test results, analysis reports, and other results that aid in documenting certification of complex systems. RACK also takes as input the _provenance_ of that data, as well as its relationship to the _structure_ of the relevant system. Specifically, RACK provides a data ingestion interface primarily for use by ARCOS TA1 performers, whose primary focus in ARCOS is to provide evidence from which assurance arguments can be crafted. RACK also provides a query interface for use by TA3 performers, whose primary focus in ARCOS is the construction of compelling assurance arguments. RACK queries allow users to find evidence related to diverse parts of the target system, understand where that evidence came from, and what the evidence infers about that system. While RACK aims to fully support the needs of the ARCOS program, we emphasize that RACK is not intended to be production-ready software.

## The RACK Deployment Model

To make RACK easy for all ARCOS performers to use, we deploy RACK as a software appliance that any user can download, install, and run in a local environment. This deployment model, which we call RACK-in-a-Box, makes it easy for both TA1 and TA3 performers to test software against the RACK APIs at will, without worrying about interfering accidentally with other users. In addition, our model allows for users to concurrently collaborate at will across TAs. For example, a TA1 user can send a set of sample data to a TA3 user, so that the TA3 user can ingest that data in a private RACK instance and develop query techniques independently. RACK's deployment approach achieves this flexibility while retaining ease of deployment in a centralized cloud instance, such as we might use during an ARCOS system evaluation experiment.

## Getting Started with RACK

RACK is available either as a virtual machine image or a Docker container, and is supported on Linux, Windows, and MacOS systems. To learn more and get detailed instructions on how to get started, see [our online User Guide](https://github.com/ge-high-assurance/RACK/wiki/RACK-in-a-Box-User-Guide-v0.0).
