In a hurry? Skip this introduction and just [learn how to install and use RACK](https://github.com/ge-high-assurance/RACK/wiki).

## Latest **Log4j security information** 

can be found [here](https://github.com/ge-high-assurance/RACK/wiki#log4j-security-update)

# Introducing RACK

**R**apid **A**ssurance **C**uration **K**it
is a semantic triplestore backed by an ontology. The ontology (or what we also call the data model) is tailored for curating evidence from certification artifacts of software systems. Evidence to show that a software package is fit-for-purpose can come from multiple subsystem providers, each generating data using different tools, in different formats, captured in different levels of granularity. As a curation platform, RACK uses its data model to normalize and organize the data. It also verifies that the ingested data is compliant with constraints specified by the data model, such as data types and cardinalities. RACK also takes as input the provenance of the data, as well as its relationship to the structure of the relevant system. Specifically, RACK provides a data ingestion interface for use by data providers whose primary focus is to generate evidence from which assurance arguments can be crafted. RACK also provides a query interface for use by data consumers whose primary focus is the construction of compelling assurance arguments. RACK queries allow users to find evidence related to diverse parts of the target system, understand where that evidence came from, and what the evidence infers about that system. 

<img src="https://github.com/ge-high-assurance/RACK/wiki/images/RACK_cartoon.jpg" alt="RACK Overview Diagram" width="300" align="middle">

RACK is a vendor agnostic data curation system. Requirements can be maintained in a third-party tool such as DOORS; source code in Git; models in Cameo Teamwork Cloud; and build processes in Jenkins. RACK pulls evidential data from all these sources into a knowledge graph from which it can be queried and the returned results can be used to show whether a system meets its goals of safe and secure operation.

## Publications

If you are citing RACK project, please use the following BibTex entries:
```latex
@inproceedings{moitra2023rack,
  title={RACK: A Semantic Model and Triplestore for Curation of Assurance Case Evidence},
  author={Moitra, Abha and Cuddihy, Paul and Siu, Kit and Archer, David and Mertens, Eric and Russell, Daniel and Quick, Kevin and Robert, Valentin and Meng, Baoluo},
  booktitle={International Conference on Computer Safety, Reliability, and Security},
  pages={149--160},
  year={2023},
  organization={Springer}
}

@inproceedings{cuddihy2023rack,
  title={Aviation Certification Powered by the Semantic Web Stack},
  author={Cuddihy, Paul and Moitra, Abha and Mertens, Eric and Siu, Kit and Archer, David and Williams, Jenny},
  booktitle={22nd International Semantic Web Conference},
  year={2023}
}

@inproceedings{moitra2022semantic,
  title={A Semantic Reference Model for Capturing System Development and Evaluation},
  author={Moitra, Abha and Cuddihy, Paul and Siu, Kit and Meng, Baoluo and Interrante, John and Archer, David and Mertens, Eric and Quick, Kevin and Robert, Valentin and Russell, Daniel},
  booktitle={2022 IEEE 16th International Conference on Semantic Computing (ICSC)},
  pages={173--174},
  year={2022},
  organization={IEEE}
}
```

## The RACK Deployment Model

To make RACK easy for all ARCOS performers to use, we deploy RACK as a software appliance that any user can download, install, and run in a local environment. This deployment model, which we call RACK-in-a-Box, makes it easy for both data providers and data consumers to test software against the RACK APIs at will, without worrying about interfering accidentally with other users. In addition, our model allows for users to concurrently collaborate at will. For example, a data provider can send a set of sample data to a data consumer to ingest that data in a private RACK instance and develop queries independently. RACK's deployment approach achieves this flexibility while retaining ease of deployment in a centralized cloud instance, such as we might use when applying RACK on a product development workflow.

## Getting Started with RACK

RACK is available as both a Linux container and a virtual machine, and is supported on Linux, Windows, and MacOS systems. To learn more and get detailed instructions on how to get started, see our [Installation Instructions](https://github.com/ge-high-assurance/RACK/wiki/Home#installation-instructions).

RACK is also available on the [ARCOS Tool Portal](https://arcos-tools.org/).

---
Copyright (c) 2021-2024 General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
