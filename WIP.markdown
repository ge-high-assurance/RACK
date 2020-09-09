Out-of-order ingestion:

* Arbitrary order when all types are known
* Test queries:
  * verify that uniqueIdentifiers are unique
  * identify ghost nodes
* Can we ingest data if we CAN'T know the type ahead of time

Implementation:

* Add Sparql command to rack cli
