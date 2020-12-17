# RACK Tests

This directory contains tests of the RACK-Box Docker image. These tests are driven by `pytest` and are run in CI.

To run them locally, first [install the RACK CLI](../RACK-Ontology/cli), then (with that virtual environment active) run

```bash
python3 -m pip --use-feature=2020-resolver install -r tests/requirements.txt
pytest
```
