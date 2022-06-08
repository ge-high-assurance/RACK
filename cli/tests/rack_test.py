# Copyright (c) 2020, General Electric Company and Galois, Inc.
"""Tests for RACK script features

Run with pytest.

Expects to be run inside the RACK repo.
"""
from pathlib import Path

from rack import DEFAULT_BASE_URL, ingest_data_driver, ingest_owl_driver, run_query, sparql_connection, Url


def test_load_csv(rack_in_a_box: str) -> None:
    # Just test that it doesn't raise an exception
    ingest_data_driver(Path("../Turnstile-Ontology/99-Utils/Data/Model.yaml"), Url(rack_in_a_box), None, None, None, False)

def test_load_owl(rack_in_a_box: str) -> None:
    # Just test that it doesn't raise an exception
    ingest_owl_driver(Path("../RACK-Ontology/OwlModels/import.yaml"), Url(rack_in_a_box), None, None, False)

def test_run_query(rack_in_a_box: str) -> None:
    conn = sparql_connection(Url(rack_in_a_box), Url("http://rack001/data"), [], None, None)
    run_query(conn, "Ingest-SystemComponent")
