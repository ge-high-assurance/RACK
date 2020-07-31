"""Tests for RACK script features

Run with pytest

For now, assumes that you've got Rack-in-a-Box v0.1 up and running like so:

    docker run --rm --detach -p 80:80 -p 3030:3030 -p 12050-12092:12050-12092 interran/rack-box:0.1

It will mutate the data on that instance.

Also, it expects to be run inside the RACK repo.
"""
from pathlib import Path

from rack import DEFAULT_BASE_URL, ingest_data_driver, ingest_owl_driver, run_query, sparql_connection, Url


def test_load_csv() -> None:
    # Just test that it doesn't raise an exception
    ingest_data_driver(Path("../models/TurnstileSystem/Data/import.yaml"), Url("http://localhost"), None, None)

def test_load_owl() -> None:
    # Just test that it doesn't raise an exception
    ingest_owl_driver(Path("../OwlModels/import.yaml"), Url("http://localhost"), None)

def test_run_query() -> None:
    conn = sparql_connection(DEFAULT_BASE_URL, Url("http://rack001/data"), None)
    # TODO(lb): This is outdated, and will need to be changed when
    # the next version of Rack-in-a-Box is released.
    run_query(conn, "TA1 ingest1 system")
