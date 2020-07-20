"""Tests for RACK script features

Run with pytest

For now, assumes that you've got Rack-in-a-Box v0.1 up and running like so:

    docker run --rm --detach -p 80:80 -p 3030:3030 -p 12050-12092:12050-12092 interran/rack-box:0.1

It will mutate the data on that instance.

Also, it expects to be run inside the RACK repo.
"""

from rack import ingest_csv_driver, ingest_owl_driver


def test_load_csv():
    # Just test that it doesn't raise an exception
    ingest_csv_driver("../models/TurnstileSystem/Data/import.yaml", "http://localhost", None, None)

def test_load_owl():
    # Just test that it doesn't raise an exception
    ingest_owl_driver("../OwlModels/import.yaml", "http://localhost", None, None)
