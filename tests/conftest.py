# Copyright (c) 2020, General Electric Company and Galois, Inc.
"""Pytest fixtures and configuration"""

import pytest

from time import sleep

import requests
from semtk3 import set_host


def check(url: str) -> bool:
    try:
        response = requests.post(url + ":12059/serviceInfo/ping")
        return "yes" in response.text
    except Exception as e:
        print(e)
        return False


@pytest.fixture(scope="session")
def rack_in_a_box(docker_ip, docker_services) -> str:  # type: ignore
    """Ensure that RACK-in-a-box is up and responsive."""
    url = "http://{}".format(docker_ip)
    set_host(url)
    # TODO(lb): For some reason, check_services always returns false.
    # docker_services.wait_until_responsive(
    #     timeout=240.0, pause=0.1, check=lambda: check_services()
    # )
    docker_services.wait_until_responsive(
        timeout=240.0, pause=1.0, check=lambda: check(url)
    )
    sleep(20)
    return url
