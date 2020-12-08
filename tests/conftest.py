# Copyright (c) 2020, General Electric Company and Galois, Inc.
"""Pytest fixtures and configuration"""

import pytest

from time import sleep
from os import getenv

import requests
from semtk3 import set_host


def check(url: str) -> bool:
    try:
        response = requests.post(url + ":12059/serviceInfo/ping")
        return "yes" in response.text
    except Exception as e:
        print(e)
        return False


TIMEOUT = 240.0
PAUSE = 1.0
AFTER = 20


if getenv("RACK_BOX_FIXTURE") is not None:

    @pytest.fixture(scope="session")
    def rack_url(docker_ip, docker_services) -> str:  # type: ignore
        """Ensure that RACK-in-a-box is up and responsive."""
        url = "http://{}".format(docker_ip)
        set_host(url)
        # TODO(lb): For some reason, check_services always returns false.
        # docker_services.wait_until_responsive(
        #     timeout=240.0, pause=0.1, check=lambda: check_services()
        # )
        docker_services.wait_until_responsive(
            timeout=TIMEOUT, pause=PAUSE, check=lambda: check(url)
        )
        sleep(AFTER)
        return url

else:

    @pytest.fixture(scope="session")
    def rack_url() -> str:
        """Ensure that RACK-in-a-box is up and responsive."""
        url = "http://localhost"
        set_host(url)
        counter = 0
        while not check(url):
            if counter > TIMEOUT:
                break
            sleep(PAUSE)
            counter += 1
        sleep(AFTER)
        return url
