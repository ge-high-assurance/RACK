# Copyright (c) 2020, General Electric Company and Galois, Inc.
"""Pytest fixtures and configuration"""

import pytest

from time import sleep
from typing import Any

from semtk3 import check_services, set_host

@pytest.fixture(scope="session")
def rack_in_a_box(docker_ip: str, docker_services: Any) -> str:
    """Ensure that RACK-in-a-box is up and responsive."""
    url = "http://{}".format(docker_ip)
    set_host(url)
    docker_services.wait_until_responsive(
         timeout=240.0, pause=0.1, check=lambda: check_services()
    )
    return url
