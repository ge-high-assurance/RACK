#!/usr/bin/env python3
#
# Copyright (c) 2021, General Electric Company, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

import os.path
import shutil

from setuptools import setup, find_packages
from setuptools.command.install import install

from AutoGeneration.GenerateSTK import autogen


class CustomInstall(install):
    def run(self):
        autogen()
        shutil.copy("Evidence/Add.py", "build/lib/Evidence/Add.py")
        shutil.copy("Evidence/CONSTANTS.py", "build/lib/Evidence/CONSTANTS.py")
        shutil.copy("Evidence/RACK-DATA.xsd", "build/lib/Evidence/RACK-DATA.xsd")
        if not os.path.isfile("Evidence/Add.py"):
            raise(Exception("Evidence/Add.py does not exist, investigate!"))
        if not os.path.isfile("Evidence/CONSTANTS.py"):
            raise(Exception("Evidence/CONSTANTS.py does not exist, investigate!"))
        install.run(self)

setup(
    cmdclass={"install": CustomInstall},
)
