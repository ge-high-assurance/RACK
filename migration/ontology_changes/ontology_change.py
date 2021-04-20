# Copyright (c) 2020, Galois, Inc.
#
# All Rights Reserved
#
# This material is based upon work supported by the Defense Advanced Research
# Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
#
# Any opinions, findings and conclusions or recommendations expressed in this
# material are those of the author(s) and do not necessarily reflect the views
# of the Defense Advanced Research Projects Agency (DARPA).

import logging
from abc import ABC, abstractmethod
from typing import List
import sys

from colorama import Fore, Style
from semtk import SemTKJSON

# c.f. https://github.com/python/typeshed/issues/3500
if sys.version_info >= (3, 8):
    from typing import TypedDict
else:
    from typing_extensions import TypedDict

LOGGER_ID = "migration-logger"

logger = logging.getLogger(LOGGER_ID)


def log_apply_change(which_change: str) -> None:
    logger.debug(f"  {which_change}")


def log_change(change: str) -> None:
    logger.info(f"     {change}")


def log_additional_deletion(
    thing_that_was_deleted: str,
    reason: str,
) -> None:
    log_change(
        f"  ➥ Also deleting {stylize_json(thing_that_was_deleted)} because {reason}"
    )


def log_additional_change(
    thing_that_changed: str,
    from_value: str,
    to_value: str,
) -> None:
    log_change(
        f"  ➥ Also changing {stylize_json(thing_that_changed)} from {from_value} to {to_value}"
    )


def stylize_class(name: str) -> str:
    return f"{Fore.MAGENTA}{name}{Style.RESET_ALL}"


def stylize_file_name(name: str) -> str:
    return f"{Fore.GREEN}{name}{Style.RESET_ALL}"


def stylize_json(json: str) -> str:
    return f"{Fore.YELLOW}{json}{Style.RESET_ALL}"


def stylize_property(prop: str) -> str:
    return f"{Fore.CYAN}{prop}{Style.RESET_ALL}"


# Design decision:
#
# - Currently assuming there will be changes that cannot be done line-per-line
# and will need whole-file transformations.
#
# - Thereby also assuming that we can fit the whole file in memory.  Not sure
# this will stay true if used on huge real data.


# File = List[List[str]]
# Header = List[str]
# Line = List[str]
# Lines = List[Line]

# HeaderMigrater = Callable[[Header], Header]
# LineMigrater = Callable[[Header, Line], Line]
# FileMigrater = Callable[[File], File]


class OntologyChange(ABC):
    # def generic_migrate_file(
    #     self, migrate_header: HeaderMigrater, migrate_line: LineMigrater
    # ) -> FileMigrater:
    #     def migrate(lines: Lines) -> Lines:
    #         if len(lines) == 0:
    #             return lines
    #         new_header = migrate_header(lines[0])
    #         new_lines = [migrate_line(lines[0], line) for line in lines[1:]]
    #         return [new_header] + new_lines

    #     return migrate

    # def migrate_file(self, file: File) -> File:
    #     return self.generic_migrate_file(self.migrate_header, self.migrate_line)(file)

    # @abstractmethod
    # def migrate_header(self, header: Header) -> Line:
    #     ...

    @abstractmethod
    def text_description(self) -> str:
        ...

    @abstractmethod
    def migrate_json(self, json: SemTKJSON) -> None:
        """In-place migration of SemTK JSON."""
        ...

    # @abstractmethod
    # def migrate_line(self, header: Header, line: Line) -> Line:
    #     ...


Commit = TypedDict(
    "Commit",
    {
        "number": str,
        "changes": List[OntologyChange],
    },
)
