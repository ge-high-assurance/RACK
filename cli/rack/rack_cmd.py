#!/usr/bin/env python
# -*- python -*-
#
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
import re
import sys

import colorama
import requests
import semtk3
import yaml

from jsonschema import ValidationError

from rack import CustomFormatter, get_argument_parser
from rack import cliMethod, CLIMethod, INGEST_CSV_CONFIG_SCHEMA, INGEST_OWL_CONFIG_SCHEMA

__author__ = "Eric Mertens"
__email__ = "emertens@galois.com"

logger = logging.getLogger(__name__)

def main() -> None:
    # Sets up colors for Windows users
    colorama.init()

    # Register our custom color formatter for our logger
    stream_handler = logging.StreamHandler()
    stream_handler.setFormatter(CustomFormatter())
    logger.propagate = False
    logger.addHandler(stream_handler)
    semtk3_logger = logging.getLogger("semtk3")
    semtk3_logger.handlers = []
    semtk3_logger.propagate = False
    semtk3_logger.addHandler(stream_handler)

    args = get_argument_parser().parse_args()

    try:
        logging.basicConfig(level=args.log_level)
    except ValueError:
        logger.error('Bad log level specified')
        sys.exit(1)

    if args.base_url.endswith('/'):
        logger.warning('Trimming the final \'/\' from your base_url')
        args.base_url = args.base_url[:-1]

    try:
        if args.command is None:
            logger.error('Subcommand required (use --help to see options)')
            sys.exit(1)
        try:
            func = args.func
        except AttributeError:
            logger.error('Unknown subcommand: %s', args.command)
            sys.exit(1)
        func(args)
    except requests.ConnectionError as exc:
        logger.error('Connection failure\n%s', exc)
        sys.exit(1)
    except semtk3.restclient.RestException as exc:
        logger.error('REST Endpoint Failure\n%s', exc)
        sys.exit(1)
    except FileNotFoundError as exc:
        logger.error('File not found\n%s', exc)
        sys.exit(1)
    except yaml.YAMLError as exc:
        logger.error('Failed to load YAML configuration file: %s\n%s', args.config, exc)
        sys.exit(1)
    except ValidationError as exc:
        instance_keys = set(exc.instance.keys())
        if instance_keys == set(INGEST_CSV_CONFIG_SCHEMA['properties']) and not cliMethod == CLIMethod.DATA_IMPORT:
            logger.warning('This looks like a data ingestion schema. Did you want "rack data import"?')
        if instance_keys == set(INGEST_OWL_CONFIG_SCHEMA['properties']) and not cliMethod == CLIMethod.MODEL_IMPORT:
            logger.warning('This looks like an OWL ingestion schema. Did you want "rack model import"?')
        logger.error('Bad configuration file: %s\n%s', args.config, exc)
        sys.exit(1)
    except re.error as exc:
        logger.error('Bad regular expression: %s\n%s', exc.pattern, exc.msg)
        sys.exit(1)
