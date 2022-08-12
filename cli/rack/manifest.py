from enum import Enum
from jsonschema import validate
from rack.types import Connection, Url
from typing import Any, Dict, List, Tuple
import yaml
import semtk3

MANIFEST_SCHEMA: Dict[str, Any] = {
    'type': 'object',
    'additionalProperties': False,
    'required': [],
    'properties': {
        'footprint': {
            'type': 'object',
            'additionalProperties': False,
            'required': [],
            'properties': {
                'model-graphs': {'type': 'array', 'items': {'type': 'string'}},
                'data-graphs': {'type': 'array', 'items': {'type': 'string'}},
                'nodegroups': {'type': 'array', 'items': {'type': 'string'}},
            }
        },
        'steps': {
            'type': 'array',
            'items': {
                'oneOf': [
                    {
                        'type': 'object',
                        'additionalProperties': False,
                        'required': ['data'],
                        'properties': {
                            'data': {'type': 'string'},
                        }
                    },
                    {
                        'type': 'object',
                        'additionalProperties': False,
                        'required': ['model'],
                        'properties': {
                            'model': {'type': 'string'},
                        }
                    },
                    {
                        'type': 'object',
                        'additionalProperties': False,
                        'required': ['nodegroups'],
                        'properties': {
                            'nodegroups': {'type': 'string'}
                        }
                    },
                    {
                        'type': 'object',
                        'additionalProperties': False,
                        'required': ['manifest'],
                        'properties': {
                            'manifest': {'type': 'string'}
                        }
                    },
                ]
            }
        }
    }
}


class StepType(Enum):
    MODEL = 1
    DATA = 2
    NODEGROUPS = 3
    MANIFEST = 4


class Manifest:
    def __init__(self) -> None:
        self.modelgraphsFootprint: List[Url] = []
        self.datagraphsFootprint: List[Url] = []
        self.nodegroupsFootprint: List[str] = []
        self.modelFootprint: bool = False
        self.steps: List[Tuple[StepType, str]] = []

    def addModelgraphFootprint(self, modelgraph: Url) -> None:
        self.modelgraphsFootprint.append(modelgraph)

    def addDatagraphFootprint(self, datagraph: Url) -> None:
        self.datagraphsFootprint.append(datagraph)

    def addModelFootprint(self) -> None:
        self.modelFootprint = True

    def addNodegroupsFootprint(self, nodegroupRegexp: str) -> None:
        self.nodegroupsFootprint.append(nodegroupRegexp)

    def addStep(self, stepType: StepType, stepFile: str) -> None:
        self.steps.append((stepType, stepFile))

    def getConnection(self, triple_store: str = "http://localhost:3030/RACK", triple_store_type: str = "fuseki") -> Connection:
        return Connection(semtk3.build_connection_str("%NODEGROUP%", triple_store_type, triple_store, self.modelgraphsFootprint, self.datagraphsFootprint[0], self.datagraphsFootprint[1:]))

    @staticmethod
    def fromYAML(src: Any) -> 'Manifest':
        """Populate a Manifest using a YAML file following the MANIFEST_SCHEMA."""
        obj = yaml.safe_load(src)
        validate(obj, MANIFEST_SCHEMA)

        manifest = Manifest()

        footprint = obj.get('footprint', {})
        for datagraph in footprint.get('data-graphs', []):
            manifest.addDatagraphFootprint(Url(datagraph))
        for nodegroupRegexp in footprint.get('nodegroups', []):
            manifest.addNodegroupsFootprint(nodegroupRegexp)
        for modelgraph in footprint.get('model-graphs', []):
            manifest.addModelgraphFootprint(Url(modelgraph))

        for step in obj.get('steps', []):
            if 'data' in step:
                manifest.addStep(StepType.DATA, step['data'])
            elif 'model' in step:
                manifest.addStep(StepType.MODEL, step['model'])
            elif 'nodegroups' in step:
                manifest.addStep(StepType.NODEGROUPS, step['nodegroups'])
            elif 'manifest' in step:
                manifest.addStep(StepType.MANIFEST, step['manifest'])

        return manifest
