from enum import Enum
from jsonschema import validate
from rack.types import Connection, Url
from typing import Any, Dict, List, Tuple, Optional
import yaml
import semtk3

MANIFEST_SCHEMA: Dict[str, Any] = {
    'type': 'object',
    'additionalProperties': False,
    'required': ['name'],
    'properties': {
        'name': {'type': 'string'},
        'description': {'type': 'string'},
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
                    {
                        'type': 'object',
                        'additionalProperties': False,
                        'required': ['copygraph'],
                        'properties': {
                            'copygraph': {
                                'type': 'object',
                                'additionalProperties': False,
                                'required': ['from-graph', 'to-graph'],
                                'properties': {
                                    'from-graph': {'type': 'string'},
                                    'to-graph': {'type': 'string'},
                                }
                            }
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
    COPYGRAPH = 5


class Manifest:
    def __init__(self, name: str, description: Optional[str] = None) -> None:
        self.name: str = name
        self.description: Optional[str] = description
        self.modelgraphsFootprint: List[Url] = []
        self.datagraphsFootprint: List[Url] = []
        self.nodegroupsFootprint: List[str] = []
        self.steps: List[Tuple[StepType, Any]] = []

    def getName(self) -> str:
        return self.name

    def getDescription(self) -> Optional[str]:
        return self.description

    def addModelgraphFootprint(self, modelgraph: Url) -> None:
        self.modelgraphsFootprint.append(modelgraph)

    def addDatagraphFootprint(self, datagraph: Url) -> None:
        self.datagraphsFootprint.append(datagraph)

    def addNodegroupsFootprint(self, nodegroupRegexp: str) -> None:
        self.nodegroupsFootprint.append(nodegroupRegexp)

    def getModelgraphsFootprint(self) -> List[Url]:
        return self.modelgraphsFootprint

    def getDatagraphsFootprint(self) -> List[Url]:
        return self.datagraphsFootprint

    def getNodegroupsFootprint(self) -> List[str]:
        return self.nodegroupsFootprint

    def addStep(self, stepType: StepType, stepFile: Any) -> None:
        self.steps.append((stepType, stepFile))

    def getConnection(self, triple_store: str = "http://localhost:3030/RACK", triple_store_type: str = "fuseki") -> Connection:
        """Build a connection string using the graphs defined in the footprint."""
        return Connection(semtk3.build_connection_str(self.name, triple_store_type, triple_store, self.modelgraphsFootprint, self.datagraphsFootprint[0], self.datagraphsFootprint[1:]))

    def getDefaultGraphConnection(self, triple_store: str = "http://localhost:3030/RACK", triple_store_type: str = "fuseki") -> Connection:
        """Build a connection string using the triple store's default graph."""
        return semtk3.build_default_connection_str("Default Graph", triple_store_type, triple_store)

    @staticmethod
    def fromYAML(src: Any) -> 'Manifest':
        """Populate a Manifest using a YAML file following the MANIFEST_SCHEMA."""
        obj = yaml.safe_load(src)
        validate(obj, MANIFEST_SCHEMA)

        manifest = Manifest(obj.get('name'), obj.get('description'))

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
            elif 'copygraph' in step:
                args = step['copygraph']
                manifest.addStep(StepType.COPYGRAPH, (args['from-graph'], args['to-graph']))

        return manifest
