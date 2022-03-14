from abc import ABC, abstractmethod
from dataclasses import dataclass

from migration_helpers.name_space import NameSpace, get_uri

from ontology_changes.ontology_change import (
    stylize_class,
)

class RangeRestriction(ABC):

    @abstractmethod
    def text_description(self) -> str:
        ...


@dataclass
class OnlyValuesOfType(RangeRestriction):
    type_namespace: NameSpace
    type_class: str

    def text_description(self) -> str:
        t = stylize_class(get_uri(self.type_namespace, self.type_class))
        return f"only values of type {t}"
