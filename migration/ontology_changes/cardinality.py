from abc import ABC, abstractmethod
from dataclasses import dataclass


class Cardinality(ABC):

    @abstractmethod
    def text_description(self) -> str:
        ...


@dataclass
class AtMost(Cardinality):
    at_most: int

    def text_description(self) -> str:
        if self.at_most == 1:
            return f"at most {self.at_most} value"
        else:
            return f"at most {self.at_most} values"


@dataclass
class SingleValue(Cardinality):

    def text_description(self) -> str:
        return "single value"


@dataclass
class Unconstrained(Cardinality):

    def text_description(self) -> str:
        return "unconstrained"
