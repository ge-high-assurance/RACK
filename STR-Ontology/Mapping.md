# Software Control Model

Most of this mapping relates the STR Data Model for Software Control Flow directly into the core RACK ontology. In a few cases identified below it was necessary to add STR-specific subtypes to capture the particular details needed.

## Entities

These subsections describe the mapping of the diagram boxes into the RACK ontology. In some cases we don't have a perfect fit and I sketch out a possible solution.

### Function

- Software syntactic elements map onto the `COMPONENT` class.
- Functions in particular will have `componentType` `SOURCE_FUNCTION`; other levels of software abstraction are also supported using different component types.
- The function name maps to our `title` property.
- The component type can describe many levels of software structure beyond function including classes, modules, variables, etc.

### Component

- RACK models the abstract parts of a system with the `SYSTEM` class
- RACK models high-level system capabilities with the `FUNCTION` class, which can be used to group systems together.
- The component name maps to the `title` property.

### Module

- As a piece of concrete software structure, modules fit into the `COMPONENT` with `componentType` `MODULE`

### Requirement

- Requirements map into the `REQUIREMENT` type.
- Description can be mapped to `description`
- Assumes can be mapped to `ifText`
- Guarantees can be mapped to `thenText`

### Requirement Document

- In the imminent, upcoming ontology refinement this will map to `DOCUMENT` or one of its subtypes.

### Port

- Ports are covered in the overlay as `StrInPort` and `StrOutPort`
- PortName maps to `title`
- Varname maps to `variableName`
- Type maps to `portType`

## Relations

Many of the relationships in the Software Control Flow Model map to existing RACK relationships, but we'll need so additional logic for handling the input and output ports.

### Informs

- Informs maps to the overlay's `informs` on the `StrSystem` subtype

### ConnectsTo

- ConnectsTo maps to the overlay's `connectsTo`

### ReceivePort / SendPort

- Association between software components and ports is provided by the `StrComponent` subclass of `COMPONENT`
- SendsVia maps to the overlay's `sendsVia`
- RecvsVia maps to the overlay's `recvsVia`

### inModule

- This maps to the `subcomponentOf` property on `COMPONENT`

### GovernedBy

- This maps to the `governs` property on `REQUIREMENT`

### ImplementsPartOf

- This maps to the `instantiates` property on `COMPONENT` links a concrete software component to a abstract `SYSTEM` component.

### PartOf

- Documents are represented as `COLLECTION` classes in the ontology. Membership in a collection is captured by the `content` relationship.

### derivedFromRequirement

- Low-level requirements will use `satisfies` to connect to their high-level requirement that they exist to satisfy.
