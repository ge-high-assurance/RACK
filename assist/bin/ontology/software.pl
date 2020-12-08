% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(software,
          [
            build/1,
            code_development/1,
            code_gen/1,
            compile/1,
            component/1,
            component_type/1,
            file/1,
            format/1,
            package/1,
            annotations/2,
            compileInput/2,
            compiledBy/2,
            componentType/2,
            controlFlowsToUnconditionally/2,
            createBy/2,
            definedIn/2,
            fileFormat/2,
            filename/2,
            mentions/2,
            name/2,
            packageInput/2,
            packagedBy/2,
            subcomponentOf/2,
            valueType/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

build(C) :- rack_instance('SOFTWARE#BUILD', C).
code_development(C) :- rack_instance('SOFTWARE#CODE_DEVELOPMENT', C).
code_gen(C) :- rack_instance('SOFTWARE#CODE_GEN', C).
compile(C) :- rack_instance('SOFTWARE#COMPILE', C).
component(C) :- rack_instance('SOFTWARE#COMPONENT', C).
component_type(C) :- rack_instance('SOFTWARE#COMPONENT_TYPE', C).
file(C) :- rack_instance('SOFTWARE#FILE', C).
format(C) :- rack_instance('SOFTWARE#FORMAT', C).
package(C) :- rack_instance('SOFTWARE#PACKAGE', C).

annotations(A, B) :- rdf(A, rack:'SOFTWARE#annotations', B).
compileInput(A, B) :- rdf(A, rack:'SOFTWARE#compileInput', B).
compiledBy(A, B) :- rdf(A, rack:'SOFTWARE#compiledBy', B).
componentType(A, B) :- rdf(A, rack:'SOFTWARE#componentType', B).
controlFlowsToUnconditionally(A, B) :- rdf(A, rack:'SOFTWARE#controlFlowsToUnconditionally', B).
createBy(A, B) :- rdf(A, rack:'SOFTWARE#createBy', B).
definedIn(A, B) :- rdf(A, rack:'SOFTWARE#definedIn', B).
fileFormat(A, B) :- rdf(A, rack:'SOFTWARE#fileFormat', B).
filename(A, B) :- rdf(A, rack:'SOFTWARE#filename', B).
mentions(A, B) :- rdf(A, rack:'SOFTWARE#mentions', B).
name(A, B) :- rdf(A, rack:'SOFTWARE#name', B).
packageInput(A, B) :- rdf(A, rack:'SOFTWARE#packageInput', B).
packagedBy(A, B) :- rdf(A, rack:'SOFTWARE#packagedBy', B).
subcomponentOf(A, B) :- rdf(A, rack:'SOFTWARE#subcomponentOf', B).
valueType(A, B) :- rdf(A, rack:'SOFTWARE#valueType', B).
