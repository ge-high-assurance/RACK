
connStringSource = """
{   "name":"RACK local fuseki Apache Phase 2 Resolved",
    "domain":"",
    "enableOwlImports":false,
    "model":[
        {"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/model"}
        ],
    "data":[
        {"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/Data"}
        ]
}"""
connStringResolved = """
{   "name":"RACK local fuseki Apache Phase 2 Resolved",
    "domain":"",
    "enableOwlImports":false,
    "model":[
        {"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/model"}
        ],
    "data":[
        {"type":"fuseki","url":"http://localhost:3030/RACK","graph":"http://rack001/ResolvedData"}
        ]
}"""

entityTypeQuery = '''prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix PROV_S:<http://arcos.rack/PROV-S#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
select distinct ?directSub
		FROM <http://rack001/model>
 where { ?directSub rdfs:subClassOf ?super.
 values ?super{PROV_S:ENTITY} .}
'''

activityTypeQuery = '''prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix PROV_S:<http://arcos.rack/PROV-S#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
select distinct ?directSub
		FROM <http://rack001/model>
 where { ?directSub rdfs:subClassOf ?super.
 values ?super{PROV_S:ACTIVITY} .}
'''

agentTypeQuery = '''prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix PROV_S:<http://arcos.rack/PROV-S#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
select distinct ?directSub
		FROM <http://rack001/model>
 where { ?directSub rdfs:subClassOf ?super.
 values ?super{PROV_S:AGENT} .}
'''

classQuery = '''prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix PROV_S:<http://arcos.rack/PROV-S#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
select distinct ?directSub
		FROM <http://rack001/model>
 where { ?directSub rdfs:subClassOf ?super.
 values ?super{<{{Type}}>} .}
'''

subClassQuery = '''prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix PROV_S:<http://arcos.rack/PROV-S#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
select distinct ?super
		FROM <http://rack001/model>
 where { ?directSub rdfs:subClassOf ?super.
 values ?directSub{<{{Type}}>} .}
'''

instanceQuery = '''prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix PROV_S:<http://arcos.rack/PROV-S#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
select distinct ?instance ?super
		FROM <http://rack001/data>
 where { ?instance a ?super.
 values ?super{<{{Types}}>} .}
'''

dataQuery = """prefix rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix semtk:<uri://semtk#>
prefix XMLSchema:<http://www.w3.org/2001/XMLSchema#>
prefix PROV_S:<http://arcos.rack/PROV-S#>
prefix rdfs:<http://www.w3.org/2000/01/rdf-schema#>
CONSTRUCT {
	?THING a ?THING_type .
	?THING ?dp ?o .

	?THING ?p ?OBJ .
	?OBJ a ?OBJ_type .
		?OBJ PROV_S:identifier ?OBJ_identifier .

	?OBJ2 ?ap ?THING .
	?OBJ2 a ?OBJ2_type .
		?OBJ2 PROV_S:identifier ?OBJ2_identifier .
}
		FROM <http://rack001/data>
		FROM <http://rack001/model>
where {
    ?THING a ?THING_type .
    ?THING_type rdfs:subClassOf* PROV_S:THING .
    FILTER ( ?THING IN (<uri://semtk#{{GUID}}> ) )  . 
    optional{
        ?THING ?p ?OBJ .
        ?OBJ a ?OBJ_type .
        ?OBJ PROV_S:identifier ?OBJ_identifier .
        ?OBJ_type rdfs:subClassOf* PROV_S:THING .
    }
    optional{
        ?THING ?dp ?o
    }
    optional{
        ?OBJ2 ?ap ?THING .
        ?OBJ2 a ?OBJ2_type .
        ?OBJ2 PROV_S:identifier ?OBJ2_identifier .
        ?OBJ2_type rdfs:subClassOf* PROV_S:THING .
    }
}"""
