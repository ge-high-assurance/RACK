
connString2 = """
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
connString = """
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
