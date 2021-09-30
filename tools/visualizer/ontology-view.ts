import { colorFor } from './colors'
import { Flavor } from './flavor'
import { Edge, GraphData, Node, GetGraphProvider } from './graph-provider'
import { RDF_SCHEMA_DOMAIN, RDF_SCHEMA_RANGE } from './rdf-schema'
import { equalsURI, Triple } from './triple'
import { idForNamespace, idForURI, Namespace, URI } from './uri'
import { isXMLSchemaType } from './xml-schema'


export const graphProvider: GetGraphProvider = (cont) => cont({
    getGraph,
    renderNode: (props, n) => {
        const properties = n.properties
            .map(p => `<tr><td>${p.property.name}</td></tr>`)
            .join('\n')
        return `
            <tr style="text-align: center">
                <td>${n.label}</td>
            </tr>
            ${props.showProperties ? properties : ''}
        `
    },
})


export type EdgeId = Flavor<string, 'EdgeId'> & string
export type NodeId = Flavor<string, 'NodeId'> & string


export type Property = {
    property: URI
    range: URI
}


type RACKPropertyKey = Flavor<string, 'RACKPropertyKey'>
type RACKClassKey = Flavor<string, 'RACKClassKey'>


export function labelForURI(u: URI): string {
    if (u.value) { return u.value } else { return u.name }
}


type OntologyViewNode = Node & {
    properties: Property[]
}


function getGraph(
    triples: Triple[],
    sortedNamespaces: Namespace[],
): GraphData<OntologyViewNode, Edge<OntologyViewNode>> {

    /*
    In this function we are looking to transform the RDF triples a bit.  The
    idea is that an ontology link such as:

    ClassA --someProperty--> ClassB

    is represented in RDF as two triples:
    - (someProperty, domain, ClassA)
    - (someProperty, range, ClassB)

    We try to invert this process, however there are complications, as SADL also
    allows:

    ClassA --someProperty--> {ClassB, ClassC, ClassD}

    which makes the RDF a little more complicated and we don't handle this yet.
    */

    const graphData: GraphData<OntologyViewNode, Edge<OntologyViewNode>> = {}

    const registerNamespace = (ns: Namespace) => {
        const nsId = idForNamespace(ns)
        if (!(nsId in graphData)) {
            graphData[nsId] = { nodes: {}, edges: {} }
        }
    }

    const domainTriples: Record<RACKPropertyKey, Triple> = {}
    const rangeTriples: Record<RACKPropertyKey, Triple> = {}

    const propertiesURIs: Record<RACKPropertyKey, URI> = {}
    /**
    * Gathers all triples that define "simple" properties of some classes, e.g.
    * string, number, etc.
    */
    const classFieldTriples: Record<RACKPropertyKey, Triple> = {}

    triples.forEach(triple => {

        // Register if domain
        if (equalsURI(triple.property, RDF_SCHEMA_DOMAIN)) {
            domainTriples[idForURI(triple.source)] = triple
        }
        // Register if range
        if (equalsURI(triple.property, RDF_SCHEMA_RANGE)) {
            rangeTriples[idForURI(triple.source)] = triple
        }

        // If domain or range, register its property (or as a class field if XML
        // type)
        if (
            (
                equalsURI(triple.property, RDF_SCHEMA_DOMAIN)
                || equalsURI(triple.property, RDF_SCHEMA_RANGE)
            )
        ) {
            // We treat properties that have targets like float, string, etc.
            // differently from properties whose target is another class.
            if (isXMLSchemaType(triple.target)) {
                classFieldTriples[idForURI(triple.source)] = triple
            } else {
                propertiesURIs[idForURI(triple.source)] = triple.source
            }
        }

    })

    // Those properties we're missing either the domain or the range.
    const propertiesMissingSomething: RACKPropertyKey[] = []
    const classes: Record<RACKClassKey, URI> = {}
    Object.keys(propertiesURIs).forEach(
        (propertyKey: RACKPropertyKey) => {

            if (!(propertyKey in domainTriples)) {
                console.log(`Missing domain: ${propertyKey} `)
                propertiesMissingSomething.push(propertyKey)
            } else {
                const domain: URI = domainTriples[propertyKey].target
                classes[idForURI(domain)] = domain
            }

            if (!(propertyKey in rangeTriples)) {
                console.log(`Missing range: ${propertyKey} `)
                propertiesMissingSomething.push(propertyKey)
            } else {
                const range: URI = rangeTriples[propertyKey].target
                classes[idForURI(range)] = range
            }

        }
    )

    Object.keys(propertiesURIs)
        .filter((key: RACKPropertyKey) => !(propertiesMissingSomething.some(k => k === key)))
        .map(id => {
            const sourceURI = domainTriples[id].target
            const propertyURI = propertiesURIs[id]
            const targetURI = rangeTriples[id].target

            registerNamespace(propertyURI.ns)
            const nsId = idForNamespace(propertyURI.ns)
            const nodes = graphData[nsId].nodes
            const edges = graphData[nsId].edges

            // Ensure there is a copy of this source in this namespace's nodes
            const sourceId = idForURI(sourceURI)
            if (!(sourceId in nodes)) { nodes[sourceId] = nodeOfURI(sortedNamespaces, sourceURI) }

            if (isXMLSchemaType(targetURI)) {
                nodes[idForURI(sourceURI)].properties.push({
                    property: propertyURI,
                    range: targetURI,
                })
                return
            } else {
                // Ensure there is a copy of this target in this namespace's nodes
                const targetId = idForURI(targetURI)
                if (!(targetId in nodes)) { nodes[targetId] = nodeOfURI(sortedNamespaces, targetURI) }
            }

            edges[id] = {
                color: colorFor(sortedNamespaces, propertiesURIs[id]),
                id,
                label: labelForURI(propertiesURIs[id]),
                source: nodes[idForURI(sourceURI)],
                target: nodes[idForURI(targetURI)],
                thickness: 3,
            }
        })

    return graphData

}


function nodeOfURI(sortedNamespaces: Namespace[], uri: URI): OntologyViewNode {
    return {
        color: colorFor(sortedNamespaces, uri),
        id: idForURI(uri),
        label: uri.name,
        // uri,
        properties: [],
    }
}
