import { match } from 'ts-pattern'

import { colorFor } from './colors'
import { Edge, GetGraphProvider, GraphData, GraphProvider, Node } from './graph-provider'
import { isRDFTypeTriple } from './rdf'
import { idForNamespace, idForURI, Namespace, URI } from './uri'
import * as ViewType from './view-type'


export const graphProvider: (viewType: ViewType.InstanceViewType) => GetGraphProvider =
    (viewType) => (cont) => cont({
        getGraph: getGraph(viewType),
        renderNode: (_props, n) => {
            return `
            <tr style="text-align: center">
                <td>${n.label}</td>
            </tr>
        `
        },
    })


type DataViewBySourceEdge = Edge<Node> & {
    count: number
    property: URI
}


export function getGraph(
    instanceViewType: ViewType.InstanceViewType,
): GraphProvider<Node, DataViewBySourceEdge>['getGraph'] {

    return (triples, sortedNamespaces) => {

        const typeDeclarations = triples.filter(isRDFTypeTriple)

        const graphData: GraphData<Node, DataViewBySourceEdge> = {}

        triples.forEach(t => {

            // We're not interested in counting these occurrences.
            if (isRDFTypeTriple(t)) { return }

            const ns = match(instanceViewType)
                .with(ViewType.InstanceViewBySource.instanceViewType, () => t.source.ns)
                .with(ViewType.InstanceViewByProperty.instanceViewType, () => t.property.ns)
                .with(ViewType.InstanceViewByTarget.instanceViewType, () => t.target.ns)
                .exhaustive()
            const nsId = idForNamespace(ns)
            if (!(nsId in graphData)) {
                graphData[nsId] = { nodes: {}, edges: {} }
            }

            const nodes = graphData[nsId].nodes
            const edges = graphData[nsId].edges

            const sourceType = typeDeclarations.find(v => idForURI(v.source) === idForURI(t.source))?.target
            if (!sourceType) {
                console.log(`Weird, no source type for ${idForURI(t.source)}`)
                return
            }
            const targetType = typeDeclarations.find(v => idForURI(v.source) === idForURI(t.target))?.target
            if (targetType === undefined) { return } // can happen, in e.g. "string"

            const sourceId = idForURI(sourceType)
            const targetId = idForURI(targetType)
            if (!(sourceId in nodes)) {
                nodes[sourceId] = nodeOfURI(sortedNamespaces, sourceType)
            }
            if (!(targetId in nodes)) {
                nodes[targetId] = nodeOfURI(sortedNamespaces, targetType)
            }

            const edgeId = `${idForURI(sourceType)}-${idForURI(t.property)}-${idForURI(targetType)}`
            if (edgeId in edges) {
                const newCount = edges[edgeId].count + 1
                Object.assign(edges[edgeId], {
                    count: newCount,
                    label: `${t.property.name} (${newCount})`,
                    thickness: 3 + Math.log10(newCount),
                })
            } else {
                edges[edgeId] = {
                    color: colorFor(sortedNamespaces, t.property),
                    count: 1,
                    id: edgeId,
                    label: `${t.property.name} (1)`,
                    property: t.property,
                    source: nodeOfURI(sortedNamespaces, sourceType),
                    target: nodeOfURI(sortedNamespaces, targetType),
                    thickness: 3,
                }
            }
        })

        return graphData

    }
}


function nodeOfURI(sortedNamespaces: Namespace[], uri: URI): Node {
    return {
        color: colorFor(sortedNamespaces, uri),
        id: idForURI(uri),
        label: uri.name,
    }
}
