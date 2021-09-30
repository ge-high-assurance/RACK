import { Flavor } from './flavor'
import { Props } from './svg-view'
import { Triple } from './triple'
import { Namespace } from './uri'

export type EdgeId = Flavor<string, 'EdgeId'> & string
export type NodeId = Flavor<string, 'NodeId'> & string


export type Node = {
    color: string
    id: string
    label: string
}


export type Edge<N extends Node> = {
    color: string
    id: string
    label: string
    source: N
    target: N
    thickness: number
}


export type GraphData<N extends Node, E extends Edge<N>> = {
    [namespaceId: string]: {
        edges: { [edgeId: string]: E },
        nodes: { [nodeId: string]: N },
    }
}


export interface GraphProvider<N extends Node, E extends Edge<N>> {

    getGraph(
        triples: Triple[],
        sortedNamespaces: Namespace[],
    ): GraphData<N, E>

    renderNode(
        props: Props<N, E>,
        node: N,
    ): string

}

/**
* CPS encoding of the existential type `(exists N E. GraphProvider<N, E>)`.
*/
export type GetGraphProvider = <R>(cont: <N extends Node, E extends Edge<N>>(graphProvider: GraphProvider<N, E>) => R) => R
