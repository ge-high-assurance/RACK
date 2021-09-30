import deepEqual from 'deep-equal'
import * as React from 'react'
import { FilePicker } from 'react-file-picker'
import { match, select } from 'ts-pattern'

import { colorForNamespace } from './colors'
import * as DataView from './data-view'
import { Edge, GetGraphProvider, Node } from './graph-provider'
import * as OntologyView from './ontology-view'
import { isOWLProperty } from './owl'
import { isRACKOntologyClassDeclaration } from './rack'
import { isRDFSchemaComment } from './rdf-schema'
import { SVGView } from './svg-view'
import { mentionsAnonymousClass, parseTriple, Triple } from './triple'
import { idForNamespace, Namespace } from './uri'
import * as ViewType from './view-type'


type Props = {
}


function getSortedNamespaces(triples: Triple[]): Namespace[] {
    /**
    * If we only sort by full URI, then the: http://arcos.rack/MIL-STD-881D/...
    * end up in the middle of: http://arcos.rack/HAZARD and:
    * http://arcos/rack/MODEL So it is nicer to sort per domain, then per
    * namespace within a domain.
    */
    const domains: { [id: string]: { [id: string]: Namespace } } = {}

    function insert(ns: Namespace) {
        if (!(ns.domain in domains)) { domains[ns.domain] = {} }
        domains[ns.domain][idForNamespace(ns)] = ns
    }

    // const namespaces: { [id: string]: Namespace } = {}
    triples.forEach(t => {
        insert(t.source.ns)
        insert(t.property.ns)
        insert(t.target.ns)
    })

    return (Object.keys(domains).sort().flatMap(domain =>
        Object.keys(domains[domain]).sort().map(k => domains[domain][k]))
    )
}


export const Visualizer: React.FC<Props> = () => {

    const [namespaceSelection, setNamespaceSelection] = React.useState<Set<string>>(new Set())
    const [selectedDomain, setSelectedDomain] = React.useState<string | null>(null)
    const [showProperties, setShowProperties] = React.useState(false)
    const [triples, setTriples] = React.useState<Triple[]>([])
    const [viewType, setViewType] = React.useState<ViewType.ViewType>(ViewType.OntologyView)

    // const rackOntologyClasses = triples.filter(isRACKOntologyClassDeclaration).map(t => t.source)
    // const rackNamespaces = new Set(rackOntologyClasses.map(u => idForNamespace(u.ns)))

    const interestingTriples = triples.filter(t =>
        (!isRDFSchemaComment(t.property))
        && (!isOWLProperty(t))
        && (!isRACKOntologyClassDeclaration(t))
        && (!mentionsAnonymousClass(t))
    )

    const sortedNamespaces = getSortedNamespaces(interestingTriples)
    const domains = sortedNamespaces.map(ns => ns.domain).filter((d, i) => sortedNamespaces.findIndex(ns => ns.domain === d) === i)

    // const rackOntologyClasses = triples.filter(isRACKOntologyClassDeclaration).map(t => t.source)
    // const rackNamespaces = new Set(rackOntologyClasses.map(u => idForNamespace(u.ns)))

    const renderDomains = domains.map((d) =>
        <option key={d} value={d}>{d}</option>
    )

    const isSelectedDomain = ns => {
        if (selectedDomain !== null) {
            return ns.domain === selectedDomain
        } else if (sortedNamespaces.length > 0) {
            return ns.domain === sortedNamespaces[0].domain
        } else {
            return false
        }
    }

    const renderClasses = (
        nodesForNamespace: (ns: string) => Node[],
        edgesForNamespace: (ns: string) => Edge<Node>[],
    ) =>
        Array
            .from(sortedNamespaces)
            .filter(isSelectedDomain)
            .map((ns, i) => {
                const nsId = idForNamespace(ns)
                const nodes = nodesForNamespace(nsId)
                const edges = edgesForNamespace(nsId)
                console.log(nsId, nodes, edges)
                // const graph = graphProvider[nsId]
                // console.log(graph)
                // const ns = triples.map(t => t.source).find(v => idForNamespace(v.ns) === ns)
                // const ns = rackOntologyClasses.find(v => idForNamespace(v.ns) === nsId)
                return (
                    <div key={i}>
                        <input
                            id={nsId}
                            name={nsId}
                            type="checkbox"
                            checked={namespaceSelection.has(nsId)}
                            onChange={e => {
                                if (e.target.checked) {
                                    setNamespaceSelection(old => {
                                        const newSet = new Set(old.values())
                                        newSet.add(nsId)
                                        return newSet
                                    })
                                } else {
                                    setNamespaceSelection(old => {
                                        const newSet = new Set(old.values())
                                        newSet.delete(nsId)
                                        return newSet
                                    })
                                }
                            }}
                        />
                        <label htmlFor={nsId}>
                            <span style={{ color: colorForNamespace(sortedNamespaces, ns) }}> ⬤ </span>
                            {ns.namespace}
                            <span style={{ fontSize: '12px' }}> (N = {nodes.length}, E = {edges.length})</span>
                        </label>
                    </div>
                )
            })

    return getGraphProvider(viewType)(graphProvider => {

        const graph = graphProvider.getGraph(interestingTriples, sortedNamespaces)

        const nodesForNamespace = (nsId: string) => {
            if (!(nsId in graph)) {
                console.log(`Missing namespace in graph: ${nsId}`)
                return []
            }
            return Object.values(graph[nsId].nodes)
        }

        const edgesForNamespace = (nsId: string) => {
            if (!(nsId in graph)) {
                console.log(`Missing namespace in graph: ${nsId}`)
                return []
            }
            return Object.values(graph[nsId].edges)
        }

        const nodes = Array.from(namespaceSelection).flatMap(nodesForNamespace)
            // Nodes are duplicated in each namespace they participate in, so we
            // remove duplicates here.
            .filter((n, i, self) => self.findIndex(v => v.id === n.id) === i)

        const edges = Array.from(namespaceSelection).flatMap(edgesForNamespace)

        // const renderNodes = elements.nodes.map((n, i) => (
        //     <li key={i}>{labelForURI(n.uri)}</li>
        // ))

        return (
            <div
                style={{
                    display: 'flex',
                    flexDirection: 'row',
                    height: '100%',
                    width: '100%',
                }}
            >

                <div style={{
                    overflow: 'scroll',
                    width: '20%',
                }}>

                    <p style={{ marginLeft: '10px' }}>
                        <FilePicker
                            extensions={['json']}
                            onChange={async (file: File) => {
                                const json = await file.text()
                                setTriples(JSON.parse(json).map(parseTriple))
                                setNamespaceSelection(new Set())
                                setSelectedDomain(null)
                            }}
                        >
                            <button>Upload JSON dump</button>
                        </FilePicker>
                    </p>

                    <form style={{ margin: '10px' }}>
                        <fieldset>
                            <input type="radio" name="viewType"
                                checked={deepEqual(viewType, ViewType.OntologyView)}
                                onChange={e => setViewType(ViewType.fromString(e.target.value))}
                                id={ViewType.toString(ViewType.OntologyView)}
                                value={ViewType.toString(ViewType.OntologyView)}
                            />
                            <label htmlFor={ViewType.toString(ViewType.OntologyView)}>Ontology</label>

                            <br />

                            <input type="radio" name="viewType"
                                checked={deepEqual(viewType, ViewType.InstanceViewBySource)}
                                onChange={e => setViewType(ViewType.fromString(e.target.value))}
                                id={ViewType.toString(ViewType.InstanceViewBySource)}
                                value={ViewType.toString(ViewType.InstanceViewBySource)}
                            />
                            <label htmlFor={ViewType.toString(ViewType.InstanceViewBySource)}>Instance (by source)</label>

                            <br />

                            <input type="radio" name="viewType"
                                checked={deepEqual(viewType, ViewType.InstanceViewByProperty)}
                                onChange={e => setViewType(ViewType.fromString(e.target.value))}
                                id={ViewType.toString(ViewType.InstanceViewByProperty)}
                                value={ViewType.toString(ViewType.InstanceViewByProperty)}
                            />
                            <label htmlFor={ViewType.toString(ViewType.InstanceViewByProperty)}>Instance (by property)</label>

                            <br />

                            <input type="radio" name="viewType"
                                checked={deepEqual(viewType, ViewType.InstanceViewByTarget)}
                                onChange={e => setViewType(ViewType.fromString(e.target.value))}
                                id={ViewType.toString(ViewType.InstanceViewByTarget)}
                                value={ViewType.toString(ViewType.InstanceViewByTarget)}
                            />
                            <label htmlFor={ViewType.toString(ViewType.InstanceViewByTarget)}>Instance (by target)</label>

                        </fieldset>
                        <p>
                            <input
                                id="show-properties"
                                type="checkbox"
                                onChange={e => setShowProperties(e.target.checked)}
                            />
                            <span style={{ marginLeft: '10px' }}>
                                <label htmlFor="show-properties">Show Properties</label>
                            </span>
                        </p>
                        <p>
                            <select onChange={e => setSelectedDomain(e.target.value)}>
                                {renderDomains}
                            </select>
                        </p>
                        <p><button type="button" onClick={() => setNamespaceSelection(() => new Set())}>Clear all selected</button></p>
                        {renderClasses(nodesForNamespace, edgesForNamespace)}
                    </form>
                </div>

                <SVGView
                    edges={edges}
                    nodes={nodes}
                    renderNode={graphProvider.renderNode}
                    showProperties={showProperties}
                />

            </div>
        )

    })

}


function getGraphProvider(
    viewType: ViewType.ViewType,
): GetGraphProvider {
    return match(viewType)
        .with(ViewType.OntologyView, () => OntologyView.graphProvider)
        .with({ type: 'InstanceView', instanceViewType: select() }, (vt) => DataView.graphProvider(vt))
        .exhaustive()
}
