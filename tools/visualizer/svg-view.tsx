import * as d3 from 'd3'
import * as d3bc from 'd3-bboxCollide'
import * as d3fa from 'd3-force-attract'
import * as d3poly from 'd3-polygon'
import * as React from 'react'

import { colors, contrastingColorFor } from './colors'
import { Flavor } from './flavor'
import { Edge, EdgeId, Node, NodeId } from './graph-provider'
// import { Edge, EdgeId, labelForURI, Node, NodeId } from './ontology-view'
import { Triple } from './triple'
import { useD3 } from './use-d3'


const EDGE_SPACING = 30
const NODE_SPACING = 4


const EDGE_LAYER = 'edge-layer'
const MAIN_GROUP = 'main-group'
const NODE_LAYER = 'node-layer'

// const EDGE_CLASS = 'edge'
const EDGE_NODE_CLASS = 'edge-node'
const EDGE_LINK_CLASS = 'edge-link'
const NODE_CLASS = 'node'
const NODE_TABLE_CLASS = 'node-table'
// const EDGE_LABEL_CLASS = 'label'
const FOREIGN_OBJECT_CLASS = 'foreign-object'
const FOREIGN_OBJECT_GROUP_CLASS = 'foreign-object-group'


type LinkId = Flavor<string, 'LinkId'>


export type Props<N extends Node, E extends Edge<N>> = {
    edges: E[]
    nodes: N[]
    renderNode: (props: Props<N, E>, node: N) => string
    showProperties: boolean
    // sortedNamespaces: Namespace[]
}


type D3Edge<N extends Node, E extends Edge<N>> =
    d3.SimulationLinkDatum<D3Node<N, E>>
    & {
        id: LinkId
        originalEdge: E
        source: D3Node<N, E>
        target: D3Node<N, E>
    }

type D3NodeOfNode<N extends Node> = N & { tag: 'node' }
type D3NodeOfEdge<N extends Node, E extends Edge<N>> = E & { tag: 'edge' }

type D3Node<N extends Node, E extends Edge<N>> = (D3NodeOfNode<N> | D3NodeOfEdge<N, E>) & d3.SimulationNodeDatum


function isD3NodeOfNode<N extends Node, E extends Edge<N>>(n: D3Node<N, E>)
    : n is D3NodeOfNode<N> & d3.SimulationNodeDatum {
    return n.tag === 'node'
}

function isD3NodeOfEdge<N extends Node, E extends Edge<N>>(n: D3Node<N, E>): n is D3NodeOfEdge<N, E> & d3.SimulationNodeDatum {
    return n.tag === 'edge'
}


type NodeMutableDatum = {
    longestLabel: number
    height: number
    width: number
    x: number
    y: number
}

type NodeOfEdgeMutableDatum = {
    height: number
    width: number
    x: number
    y: number
}

type EdgeMutableDatum = {
    firstLinkId: LinkId
    secondLinkId: LinkId
    textLength: number
}

type EdgeMutableData = {
    // [id: EdgeId]: EdgeMutableDatum
    [id: string]: EdgeMutableDatum
}

type NodeMutableData = {
    // [id: NodeId]: NodeMutableDatum
    [id: string]: NodeMutableDatum
}

type NodeOfEdgeMutableData = {
    // [id: EdgeId]: NodeOfEdgeMutableDatum
    [id: string]: NodeOfEdgeMutableDatum
} & Record<EdgeId, NodeOfEdgeMutableDatum>


function linkIds<N extends Node>(e: Edge<N>): [LinkId, LinkId] {
    return [
        `${e.source.id} ---> ${e.id}`,
        `${e.id} ---> ${e.target.id}`,
    ]
}


type Component = <N extends Node, E extends Edge<N>>(props: Props<N, E>) => React.ReactElement<Props<N, E>>


export const SVGView: Component = <N extends Node, E extends Edge<N>>(props) => {

    const {
        createEdgeDatumIfMissing,
        createNodeDatumIfMissing,
        createNodeOfEdgeDatumIfMissing,
        // edgeDatum,
        nodeDatum, nodeOfEdgeDatum,
        // setEdgeDatum,
        setNodeDatum,
        // setNodeOfEdgeDatum,
    } = (() => {
        const edgeMutableDataRef = React.useRef<EdgeMutableData>({})
        const nodeMutableDataRef = React.useRef<NodeMutableData>({})
        const nodeOfEdgeMutableDataRef = React.useRef<NodeOfEdgeMutableData>({})

        // TypeScript doesn't enforce the id type when this is inlined, so using
        // type-safe wrappers.
        return {
            createEdgeDatumIfMissing: (e: E): void => {
                if (e.id in edgeMutableDataRef.current) { return }
                const [firstLinkId, secondLinkId] = linkIds(e)
                edgeMutableDataRef.current[e.id] = {
                    firstLinkId,
                    secondLinkId,
                    textLength: 0,
                }
            },
            createNodeDatumIfMissing: (id: NodeId): void => {
                if (id in nodeMutableDataRef.current) { return }
                nodeMutableDataRef.current[id] = {
                    height: 0,
                    longestLabel: 0,
                    width: 0,
                    x: 0,
                    y: 0,
                }
            },
            createNodeOfEdgeDatumIfMissing: (id: EdgeId): void => {
                if (id in nodeOfEdgeMutableDataRef.current) { return }
                nodeOfEdgeMutableDataRef.current[id] = {
                    height: 0,
                    width: 0,
                    x: 0,
                    y: 0,
                }
            },
            edgeDatum: (id: EdgeId): EdgeMutableDatum => {
                return edgeMutableDataRef.current[id]
            },
            nodeDatum: (id: NodeId): NodeMutableDatum => {
                return nodeMutableDataRef.current[id]
            },
            nodeOfEdgeDatum: (id: EdgeId): NodeOfEdgeMutableDatum => {
                return nodeOfEdgeMutableDataRef.current[id]
            },
            setEdgeDatum: (id: EdgeId, datum: EdgeMutableDatum): void => {
                edgeMutableDataRef.current[id] = datum
            },
            setNodeDatum: (id: NodeId, datum: NodeMutableDatum): void => {
                nodeMutableDataRef.current[id] = datum
            },
            setNodeOfEdgeDatum: (id: EdgeId, datum: NodeOfEdgeMutableDatum): void => {
                nodeOfEdgeMutableDataRef.current[id] = datum
            },
        }
    })()

    // const forceCollide = d3bc.bboxCollide(d => {
    //     const datum = nodeMutableDataRef.current[d.id]
    //     const spacing = datum.longestLabel / 2 + NODE_MARGIN
    //     return [
    //         [-datum.width / 2 - spacing, -datum.height / 2 - spacing],
    //         [datum.width / 2 + spacing, datum.height / 2 + spacing],
    //     ]
    // })
    //     .strength(1)
    //     .iterations(2)

    //   const forceCollide = d3.forceCollide<D3Node>().radius(d => Math.max(nodeMutableDataRef.current[d.id].width, nodeMutableDataRef.current[d.id].height) / 2)

    props.nodes.forEach(n => { createNodeDatumIfMissing(n.id) })

    props.edges.forEach(e => {
        createEdgeDatumIfMissing(e)
        createNodeOfEdgeDatumIfMissing(e.id)
        // const [fst, snd] = linkIds(e)
        // createNodeOfEdgeDatumIfMissing(fst)
        // createNodeOfEdgeDatumIfMissing(snd)
    })

    const forceAttract = d3fa.forceAttract()

    const forceCenter =
        d3.forceCenter()
            .strength(1)

    const forceLink =
        d3.forceLink<D3Node<N, E>, d3.SimulationLinkDatum<D3Node<N, E>>>([])
            .id(v => v.id)
            .strength(1)

    const simulation = (
        d3
            .forceSimulation([])
            .force('attract', forceAttract)
            .force('center', forceCenter)
            // Nodes attract each other
            .force('charge', d3.forceManyBody().strength(1))
            // Nodes should bump each other away
            // .force('collide', forceCollide)
            .force('link', forceLink)
    )

    const ref = useD3(

        // init
        svg => {

            const zoomed = (e: SVGSVGElement) => {
                svg
                    .select(`.${MAIN_GROUP}`)
                    .attr('transform', `${e.transform}`)
            }

            const zoom = d3.zoom<SVGSVGElement, Triple>()
                .scaleExtent([0.1, 3])
                .on('zoom', zoomed)

            const dw = svg.node().width.baseVal.value / 2
            const dh = svg.node().height.baseVal.value / 2

            // forceAttract.target(d => {
            //     return [dw, dh]
            // }).strength(0.10)

            svg
                .call(zoom)
                .call(zoom.transform, d3.zoomIdentity.translate(dw, dh))

        },

        // update
        svg => {

            const nodes: D3NodeOfNode<N>[] = props.nodes.map(n => ({ tag: 'node', ...n }))

            // For every edge, we create two links: one from the source to the
            // "edge-node", and one from the "edge-node" to the target.
            const links: D3Edge<N, E>[] = props.edges.flatMap(e => {

                const [firstLinkId, secondLinkId] = linkIds(e)

                return [
                    {
                        id: firstLinkId,
                        originalEdge: e,
                        source: { tag: 'node', ...e.source } as D3NodeOfNode<N>,
                        target: { tag: 'edge', ...e } as D3NodeOfEdge<N, E>,
                    },
                    {
                        id: secondLinkId,
                        originalEdge: e,
                        source: { tag: 'edge', ...e } as D3NodeOfEdge<N, E>,
                        target: { tag: 'node', ...e.target } as D3NodeOfNode<N>,
                    },
                ]

            })

            const edgesAsNodes: D3NodeOfEdge<N, E>[] = props.edges.map(e => ({ tag: 'edge', ...e }))

            const simulationNodes = ([] as D3Node<N, E>[]).concat(
                nodes,
                edgesAsNodes,
            )

            simulation.nodes(simulationNodes)

            forceLink.links(
                links.map(e => ({
                    source: e.source.id,
                    target: e.target.id,
                }))
            )

            /*
             * When dragged, nodes should follow the mouse, and reheat the
             * simulation.
             */
            const drag = (
                d3
                    .drag<SVGElement, D3NodeOfNode<N>>()
                    .subject(e => simulation.find(e.x, e.y))
                    .on('start', (e: d3.D3DragEvent<Element, D3Node<N, E>, D3Node<N, E>>) => {
                        if (!e.active) { simulation.alphaTarget(0.1).restart() }
                        e.subject.fx = e.subject.x
                        e.subject.fy = e.subject.y
                    })
                    .on('drag', (e: d3.D3DragEvent<Element, D3Node<N, E>, D3Node<N, E>>) => {
                        e.subject.fx = e.x
                        e.subject.fy = e.y
                    })
                    .on('end', (e: d3.D3DragEvent<Element, D3Node<N, E>, D3Node<N, E>>) => {
                        if (!e.active) simulation.alphaTarget(0)
                        // The following makes it so when nodes are released,
                        // they are again moving based to simulation.  If you
                        // comment these out, mouse-positioned nodes will no
                        // longer move.
                        e.subject.fx = null
                        e.subject.fy = null
                    })
            )

            const nodesSelection = svg
                .select(`.${NODE_LAYER}`)
                .selectAll<SVGElement, D3NodeOfNode<N> & d3.SimulationNodeDatum>(`.${NODE_CLASS}`)
                .data<D3NodeOfNode<N> & d3.SimulationNodeDatum>(nodes, d => d.id)
                .join(
                    enter => {
                        const groups = enter.append('g')
                            .classed(NODE_CLASS, true)
                            .attr('transform', d => `translate(${d.x},${d.y})`)
                            .each(d => {
                                setNodeDatum(d.id, {
                                    height: 0,
                                    longestLabel: 0,
                                    width: 0,
                                    x: 0,
                                    y: 0,
                                })
                            })
                            .selection()

                        const foreignObjectGroups =
                            groups.append('g')
                                .classed(FOREIGN_OBJECT_GROUP_CLASS, true)

                        const foreignObjects =
                            foreignObjectGroups.append('foreignObject')
                                .classed(FOREIGN_OBJECT_CLASS, true)
                        //         .attr('width', '100%')

                        const bodies =
                            foreignObjects.append('xhtml:body')
                                .style('word-wrap', 'break-word')
                                .style('margin', 0)
                                .style('height', '100%')
                                .style('width', '100%')

                        bodies.append('table')
                            .classed(NODE_TABLE_CLASS, true)
                            // .html(d => {
                            //     // const properties = d.properties
                            //     //     .map(p => `<tr><td>${p.property.name}</td></tr>`)
                            //     //     .join('\n')
                            //     return `
                            //         <tr style="text-align: center">
                            //             <td>${d.uri.ns.namespace}#${labelForURI(d.uri)}</td>
                            //         </tr>
                            //         ${props.showProperties ? 'YES' : 'NO'}
                            //     `
                            // })
                            // .style('-webkit-text-stroke', '1px black')
                            .style('background-color', d => d.color)
                            .style('border', '2px solid black')
                            .style('border-radius', '4px')
                            .style('font-family', 'sans-serif')
                            .style('font-weight', 'bold')
                            .style('color', d => contrastingColorFor(d.color))
                            .style('font-size', '16px')
                        // .style('opacity', '50%')

                        foreignObjectGroups.each((d, i, s) => {
                            const self = s[i] as SVGGElement
                            const table = self.getElementsByTagName('table')[0]
                            const rect = table.getBoundingClientRect()
                            // Because getBoundingClientRect is based on what's
                            // being currently displayed, the sizes of the
                            // tables are scaled.  We need to cancel it out.
                            const zoomFactor = d3.zoomTransform(table).scale(1)
                            nodeDatum(d.id).width = rect.width / zoomFactor.k
                            nodeDatum(d.id).height = rect.height / zoomFactor.k
                        })
                            .attr('transform', d => `translate(${- nodeDatum(d.id).width / 2}, ${- nodeDatum(d.id).height / 2})`)

                        foreignObjects
                            .attr('width', d => nodeDatum(d.id).width)
                            .attr('height', d => nodeDatum(d.id).height)

                        // forceCollide.initialize(simulation.nodes(), () => Math.random())
                        return groups
                    },
                    update => {
                        const groups = update
                            .attr('transform', d => `translate(${d.x},${d.y})`)

                        // forceCollide.initialize(simulation.nodes(), () => Math.random())

                        return groups
                    },
                    exit => exit.remove(),
                )
                .call(drag)

            const edgeNodesSelection = svg
                .select(`.${NODE_LAYER}`)
                .selectAll<SVGTextElement, D3NodeOfEdge<N, E> & d3.SimulationNodeDatum>(`.${EDGE_NODE_CLASS}`)
                .data<D3NodeOfEdge<N, E> & d3.SimulationNodeDatum>(edgesAsNodes, d => d.id)
                .join(
                    enter => enter
                        .append('text')
                        .classed(EDGE_NODE_CLASS, true)
                        .text(d => d.label)
                        .attr('fill', 'white')
                        .attr('filter', 'url(#outline)')
                        .attr('text-anchor', 'middle')
                    // .attr('r', '5')
                    // .attr('fill', 'green')
                    ,
                    update => update,
                    exit => exit.remove(),
                )

            const hullSelection =
                svg
                    .select(`.${EDGE_LAYER}`)
                    .selectAll('.hull')
                    .data([1])
                    // .datum(d3poly.polygonHull(nodes.map(d => {
                    //     console.log([nodeDatum(d.id).x, nodeDatum(d.id).y])
                    //     return [nodeDatum(d.id).x, nodeDatum(d.id).y]
                    // })))
                    .append('path')
                    .classed('hull', true)

            // const edgeLinksSelection =
            //     svg.select(`.${EDGE_LAYER}`)
            //         .selectAll<SVGPathElement, D3Edge>(`.${EDGE_LINK_CLASS}`)
            //         .data(links, d => d.id)
            //         .join(
            //             enter => enter
            //                 .append('path')
            //                 .classed(EDGE_LINK_CLASS, true)
            //                 .attr('stroke', 'pink')
            //             ,
            //             update => update,
            //             exit => exit.remove(),
            //         )

            const edgeLinksSelection =
                svg.select(`.${EDGE_LAYER}`)
                    .selectAll<SVGPathElement, E>(`.${EDGE_LINK_CLASS}`)
                    .data<E>(props.edges, d => d.id)
                    .join(
                        enter => enter
                            .append('path')
                            .classed(EDGE_LINK_CLASS, true)
                            .attr('fill', 'none')
                            .attr('id', d => d.id)
                            .style('stroke', d => d.color)
                            // .attr('stroke', 'black')
                            .attr('stroke-width', d => `${d.thickness}px`)
                            .attr('marker-mid', d => `url(#arrowhead-${d.color})`)
                        ,
                        update => update,
                        exit => exit.remove(),
                    )

            // const linksSelection =
            //     svg.select(`.${EDGE_LAYER}`)
            //         .selectAll<SVGLineElement, D3Edge>(`.${EDGE_CLASS}`)
            //         .data(edges, d => {
            //             // console.log(d, d.id)
            //             return d.id
            //         })
            //         .join(
            //             enter => enter
            //                 .append('path')
            //                 .classed(EDGE_CLASS, true)
            //                 .attr('fill', 'pink')
            //                 .attr('stroke', 'black')
            //                 .attr('stroke-width', '3px')
            //                 .attr('id', d => d.id)
            //                 .attr('marker-mid', 'url(#arrowhead)')
            //             ,
            //             update => update
            //             // .attr('pointyhat', d => {
            //             //     const x1 = nodeMutableDataRef.current[idForURI(d.source)].x
            //             //     const y1 = nodeMutableDataRef.current[idForURI(d.source)].y
            //             //     const x2 = nodeMutableDataRef.current[idForURI(d.target)].x
            //             //     const y2 = nodeMutableDataRef.current[idForURI(d.target)].y
            //             //     return `${x1},${y1} ${(x1 + x2) / 2},${(y1 + y2) / 2} ${x2},${y2}`
            //             // })
            //             // .attr('x1', d => (nodeMutableDataRef.current[idForURI(d.source)].x))
            //             // .attr('y1', d => (nodeMutableDataRef.current[idForURI(d.source)].y))
            //             // .attr('x2', d => (nodeMutableDataRef.current[idForURI(d.target)].x))
            //             // .attr('y2', d => (nodeMutableDataRef.current[idForURI(d.target)].y))
            //             ,
            //             exit => exit.remove(),
            //         )

            // const textsSelection =
            //     svg.select(`.${EDGE_LAYER}`)
            //         .selectAll<SVGTextElement, D3Edge>(`.${EDGE_LABEL_CLASS}`)
            //         .data(props.edges, d => d.id)
            //         .join(
            //             enter => {
            //                 const s = enter
            //                     // return enter
            //                     .append('text')
            //                     .classed(EDGE_LABEL_CLASS, true)
            //                     .attr('dy', '-10px')

            //                 s
            //                     .append('textPath')
            //                     .attr('startOffset', '50%')
            //                     .attr('text-anchor', 'middle')
            //                     .attr('xlink:href', d => `#${d.id}`)
            //                     .text(d => d.uri.name)

            //                 return s
            //             },
            //             update => update,
            //             exit => exit.remove(),
            //         )

            simulation.on('tick', () => {

                forceAttract.target((d: D3Node<N, E>) => {

                    if (isD3NodeOfNode(d)) {
                        return [
                            svg.node().width.baseVal.value / 2,
                            svg.node().height.baseVal.value / 2,
                        ]
                    }

                    // Edges are attracted to the midpoint between their endpoints
                    if (isD3NodeOfEdge(d)) {
                        const source = nodeDatum(d.source.id)
                        const target = nodeDatum(d.target.id)
                        return [(source.x + target.x) / 2, (source.y + target.y) / 2]
                    }

                    console.log(d)
                    throw new Error('forceAttract: weird node')
                })

                hullSelection
                    .datum(d3poly.polygonHull(nodes.map(d => {
                        // console.log([nodeDatum(d.id).x, nodeDatum(d.id).y])
                        return [nodeDatum(d.id).x, nodeDatum(d.id).y]
                    })))
                    .attr('d', d => `M ${d.join('L')} Z`)

                nodesSelection
                    .attr('transform', d => `translate(${d.x}, ${d.y})`)
                    .each(d => {
                        nodeDatum(d.id).x = d.x
                        nodeDatum(d.id).y = d.y
                    })

                const foreignObjects =
                    nodesSelection
                        .selectAll<SVGForeignObjectElement, D3NodeOfNode<N>>(`.${FOREIGN_OBJECT_CLASS}`)
                        .data<D3NodeOfNode<N>>(nodes, d => d.id)
                        .attr('width', '100%')

                nodesSelection
                    .selectAll<HTMLTableElement, D3NodeOfNode<N>>(`.${NODE_TABLE_CLASS}`)
                    .data<D3NodeOfNode<N>>(nodes, d => d.id)
                    .html(d => props.renderNode(props, d))

                nodesSelection
                    .selectAll<SVGGElement, D3NodeOfNode<N>>(`.${FOREIGN_OBJECT_GROUP_CLASS}`)
                    .data<D3NodeOfNode<N>>(nodes, d => d.id)
                    .each((d, i, s) => {
                        const self = s[i] as SVGGElement
                        const table = self.getElementsByTagName('table')[0]
                        const rect = table.getBoundingClientRect()
                        // Because getBoundingClientRect is based on what's
                        // being currently displayed, the sizes of the
                        // tables are scaled.  We need to cancel it out.
                        const zoomFactor = d3.zoomTransform(table).scale(1)
                        nodeDatum(d.id).width = rect.width / zoomFactor.k
                        nodeDatum(d.id).height = rect.height / zoomFactor.k
                    })
                    .attr('transform', d => `translate(${- nodeDatum(d.id).width / 2}, ${- nodeDatum(d.id).height / 2})`)

                foreignObjects
                    .attr('width', d => nodeDatum(d.id).width)
                    .attr('height', d => nodeDatum(d.id).height)

                edgeNodesSelection
                    .attr('x', d => d.x)
                    .attr('y', d => d.y - 12) // little offset makes arrowhead more visible
                    .each((d, i, s) => {
                        const self = s[i]
                        const rect = self.getBoundingClientRect()
                        const datum = nodeOfEdgeDatum(d.id)
                        datum.x = d.x
                        datum.y = d.y
                        // TODO: might need to scale these by zoom factor
                        datum.width = rect.width
                        datum.height = rect.height
                    })

                // function xOfD3Node(n: D3Node): number {
                //     return (isD3NodeOfNode(n)) ? nodeDatum(n.id).x : nodeOfEdgeDatum(n.id).x
                // }

                // function yOfD3Node(n: D3Node): number {
                //     return isD3NodeOfNode(n) ? nodeDatum(n.id).y : nodeOfEdgeDatum(n.id).y
                // }

                edgeLinksSelection
                    .attr('d', (d) => {


                        // const link = d3.linkHorizontal<unknown, Edge, Node>()
                        //     .x(d => nodeDatum(d.id).x)
                        //     .y(d => nodeDatum(d.id).y)

                        // return link(d)

                        const source = nodeDatum(d.source.id)
                        const edge = nodeOfEdgeDatum(d.id)
                        const target = nodeDatum(d.target.id)

                        const controlPoint = (start, end, middle) => {
                            return {
                                x: 2 * middle.x - (start.x + end.x) / 2,
                                y: 2 * middle.y - (start.y + end.y) / 2,
                            }
                        }

                        const midPoint = (start, end) => {
                            return {
                                x: (start.x + end.x) / 2,
                                y: (start.y + end.y) / 2,
                            }
                        }

                        // const firstControlPoint = controlPoint(source, edge, midPoint(source, edge))
                        // const secondControlPoint = controlPoint(edge, target, midPoint(edge, target))

                        // return `M${source.x},${source.y} S${edge.x},${edge.y} ${target.x},${target.y}`

                        // const controlPointX = 2 * edge.x - (source.x + target.x) / 2
                        // const controlPointY = 2 * edge.y - (source.y + target.y) / 2

                        // const x1 = xOfD3Node(d.source)
                        // const y1 = yOfD3Node(d.source)
                        // const x2 = xOfD3Node(d.target)
                        // const y2 = yOfD3Node(d.target)

                        const initialControlPoint = controlPoint(source, target, edge)

                        const firstControlPoint = midPoint(source, initialControlPoint)
                        const secondControlPoint = midPoint(initialControlPoint, target)

                        return `M ${source.x},${source.y} Q ${firstControlPoint.x},${firstControlPoint.y} ${edge.x},${edge.y} Q ${secondControlPoint.x},${secondControlPoint.y} ${target.x},${target.y}`

                    })

                // linksSelection
                //     .attr('d', d => {
                //         const x1 = nodeMutableDataRef.current[d.source.id].x
                //         const y1 = nodeMutableDataRef.current[d.source.id].y
                //         const x2 = nodeMutableDataRef.current[d.target.id].x
                //         const y2 = nodeMutableDataRef.current[d.target.id].y
                //         return `M${x1},${y1} L${(x1 + x2) / 2},${(y1 + y2) / 2} L${x2},${y2}`
                //     })

                // textsSelection
                //     .each((d, i, s) => {
                //         const self = s[i] as SVGTextElement
                //         const rect = self.getBoundingClientRect()
                //         const datum = nodeOfEdgeDatum(d.id)
                //         // TODO: might need to scale by zoom factor
                //         datum.width = rect.width
                //         datum.height = rect.height
                //         datum.x = rect.left + rect.width / 2
                //         datum.y = rect.top + rect.height / 2
                //     })

                // textsSelection
                //     .each((d, i, s) => {
                //         const self = s[i] as SVGTextElement
                //         const source = nodeMutableDataRef.current[d.source.id]
                //         const target = nodeMutableDataRef.current[d.target.id]
                //         // TODO: we might never go down to smaller sizes if a very large label disappears
                //         source.longestLabel = Math.max(self.getComputedTextLength(), source.longestLabel)
                //         target.longestLabel = Math.max(self.getComputedTextLength(), target.longestLabel)
                //         // edgeMutableDataRef.current[d.id].textWidth = Math.round(rect.width)
                //         // edgeMutableDataRef.current[d.id].textHeight = Math.round(rect.height)
                //         edgeMutableDataRef.current[d.id].textLength = Math.round(self.getComputedTextLength())
                //         // Math.sqrt(Math.pow(rect.width, 2) + Math.pow(rect.height, 2))
                //     })
                //     .text(d => {
                //         const datum = edgeMutableDataRef.current[d.id]
                //         return `${d.id} (${datum.textLength})`
                //         // return `${d.uri.name} (${datum.textWidth}x${datum.textHeight})`
                //     })
                // .attr('points', d => {
                //     const x1 = nodeMutableDataRef.current[idForURI(d.source)].x
                //     const y1 = nodeMutableDataRef.current[idForURI(d.source)].y
                //     const x2 = nodeMutableDataRef.current[idForURI(d.target)].x
                //     const y2 = nodeMutableDataRef.current[idForURI(d.target)].y
                //     return `${x1},${y1} ${(x1 + x2) / 2},${(y1 + y2) / 2} ${x2},${y2}`
                // })
                // .attr('x1', d => (nodeMutableDataRef.current[idForURI(d.source)].x))
                // .attr('y1', d => (nodeMutableDataRef.current[idForURI(d.source)].y))
                // .attr('x2', d => (nodeMutableDataRef.current[idForURI(d.target)].x))
                // .attr('y2', d => (nodeMutableDataRef.current[idForURI(d.target)].y))

                simulation.force(
                    'collide',
                    d3bc.bboxCollide((d: D3Node<N, E>) => {

                        if (isD3NodeOfNode(d)) {
                            const datum = nodeDatum(d.id)
                            const spacing = NODE_SPACING / 2
                            // const spacing = datum.longestLabel / 2
                            return [
                                [-datum.width / 2 - spacing, -datum.height / 2 - spacing],
                                [datum.width / 2 + spacing, datum.height / 2 + spacing],
                            ]
                        }

                        if (isD3NodeOfEdge(d)) {
                            const datum = nodeOfEdgeDatum(d.id)
                            const spacing = EDGE_SPACING / 2
                            return [
                                [-datum.width / 2 - spacing, -datum.height / 2 - spacing],
                                [datum.width / 2 + spacing, datum.height / 2 + spacing],
                            ]
                            // return [
                            //     [-spacing, -spacing],
                            //     [spacing, spacing],
                            // ]
                        }

                        console.log(d)
                        throw new Error('Node was neither NodeOfNode nor NodeOfEdge')

                    })
                        .strength(1)
                        .iterations(2)
                )

            })

        },
        [props.nodes],
    )

    // Unfortunately, SVG does not let you override marker attributes a
    // posteriori.  So we need one marker per color!
    const markers =
        colors.map((color, i) =>
            <marker key={i} id={`arrowhead-${color}`} orient="auto" markerWidth="20" markerHeight="40" refX="2" refY="2.5">
                <polygon points="1,1 1,4 5,2.5" fill={color} stroke="black" strokeWidth="0.5px" />
                {/* <path d="M0,0 V6 L4,3 Z" fill='black' />
                <path d="M1,1 V4 L3,2 Z" fill={color} /> */}
            </marker>
        )

    return (
        <svg
            ref={ref}
            style={{
                height: '100%',
                width: '100%',
            }}
        >

            <defs>
                <pattern id="smallGrid" width="8" height="8" patternUnits="userSpaceOnUse">
                    <path d="M 8 0 L 0 0 0 8" fill="none" stroke="gray" strokeWidth="0.5" />
                </pattern>
                <pattern id="grid" width="80" height="80" patternUnits="userSpaceOnUse">
                    <rect width="80" height="80" fill="url(#smallGrid)" />
                    <path d="M 80 0 L 0 0 0 80" fill="none" stroke="gray" strokeWidth="1" />
                </pattern>
                {markers}
            </defs>

            <filter id="outline">
                <feMorphology in="SourceAlpha" result="DILATED" operator="dilate" radius="1"></feMorphology>

                <feFlood floodColor="black" floodOpacity="1" result="PINK"></feFlood>
                <feComposite in="PINK" in2="DILATED" operator="in" result="OUTLINE"></feComposite>

                <feMerge>
                    <feMergeNode in="OUTLINE" />
                    <feMergeNode in="SourceGraphic" />
                </feMerge>
            </filter>

            <g className={MAIN_GROUP}>

                {/* Debug: show the origin */}
                {/* <circle cx={0} cy={0} r={10} fill="blue" /> */}

                {/* Debug: show a grid */}
                {/* <rect x="-100%" y="-100%" width="200%" height="200%" fill="url(#grid)" /> */}

                <g className={EDGE_LAYER} />
                <g className={NODE_LAYER} />
            </g>

        </svg>
    )

}
