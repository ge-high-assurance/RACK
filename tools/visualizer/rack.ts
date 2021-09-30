import { OWL_CLASS } from './owl'
import { isRDFTypeTriple, RDF_TYPE } from './rdf'
import { equalsURI, Triple } from './triple'
import { idForNamespace, URI } from './uri'

export const RACK_DOMAIN = 'http://arcos.rack'


export function isInRACKNamespace(namespace: string): (u: URI) => boolean {
    return u => idForNamespace(u.ns) === namespace
}


export function hasRACKOntologyClass(t: Triple): boolean {
    return (isRDFTypeTriple(t) && equalsURI(t.target, OWL_CLASS))
}


export function isRACKOntologyClass(u: URI): boolean {
    return u.ns.domain === RACK_DOMAIN
}


export function pointsToRACKOntologyClass(t: Triple): boolean {
    return isRACKOntologyClass(t.source) || isRACKOntologyClass(t.target)
}


export function isRACKOntologyClassDeclaration(t: Triple): boolean {
    return (
        t.source.ns.domain === RACK_DOMAIN
        && equalsURI(t.property, RDF_TYPE)
        && equalsURI(t.target, OWL_CLASS)
    )
}


export function isRACKOntologyTriple(t: Triple): boolean {
    return (
        t.source.ns.domain === RACK_DOMAIN
        && (t.target.value === undefined)
    )
}
