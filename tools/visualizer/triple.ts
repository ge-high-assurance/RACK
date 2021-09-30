import { idForURI, parseURI, URI } from './uri'

export type Triple = {
    source: URI
    property: URI
    target: URI
}

type JSONTriple = {
    source: string
    property: string
    target: string
}

export function parseTriple(t: JSONTriple): Triple {
    return {
        source: parseURI(t.source),
        property: parseURI(t.property),
        target: parseURI(t.target),
    }
}


export function equalsURI(a: URI, b: URI): boolean {
    return idForURI(a) === idForURI(b)
}


export function mentionsAnonymousClass(t: Triple): boolean {
    return (
        t.source.ns.domain.startsWith('_:file:///')
        || t.target.ns.domain.startsWith('_:file:///')
    )
}


export function tripleId(t: Triple): string {
    return `${idForURI(t.source)}-${t.property}-${idForURI(t.target)}`
}
