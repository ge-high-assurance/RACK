import { equalsURI, Triple } from './triple'
import { URI } from './uri'

const RDF_DOMAIN = 'http://www.w3.org/1999/02'
const RDF_NAMESPACE = '22-rdf-syntax-ns'

export const RDF_TYPE: URI = {
    ns: {
        domain: RDF_DOMAIN,
        namespace: RDF_NAMESPACE,
    },
    name: 'type',
}


export function isRDFTypeTriple(t: Triple): boolean {
    return equalsURI(t.property, RDF_TYPE)
}
