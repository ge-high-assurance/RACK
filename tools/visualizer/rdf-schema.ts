import { equalsURI } from './triple'
import { URI } from './uri'

const RDF_SCHEMA_URL_DOMAIN = 'http://www.w3.org/2000/01'
const RDF_SCHEMA_NAMESPACE = 'rdf-schema'

export const RDF_SCHEMA_COMMENT: URI = {
    ns: {
        domain: RDF_SCHEMA_URL_DOMAIN,
        namespace: RDF_SCHEMA_NAMESPACE,
    },
    name: 'comment',
}

export const RDF_SCHEMA_DOMAIN: URI = {
    ns: {
        domain: RDF_SCHEMA_URL_DOMAIN,
        namespace: RDF_SCHEMA_NAMESPACE,
    },
    name: 'domain',
}

export const RDF_SCHEMA_RANGE: URI = {
    ns: {
        domain: RDF_SCHEMA_URL_DOMAIN,
        namespace: RDF_SCHEMA_NAMESPACE,
    },
    name: 'range',
}

export const RDF_SCHEMA_SUBCLASSOF: URI = {
    ns: {
        domain: RDF_SCHEMA_URL_DOMAIN,
        namespace: RDF_SCHEMA_NAMESPACE,
    },
    name: 'subClassOf',
}


export function isRDFSchemaComment(u: URI): boolean {
    return equalsURI(u, RDF_SCHEMA_COMMENT)
}
