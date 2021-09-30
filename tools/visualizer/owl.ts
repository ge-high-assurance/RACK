import { RDF_SCHEMA_SUBCLASSOF } from './rdf-schema'
import { equalsURI, Triple } from './triple'
import { URI } from './uri'

const OWL_DOMAIN = 'http://www.w3.org/2002/07'
const OWL_NAMESPACE = 'owl'

export const OWL_CLASS: URI = {
    ns: {
        domain: OWL_DOMAIN,
        namespace: OWL_NAMESPACE,
    },
    name: 'Class',
}

export const OWL_DATATYPE_PROPERTY: URI = {
    ns: {
        domain: OWL_DOMAIN,
        namespace: OWL_NAMESPACE,
    },
    name: 'DatatypeProperty',
}

export const OWL_FUNCTIONAL_PROPERTY: URI = {
    ns: {
        domain: OWL_DOMAIN,
        namespace: OWL_NAMESPACE,
    },
    name: 'FunctionalProperty',
}

export const OWL_OBJECT_PROPERTY: URI = {
    ns: {
    domain: OWL_DOMAIN,
    namespace: OWL_NAMESPACE,
    },
    name: 'ObjectProperty',
}


/**
 * OWL restrictions are actually expressed as rdf-schema's `subClassOf`
 * relationships whose codomain is an anonymous class expressing the
 * restriction.  They are not very useful in this visualization.
 * @param t - Triple
 */
export function isOWLRestriction(t: Triple): boolean {
    return (
        equalsURI(t.property, RDF_SCHEMA_SUBCLASSOF)
        &&
        t.target.ns.domain.startsWith('_:file:///')
    )
}


export function isOWLProperty(t: Triple): boolean {
    return (
        equalsURI(t.target, OWL_DATATYPE_PROPERTY)
        || equalsURI(t.target, OWL_FUNCTIONAL_PROPERTY)
        || equalsURI(t.target, OWL_OBJECT_PROPERTY)
    )
}
