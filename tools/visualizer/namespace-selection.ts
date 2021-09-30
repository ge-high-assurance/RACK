import { isInRACKNamespace } from './rack'
import { Triple } from './triple'
import { URI } from './uri'


export function isURIInNamespaceSelection(namespaceSelection: Set<string>): (u: URI) => boolean {
    return u => {
        return Array.from(namespaceSelection).some(ns => isInRACKNamespace(ns)(u))
    }
}


export function isTripleInNamespaceSelection(namespaceSelection: Set<string>): (t: Triple) => boolean {
    return t => {
        return Array.from(namespaceSelection).some(ns =>
            isInRACKNamespace(ns)(t.source) || isInRACKNamespace(ns)(t.target)
        )
    }
}
