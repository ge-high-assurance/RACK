
export type Namespace = {
    domain: string
    namespace: string
}

export type URI = {
    ns: Namespace
    name: string
    value?: string
}


export function idForNamespace(n: Namespace): string {
    return `${n.domain}/${n.namespace}`
}


export function idForURI(u: URI): string {
   const s = `${u.ns.domain}/${u.ns.namespace}#${u.name}`
    if (u.value) {
        return `${u.value}^^${s}`
    }
    return s
}


export function isSameNamespaceAs(n: Namespace): (o: Namespace) => boolean {
    return o => (n.domain === o.domain && n.namespace === o.namespace)
}


export function parseURI(s: string): URI {
    const { value, rest } = extractValue(s)
    const lastSlashIndex = rest.lastIndexOf('/')
    const domain = rest.slice(0, lastSlashIndex)
    const qualifiedName = rest.slice(lastSlashIndex + 1)
    const sharpIndex = qualifiedName.indexOf('#')

    // Sometimes it's just a domain...
    if (sharpIndex === -1) {
        return {
            name: '',
            ns: {
                domain,
                namespace: '',
            },
            value,
        }
    }

    const namespace = qualifiedName.slice(0, sharpIndex)
    const name = qualifiedName.slice(sharpIndex + 1)
    if (namespace === 'DevelopmentPlanDat') {
        console.log(qualifiedName, namespace, name, s)
    }
    return {
        name,
        ns: {
            domain,
            namespace,
        },
        value,
    }
}


function extractValue(s: string): { value?: string, rest: string } {
    const doubleCaretIndex = s.indexOf('^^')
    if (doubleCaretIndex < 0) { return { rest: s } }
    return {
        value: s.slice(0, doubleCaretIndex),
        rest: s.slice(doubleCaretIndex + '^^'.length + 1),
    }
}
