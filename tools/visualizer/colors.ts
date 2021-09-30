import { isSameNamespaceAs, Namespace, URI } from './uri'

// from https://sashamaps.net/docs/resources/20-colors/
export const colors: string[] = [
    '#E6194B',
    '#3CB44B',
    '#FFE119',
    '#4363D8',
    '#F58231',
    '#42D4F4',
    '#F032E6',
    '#FABED4',
    '#469990',
    '#DCBEFF',
    '#9A6324',
    // '#FFFAC8', // too close to white
    '#800000',
    '#AAFFC3',
    '#000075',
    '#A9A9A9',
    // '#FFFFFF', // nope
    '#000000',
]


export function colorForNamespace(sortedNamespaces: Namespace[], namespace: Namespace): string {
    const index = sortedNamespaces.findIndex(isSameNamespaceAs(namespace))
    return colors[index % colors.length]
}


export function colorFor(sortedNamespaces: Namespace[], uri: URI): string {
    return colorForNamespace(sortedNamespaces, uri.ns)
}


export function contrastingColorFor(color: string): string {
    switch (color) {
        case '#000000':
        case '#000075':
        case '#4363D8':
        case '#469990':
        case '#800000':
        case '#9A6324':
        case '#E6194B':
            return 'white'

        default:
            return 'black'
    }
}
