import { RACK_DOMAIN } from './rack'
import { isSameNamespaceAs, Namespace, URI } from './uri'

const MIL_STD_881D_DOMAIN = `${RACK_DOMAIN}/MIL-STD-881D`
const MIL_STD_881D_NAMESPACE_NAME = 'MIL-STD-881D'

export const MIL_STD_881D_NAMESPACE: Namespace = {
    domain: MIL_STD_881D_DOMAIN,
    namespace: MIL_STD_881D_NAMESPACE_NAME,
}

export const MIL_STD_881D_APPXA_NAMESPACE: Namespace = {
    domain: MIL_STD_881D_DOMAIN,
    namespace: `${MIL_STD_881D_NAMESPACE_NAME}-AppxA`,
}

export const MIL_STD_881D_APPXB_NAMESPACE: Namespace = {
    domain: MIL_STD_881D_DOMAIN,
    namespace: `${MIL_STD_881D_NAMESPACE_NAME}-AppxB`,
}

export const MIL_STD_881D_APPXC_NAMESPACE: Namespace = {
    domain: MIL_STD_881D_DOMAIN,
    namespace: `${MIL_STD_881D_NAMESPACE_NAME}-AppxC`,
}

export const MIL_STD_881D_APPXD_NAMESPACE: Namespace = {
    domain: MIL_STD_881D_DOMAIN,
    namespace: `${MIL_STD_881D_NAMESPACE_NAME}-AppxD`,
}

export function isMILSTDURI(u: URI): boolean {
    return [
        isSameNamespaceAs(MIL_STD_881D_NAMESPACE)(u.ns),
        isSameNamespaceAs(MIL_STD_881D_APPXA_NAMESPACE)(u.ns),
        isSameNamespaceAs(MIL_STD_881D_APPXB_NAMESPACE)(u.ns),
        isSameNamespaceAs(MIL_STD_881D_APPXC_NAMESPACE)(u.ns),
        isSameNamespaceAs(MIL_STD_881D_APPXD_NAMESPACE)(u.ns),
    ].some(Boolean)
}
