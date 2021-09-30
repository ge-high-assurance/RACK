import { match } from 'ts-pattern'


export type InstanceViewType =
    | InstanceViewBySource
    | InstanceViewByProperty
    | InstanceViewByTarget

type InstanceViewBySource = { type: 'InstanceViewBySource' }
export const InstanceViewBySource = instanceView({ type: 'InstanceViewBySource' })
type InstanceViewByProperty = { type: 'InstanceViewByProperty' }
export const InstanceViewByProperty = instanceView({ type: 'InstanceViewByProperty' })
type InstanceViewByTarget = { type: 'InstanceViewByTarget' }
export const InstanceViewByTarget = instanceView({ type: 'InstanceViewByTarget' })


export type ViewType =
    | OntologyView
    | { type: 'InstanceView', instanceViewType: InstanceViewType }

type OntologyView = { type: 'OntologyView' }
export const OntologyView: ViewType & OntologyView = { type: 'OntologyView' }

function instanceView(t: InstanceViewType): ViewType & { type: 'InstanceView' } {
    return { type: 'InstanceView', instanceViewType: t }
}


export function toString(v: ViewType): string {
    return match(v)
        .with({ type: 'OntologyView' }, () => OntologyView.type)
        .with(
            { type: 'InstanceView', instanceViewType: { type: 'InstanceViewBySource' } },
            () => InstanceViewBySource.instanceViewType.type,
        )
        .with(
            { type: 'InstanceView', instanceViewType: { type: 'InstanceViewByProperty' } },
            () => InstanceViewByProperty.instanceViewType.type,
        )
        .with(
            { type: 'InstanceView', instanceViewType: { type: 'InstanceViewByTarget' } },
            () => InstanceViewByTarget.instanceViewType.type,
        )
        .exhaustive()
}


export function fromString(v: string): ViewType {
    return match(v)
        .with(OntologyView.type, () => OntologyView)
        .with(InstanceViewBySource.instanceViewType.type, () => InstanceViewBySource)
        .with(InstanceViewByProperty.instanceViewType.type, () => InstanceViewByProperty)
        .with(InstanceViewByTarget.instanceViewType.type, () => InstanceViewByTarget)
        .otherwise(() => { throw new Error('viewTypeToString') })
}
