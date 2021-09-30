import * as d3 from 'd3'
import { BaseType } from 'd3-selection'
import * as React from 'react'

import { Triple } from './triple'


type UseD3<D> = (
    initFn: (s: d3.Selection<SVGSVGElement, D, BaseType, unknown>) => void,
    renderChartFn: (s: d3.Selection<SVGSVGElement, D, BaseType, unknown>) => void,
    dependencies: unknown[],
) => React.Ref<SVGSVGElement>


export const useD3: UseD3<Triple> = (initFn, renderChartFn, dependencies) => {
    const ref = React.createRef<SVGSVGElement>()

    React.useEffect(
        () => { initFn(d3.select(ref.current)) },
        [],
    )

    React.useEffect(
        () => {
            renderChartFn(d3.select(ref.current))
            return () => { return }
        },
        dependencies,
    )

    return ref
}
