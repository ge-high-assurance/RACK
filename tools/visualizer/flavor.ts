export interface Flavored<F> {
    _flavor?: F
}

export type Flavor<T, F> = T & Flavored<F>
