import * as React from 'react'
import * as ReactDOM from 'react-dom'

import { Visualizer } from './visualizer'


ReactDOM.render(
    React.createElement(
        Visualizer,
        {}
    ),
    document.getElementById('container'),
)
