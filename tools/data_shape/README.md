# Data Visualization Experiment

This script is an experiment intended to inform what visualizations we
want to generate to support the ARCOS teams in understanding what data
is available in RACK.

## Building

```
python3 -m venv venv
. venv/bin/activate
./setup.py develop
```

## Example use

```
. venv/bin/activate
bin/data_shape --graph http://rack001/data --graph http://rack001/gtdata
```
