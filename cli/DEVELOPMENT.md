# Hacking

The following documentation is only useful for developers of this script.

## SemTK API Documentation

Documentation on the REST API is available on the
[REST API Swagger Demo](https://github.com/ge-high-assurance/RACK/wiki/REST-API-Swagger-Demo)
and [REST cookbook](https://github.com/ge-semtk/semtk/wiki/REST-cookbook) wiki pages.

## Mypy

This script has [Mypy](http://mypy-lang.org)-compliant type annotations which
are used to statically type-check the code. Usage is simple:

```shell
source venv/bin/activate
pip install ".[dev]"
mypy rack
```

## Tests

You can run the tests with `pytest`:

```shell
source venv/bin/activate
pip install ".[dev]"
pytest
```

Note that you should _always install the code before testing_.

If you see an error message like this:

```text
Couldn't connect to Docker daemon at http+docker://localhost - is it running?
```

the pytest process likely doesn't have enough privileges to run the RACK-in-a-box Docker container. On most Linux distros, you can use the following:

```shell
sudo -g docker pytest
```

---
Copyright (c) 2020, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
