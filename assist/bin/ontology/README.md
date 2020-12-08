The files in this directory have been automatically generated via the
write_ontology/0 predicate, from the rack(write_ontology) module.

If you want to refresh the files, you can run `swipl` from the `RACK`
directory, and then input:

```prolog
?- [assist/bin/rack/write_ontology].
true.

?- rack:load_local_model('./RACK-Ontology/OwlModels').
... lots of imports, supposedly ...

?- write_ontology().
true.
```

Existing files *will* be overwritten.  Files that no longer have a model
counterpart *will not* be deleted.

---
Copyright (c) 2020, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
