The files in this directory have been automatically generated via the
write_ontology/0 predicate, from the rack(write_ontology) module.

If you want to refresh the files, you can run `swipl` from the `RACK-Ontology`
directory, and then input:

```prolog
?- [bin/rack/write_ontology].
true.

?- rack:load_local_model('./OwlModels').
... lots of imports, supposedly ...

?- write_ontology().
true.
```

Existing files *will* be overwritten.  Files that no longer have a model
counterpart *will not* be deleted.
