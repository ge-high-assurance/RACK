The scripts in this directory rely on the prolog-graphviz library.  As we do not
currently have a way of specifying those dependencies programatically, you will
need to run the following command within swi-prolog:

```prolog
?- pack_install(prolog_graphviz).
```

and answer a few questions (it should install locally in
~/.local/share/swi-prolog/...).
