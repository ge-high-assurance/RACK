The scripts in this directory rely on the prolog-graphviz library.  As we do not
currently have a way of specifying those dependencies programatically, you will
need to run the following command within swi-prolog:

```prolog
?- pack_install(prolog_graphviz).
```

and answer a few questions (it should install locally in
~/.local/share/swi-prolog/...).

---
Copyright (c) 2020, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).
