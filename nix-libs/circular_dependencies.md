# A note on how we solve circular dependencies

Npm supports circular dependencies, while nix does not. A package *P*
has a circular dependency *D* if and only if *P* appears in the dependency
closure of *D*. (By *dependency closure* we mean "a package's
dependencies, and all of its dependencies' dependencies, etc") Imagine
the following situation ("->" means "depends on"):

```
  A -> {B}, B -> {A, C}, C -> {D, E, F}
```

In this case we have circularity between *A* and *B*. When building *A*,
we can't build *B* the normal way, because we'd have to first build
*A*, and we'd get infinite recursion. However, we can put the
package *B*, and all of its dependencies, in the `node_modules`
folder for *A*, and then *A* will be able to use *B* and vice versa when
*A* is a dependency of some library. This means to build *A* we need to:

1. Build *C*, *D*, *E*, and *F* the normal way.
2. Symlink *C*, *D*, *E* and *F* into *A*'s `node_modules` folder.
3. Extract the source of *B* into the `node_modules` folder.
4. Make a self-referential symlink of *A* into its own `node_modules` folder.

The last step is a bit strange but since *B* needs to be able to import
*A*, *A* needs to be in the same `node_modules` folder as *B*. We can
accomplish this with a symlink.

Now, how about building *B*? *A* is a circular dependency of *B*, but *A*
doesn't have any other dependencies. This means that the process
is simply

1. Extract the source of *A* into the `node_modules` folder.
2. Symlink *B* into the `node_modules` folder.

Now let's imagine a slightly more complicated situation involving
a three-way circularity.

```
  A -> {B, C}, B -> {C, D, E}, C -> {A, D}
```

In this case the only packages we can build normally are *D* and
*E*. Then to build *A*, we build *E* and *D* as normal and symlink them
into `node_modules`, extract *B* and *C*'s source into `node_modules`, and
make a reflexive symink.

Finally, a double-circular dependency:

```
  A -> {B}, B -> {C, A}, C -> {B}
```

So the more general algorithm to build a package *A* which has one
or more circular dependencies is:

1. Compute the full set of circular dependencies of *A*.
2. Compute the full set of packages that those dependencies depend
   on at runtime, and symlink all of these into `node_modules`.
3. Extract the source of each circular dependency into `node_modules`.
4. Make a self-referential symlink of *A*.
