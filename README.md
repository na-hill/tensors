# tensors

Home of some related experiments:

1. Numeric tensor products

Implement summation (Einstein or otherwise) over arbitrary tensor products.  Single-thread performance is currently the goal.

2. Functional tensor products

Consider the natural definition of the operator 'x1:: ((a->c)->d) -> (b->c) -> (a->b) -> d'

This operator can be thougnt of as taking the product of two things, each looking vaguely similar to "($x)", and producing a result which also does.  Building from it, we can do familiar operations such as map and reduce over a product space, with similar ease as 'Applicative []', but without the flattening.

Using this type of structure, we can write nested "list comprehensions" using a functional pipeline.  Instead of 'map', do whatever - it doesn't even need to be expressible in terms of 'fmap'.

3. Exofunctor and friends

Relax the definitions of the usual typeclasses to accomodate containers that have constraints on their contents.  Also contains a generalization of `ZipList`.

Originally created to try to reduce the friction involved in working with 'Data.Vector.Unboxed'.
