wordsnake
=========

Solves the &quot;wordsnake&quot; puzzle.

Brute-force solves the "wordsnake" problem. Wordsnakes are the different ways
one can combine words together using as few letters as possible.  For example,
"terrible" and "english" have two (ordered) wordsnakes: "terribleenglish" and
"terriblenglish".  For the wordsnakes problem, we take a list of words and
compute all the smallest ways to combine them.

This is in essence a travelling salesman problem, which is ultimately a problem in permutations.
In this program we consider all permutations of the list of words, and fold over the permutation
a merge operation that finds the shortest wordsnake for each adjacent pair of words, so that we end
up with the smallest wordsnake for each permutation.  When then iterate over all permutations and
remember the best paths.

One interesting aspect of this program from a scala point of view, is how it uses a lazy iterator over
the permutations, and a fold operation (instead of foldLeft).  The fold works over a parallel collection,
so we pull off some number of permutations from the iterator, and then do the parallel fold over them.
It took some experimentation to find the right value for "pageSize", the number to pull of the iterator
in one go.

This program runs fastest without about a 500M heap, though will run fine (slightly slower) with a much
smaller heap; I ran it with a 5M heap to completion (though much slower than with a bigger heap).

$ scala -cp target/scala-2.9.1/classes -J-Xms5m -J-Xmx5m -J-server Main

This idea was taken from _Puzzles for Programmers and Pros_ by Dennis E. Sasha, Wiley Publishing, Indianapolis,
(c) pp.171-173


