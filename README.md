# Half-planes Intersection
#### (Computational Geometry project)

### 1. The problem and the solution

The program computes an intersection for a given set of half-planes.
The solution will always be a convex polygon. One can see, that such
a problem is equivalent to linear programming with two variables, which
is obviously very basic but still might appear handy on occasions.

The algorithm is going to follow divide-and-conquer approach. It will split
the input set into two parts, find solutions for the parts and merge by
computing an intersection for two resulting convex polygon (the Intersection)
will be a convex polygon as well. For implementation of this outline look
at the function [`halfPlanesIntersection` in `src/main/scala/alg.scala` (line 101)](/jasiekmarc/go-hpl/src/master/src/main/scala/alg.scala#cl-101).

Intersecting two polygons is done using a linear-time sweep-line algorithm.
The sweep-line is scanning the left and right borders of two given polygons
separately and constructing the intersection, when the left border, which is
most to the right among the two is left to the right border more to the left.
There are few kinds of possible events (see function
[`SweepLine.nextPI` (same file, line 163)](/jasiekmarc/go-hpl/src/master/src/main/scala/alg.scala#cl-101)).

* _CrossL_ or _CrossR_ happens when the left (or right) borders of two
  given polygons cross (so their order in x-coordinate changes).
* _CrossLR_ is when left and right border cross, which means that the
  resulting polygon is just opening or closing (that depends on the order).
* _EdgeChangeL_ and _EdgeChangeR_ happen, when the sweep-line meets
  a vertex on one of the four scanned borders.

As one can imagine, the devil is in detail. Here dealing with open polygons
(which are allowed) generates some trouble. There is also an issue of
horizontal edges, which I decided to ignore.

Having implemented the sweep-line, it is easy to compute an intersection of
two given convex polygons (either open or not). We simply need to lower
the line finding next _points of interest_ till we are done. This is done
in function [`intersectPolygons` (same file, line 118)](/jasiekmarc/go-hpl/src/master/src/main/scala/alg.scala#cl-118)).

An overall running time of the algorithm is _Θ(n lg n)_.


### 2. Compiling and Usage

In order to compile the program, one requires a __Scala__ compiler as well as
__sbt__ build tool. After downloading the sources, it is enough to type

    $ sbt run
to simply run the program, or

    $ sbt assembly
to create a `.jar` package runnable with Java Virtual Machine, and then

    $  java -jar target/scala-2.10/go-hpl-assembly-0.1.jar
to use it.

A black window will appear. The user can draw half-planes by
clicking-and-dragging inside that window. Once he is satisfied, the “start”
button should be used to run the algorithm. During the merging, the mergees
will be coloured in green and blue, while the result will be drawn in orange.

### 3. Drawing

Drawing is done using [processing library](http://processing.org/) and
implemented in file [`drawing.scala`](/jasiekmarc/go-hpl/src/master/src/main/scala/drawing.scala).
The only non-trivial function there is [`drawPolygon` (line 22)](/jasiekmarc/go-hpl/src/master/src/main/scala/drawing.scala#cl-22),
one can however observe few nice usages of _case classes_ and _pattern matching_,
which make the code cleaner, for instance, in a `draw` function.


