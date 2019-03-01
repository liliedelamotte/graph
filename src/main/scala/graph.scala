// ldelamotte17@georgefox.edu
// Assignment 4
// 2019-03-05


import java.io.{File, IOException}
import java.util.Scanner

/** Factory for graph instances. */
object graph
{

  /** A trait for representing directed and undirected graphs.
    *
    * @tparam T the data type of the graph.
    */
  trait Graph[T]
  {


    /** Returns true if the graph is directed. */
    def isDirected:Boolean


    /** Returns an iterable of all the vertices within the graph. */
    def getVertices:Iterable[T]


    /** Returns true if an edge exists, given a source and destination. */
    def edgeExists(source:T, destination:T):Boolean


    /** Gets the weight of an edge, given a source and destination.
      *
      * -1 is returned if the edge does not exist.
      */
    def getEdgeWeight(source:T, destination:T):Int


    /** Creates a new vertex and returns the updated graph.
      *
      * IllegalArgumentException is thrown if the given vertex already exists or is null.
      */
    @throws(classOf[IllegalArgumentException])
    def addVertex(vertex:T):Graph[T]


    /** Removes a given vertex and returns the updated graph.
      *
      * IllegalArgumentException is thrown if the given vertex does not exist.
      */
    @throws(classOf[IllegalArgumentException])
    def removeVertex(vertex:T):Graph[T]


    /** Creates an edge given a source, destination, and weight and returns
      * the updated graph.
      *
      * IllegalArgumentException is thrown if the given weight is not valid,
      * if either the source of the destination is null, if a loop is created,
      * or if either the source or destination of the new edge does not exist.
      */
    @throws(classOf[IllegalArgumentException])
    def addEdge(source:T, destination:T, weight:Int):Graph[T]


    /** Removes an edge given a source and destination and returns the updated graph.
      *
      * IllegalArgumentException is thrown if either the source or destination
      * of the edge to be removed does not exist.
      */
    @throws(classOf[IllegalArgumentException])
    def removeEdge(source:T, destination:T):Graph[T]


    /** Gets all adjacent vertices.
      *
      * IllegalArgumentException is thrown if the source does not exist or is null.
      */
    @throws(classOf[IllegalArgumentException])
    def getAdjacent(source:T):Iterable[T]


    /** Returns the length of the path given a sequence of vertices. */
    def pathLength(path:Seq[T]):Option[Long]


    /** Determines the shortest path between two vertices.
      *
      * IllegalArgumentException is thrown if the source or destination are
      * null or do not exist.
      */
    @throws(classOf[IllegalArgumentException])
    def shortestPathBetween(source:T, destination:T):Option[Seq[Edge[T]]]


    /** Returns an edge object given a source and destination, or None
      * if the edge does not exist.
      *
      * @throws java.lang.IllegalArgumentException if either the source or
      *                                            destination is null.
      */
    @throws(classOf[IllegalArgumentException])
    def getEdge(source:T, destination:T):Option[Edge[T]]


    /** Returns an iterable of all edges within the graph. */
    def getEdges():Iterable[Edge[T]]


    /** Returns a minimum spanning tree of the graph called upon.
      *
      * @throws java.lang.IllegalStateException if called upon a directed graph.
      */
    @throws(classOf[IllegalStateException])
    def minimumSpanningTree:Option[Graph[T]]


    /** Returns approximately the quickest way to visit all vertices in a graph. */
    def getLocalTSP():Seq[Edge[T]]


    /** Returns approximately the quickest way to visit all vertices in a graph. */
    def getLocalTSP(initialTour:Seq[T]):Seq[Edge[T]]


    /** Returns a string literal of the graph. */
    override def toString:String
  }


  /** An implementation of the Edge object. */
  class Edge[T](val source:T, val destination:T, val weight:Int) extends Ordered[Edge[T]] {


    /** Returns the result of comparing the weight of two Edges. A positive
      * number is returned if the other is smaller, zero if they are the same,
      * and a negative number if the other is larger.
      */
    def compare(other:Edge[T]):Int = {
      weight - other.weight
    }


    /** Returns an Edge string. */
    override def toString:String = {
      "\n" + source + ", " + destination + ", " + weight
    }

  }


  /** Serves as a factory function for producing new empty Graphs. */
  object Graph
  {


    /** Creates a new empty Graph given whether or not it is directed. */
    def apply[T](isDirected:Boolean):Graph[T] =
    {

      if (isDirected) new DirectedGraphImpl(Map())
      else new UndirectedGraphImpl(Map())

    }


    /** Takes in data from a CSV file and populate a graph accordingly. */
    @throws(classOf[IOException])
    def fromCSVFile(isDirected:Boolean, fileName:String):Graph[String] = {

      val file:File = new File(fileName)
      val scanner:Scanner = new Scanner(file)
      var graph = apply[String](isDirected)
      var numVertices:Int = 0
      var vertex:String = ""
      var numEdges:Int = 0
      var source:String = ""
      var destination:String = ""
      var weight:String = ""

      try {

        numVertices = scanner.nextInt()
        scanner.nextLine()
        scanner.useDelimiter("\r")

        for (v <- 1 to numVertices) {
          vertex = scanner.next()
          graph = graph.addVertex(vertex)
        }

        numEdges = scanner.nextInt()
        scanner.nextLine()

        scanner.useDelimiter(",|\\r\n")
        for (e <- 1 to numEdges) {
          source = scanner.next()
          destination = scanner.next()
          weight = scanner.next()
          graph = graph.addEdge(source, destination, weight.toInt)
        }
      }
      catch {
        case e: IOException => println("File error.")
      }

      graph

    }


    /** A private implementation of the Directed Graph trait. */
    private class DirectedGraphImpl[T]
    (val adjacencyList:Map[T, Map[T, Int]]) extends Graph[T]
    {


      /** Returns whether or not the graph is directed. */
      override def isDirected: Boolean = true


      /** Returns an iterable of all vertices within the graph. */
      def getVertices:Iterable[T] = adjacencyList.keys


      /** Returns true if an edge exists, given a source and destination. */
      def edgeExists(source:T, destination:T):Boolean = {

        (adjacencyList contains source) &&
          (adjacencyList contains destination) &&
          (adjacencyList(source).contains(destination))

      }


      /** Gets the weight of an edge, given a source and destination.
        *
        * -1 is returned if the edge does not exist.
        */
      def getEdgeWeight(source:T, destination:T):Int = {

        var edgeWeight = -1

        if (edgeExists(source, destination)) {
          edgeWeight = adjacencyList get source get destination
        }

        edgeWeight

      }


      /** Creates a new vertex and returns the updated graph.
        *
        * IllegalArgumentException is thrown if the given vertex is null or already exists.
        */
      def addVertex(vertex:T):Graph[T] = {

        if (vertex == null) {
          throw new IllegalArgumentException("Vertices cannot be null.")
        }

        if (adjacencyList contains vertex) {
          throw new IllegalArgumentException("Given vertex already exists.")
        }

        val newAdjacencyList = adjacencyList + (vertex -> Map[T, Int]())

        new DirectedGraphImpl[T](newAdjacencyList)

      }


      /** Removes a given vertex and returns the updated graph.
        *
        * IllegalArgumentException is thrown if the given vertex does not exist.
        */
      def removeVertex(vertex:T):Graph[T] = {

        if (vertex == null) {
          throw new IllegalArgumentException("Vertices cannot be null.")
        }

        if (!adjacencyList.contains(vertex)) {
          throw new IllegalArgumentException("Vertex does not exist.")
        }

        val newAdjacencyList = (adjacencyList - vertex).
          mapValues(map => map - vertex)

        new DirectedGraphImpl[T](newAdjacencyList)

      }


      /** Creates an edge given a source, destination, and weight and returns
        * the updated graph.
        *
        * IllegalArgumentException is thrown if the given weight is not valid,
        * if either the source of the destination is null, if a loop is created,
        * or if either the source or destination of the new edge does not exist.
        */
      def addEdge(source:T, destination:T, weight:Int):Graph[T] = {

        if (source == null || destination == null) {
          throw new IllegalArgumentException("Vertices cannot be null.")
        }

        if (weight <= 0) {
          throw new IllegalArgumentException("Weight cannot be negative.")
        }

        if (source == destination) {
          throw new IllegalArgumentException("Loops are not allowed.")
        }

        if (!adjacencyList.contains(source) || !adjacencyList.contains(destination)) {
          throw new IllegalArgumentException("One or both vertices do not exist.")
        }

        val newSourceMap = adjacencyList(source) + (destination -> weight)
        val newAdjacencyList = adjacencyList + (source -> newSourceMap)
        new DirectedGraphImpl[T](newAdjacencyList)

      }


      /** Removes an edge given a source and destination and returns the updated graph.
        *
        * IllegalArgumentException is thrown if either the source or destination
        * of the edge to be removed does not exist.
        */
      def removeEdge(source:T, destination:T):Graph[T] = {

        if (!edgeExists(source, destination)) {
          throw new IllegalArgumentException("Edge does not exist.")
        }

        val newSourceMap = adjacencyList(source) - destination
        val newAdjacencyList = adjacencyList + (source -> newSourceMap)

        new DirectedGraphImpl[T](newAdjacencyList)

      }


      /** Gets all adjacent vertices.
        *
        * IllegalArgumentException is thrown if the source does not exist or is null.
        */
      @throws(classOf[IllegalArgumentException])
      def getAdjacent(source:T):Iterable[T] = {

        if (source == null) {
          throw new IllegalArgumentException("Vertex is null.")
        }

        if (!adjacencyList.contains(source)) {
          throw new IllegalArgumentException("Vertex does not exist.")
        }

        adjacencyList(source).keys

      }


      /** Returns the weight of the path given a sequence of vertices or None
        * if no path exists. */
      def pathLength(path:Seq[T]):Option[Long] = {

        var pathSize = 0
        var missingEdge = false

        // looks at pairs of vertices and gets edge weight between the two, if any
        for (pair <- path.sliding(2)) {
          if (getAdjacent(pair.head).toSet.contains(pair.last)) {
            pathSize = pathSize + getEdgeWeight(pair.head, pair.last)
          }
          else {
            missingEdge = true
          }
        }

        if (missingEdge) {
          None
        }
        else {
          Some(pathSize.toLong)
        }

      }


      /** Determines the shortest path between two vertices.
        *
        * IllegalArgumentException is thrown if the source or destination are
        * null or do not exist or if there is no path between the source and destination.
        */
      @throws(classOf[IllegalArgumentException])
      def shortestPathBetween(source:T, destination:T):Option[Seq[Edge[T]]] = {

        var visited = Set[T]()
        var parent = Map[T, T]()
        var distance = Map[T, Int]()
        var current = source
        val infinity = scala.Int.MaxValue
        var bestCost = infinity
        var cost = 0
        var shortestPathBetween = Seq[Edge[T]]()
        var isValidPath = true
        var numCandidates = 0

        if (source == null || destination == null) {
          throw new IllegalArgumentException("Vertices cannot be null.")
        }

        if (!adjacencyList.contains(source) || !adjacencyList.contains(destination)) {
          throw new IllegalArgumentException("One or both vertices do not exist.")
        }

        if (getAdjacent(source).size < 1) {
          throw new IllegalArgumentException("Source is not connected to graph.")
        }

        // sets distances to infinity; primes the pump
        for (vertex <- getVertices) {
          distance += (vertex -> infinity)
        }

        // sets the distance to the source to zero as that's where the path begins
        distance += (source -> 0)

        // officially visits the source
        visited += current

        while (!visited.contains(destination) && isValidPath) {

          // compares current distance to each adjacent vertex to the distance
          // from the current vertex and replaces it with a smaller value if applicable
          for (vertex <- getAdjacent(current)) {
            if (!visited.contains(vertex)) {
              cost = getEdgeWeight(current, vertex) + distance(current)
              if (cost < distance(vertex)) {
                distance += (vertex -> cost)
                parent += (vertex -> current)
              }
            }

          }

          // resets best cost
          bestCost = infinity

          // resets number of available vertices to visit
          numCandidates = 0

          for (vertex <- distance.keys) {

            if (!visited.contains(vertex) && distance(vertex) < infinity) {
              numCandidates += 1
            }

            // selects smallest unvisited vertex from the distance map
            if (distance(vertex) < bestCost && !visited.contains(vertex)) {

              bestCost = distance(vertex)
              current = vertex

            }
          }

          // enters if there are still vertices left to visit
          if (numCandidates > 0) {
            // adds the current vertex to visited set
            visited += current
          }
          else {
            isValidPath = false
          }
        }


        // a path does not exist, None is returned
        if (!isValidPath) {
          None
        }
        else {
          // creates a sequence of edges that record the
          // shortest path between the source and destination
          while (current != source) {
            shortestPathBetween +:= new Edge[T](parent(current), current,
              getEdgeWeight(parent(current), current))
            current = parent(current)
          }
          Some(shortestPathBetween)
        }
      }


      /** Returns an edge object given a source and destination, or None
        * if the edge does not exist. */
      def getEdge(source:T, destination:T):Option[Edge[T]] = {

        if (edgeExists(source, destination)) {
          Some(new Edge[T](source, destination,
            getEdgeWeight(source, destination)))
        }
        else {
          None
        }

      }


      /** Returns an iterable of all edges within the graph. */
      def getEdges():Iterable[Edge[T]] = {

        var iterableOfEdges = Set[Edge[T]]()

        for (source <- getVertices) {
          for (destination <- getAdjacent(source)) {
              iterableOfEdges += new Edge[T](source, destination,
                getEdgeWeight(source, destination))
          }
        }

        iterableOfEdges

      }


      /** Returns None as directed graphs cannot have minimum spanning trees. */
      def minimumSpanningTree:Option[Graph[T]] = { None }


      /** Returns approximately the quickest way to visit all vertices in a graph. */
      def getLocalTSP():Seq[Edge[T]] = {

        var tour = Seq[T]()
        var stack = Seq[T]()
        var visited = Set[T]()

        stack +:= getVertices.head

        while (stack.nonEmpty) {

          // pops the first element from the stack
          var vertex = stack.head
          stack = stack.tail

          // adds vertex to the tour if it has not already been visited
          if (!visited.contains(vertex)) {
            tour :+= vertex
            visited += vertex
          }

          // looks at adjacent vertices and adds them to
          // the stack if they have not been visited
          for (adjacentVertex <- getAdjacent(vertex)) {
            if (!visited.contains(adjacentVertex)) {
              stack +:= adjacentVertex
            }
          }
        }

        tour :+= getVertices.head

        getLocalTSP(tour)

      }


      /** Returns approximately the quickest way to visit all vertices in a graph. */
      def getLocalTSP(initialTour:Seq[T]):Seq[Edge[T]] = {

        var current = initialTour
        var found = true
        var newTour = Seq[T]()
        var bestTour = Seq[Edge[T]]()
        var bestDistance = pathLength(current).get

        while (found) {

          found = false
          bestDistance = pathLength(current).get

          // iterates through all possible pairs of vertices
          for (i <- getVertices) {

            for (j <- getVertices) {

              // swaps the two vertices and the path
              // between them to create a potential tour
              newTour = twoOptSwap(current, i, j)
              val newDistance = pathLength(newTour)

              // determines whether or not the new tour is quicker
              if (newDistance.isDefined) {

                if (newDistance.get < bestDistance
                  && newTour.size == getVertices.size) {

                  current = newTour
                  bestDistance = newDistance.get
                  found = true

                }

              }

            }

          }

        }

        // creates a sequence of edges based off the best tour
        for (pair <- current.sliding(2)) {
          bestTour = bestTour ++ getEdge(pair.head, pair.last)
        }

        bestTour

      }


      /** Swaps the path between two elements and returns a new tour. */
      def twoOptSwap(tour:Seq[T], firstVertex:T, secondVertex:T):Seq[T] = {

        val prefix = tour.slice(0, tour.indexOf(firstVertex))
        val reverse = tour.slice(tour.indexOf(firstVertex), tour.indexOf(secondVertex))
        val suffix = tour.slice(tour.indexOf(secondVertex), tour.size)
        prefix ++ reverse ++ suffix

      }


      /** Returns a string literal of the graph. */
      override def toString:String = {

        var string = "\nSource -> [Destination, Edge Weight]\n"

        for (vertex <- getVertices) {
          string += (vertex + " -> (")
          for ((k, v) <- adjacencyList(vertex)) {
            string += ("[" + k + ", " + v + "]")
          }
          string += ")\n"
        }

        string

      }
    }


    /**
      * A private implementation of the Undirected Graph trait.
      *
      * @tparam T the data type of the graph.
      */
    private class UndirectedGraphImpl[T]
    (val adjacencyList:Map[T, Map[T, Int]]) extends Graph[T]
    {


      /** Returns whether or not the graph is directed. */
      override def isDirected: Boolean = false


      /** Returns an iterable of all vertices within the graph. */
      def getVertices:Iterable[T] = adjacencyList.keys


      /** Returns true if an edge exists, given a source and destination. */
      def edgeExists(source:T, destination:T):Boolean = {

        (adjacencyList contains source) &&
          (adjacencyList contains destination) &&
          (adjacencyList(source).contains(destination)) &&
          (adjacencyList(destination).contains(source))

      }


      /** Gets the weight of an edge, given a source and destination.
        *
        * -1 is returned if the edge does not exist.
        */
      def getEdgeWeight(source:T, destination:T):Int = {

        var edgeWeight = -1

        if (edgeExists(source, destination)) {
          edgeWeight = adjacencyList get source get destination
        }

        edgeWeight

      }


      /** Creates a new vertex and returns the updated graph.
        *
        * IllegalArgumentException is thrown if the
        * given vertex is null or already exists.
        */
      def addVertex(vertex:T):Graph[T] = {

        if (vertex == null) {
          throw new IllegalArgumentException("Vertices cannot be null.")
        }

        if (adjacencyList.contains(vertex)) {
          throw new IllegalArgumentException("Given vertex already exists.")
        }

        val newAdjacencyList = adjacencyList + (vertex -> Map[T, Int]())

        new UndirectedGraphImpl[T](newAdjacencyList)

      }


      /** Removes a given vertex and returns the updated graph.
        *
        * IllegalArgumentException is thrown if the given vertex does not exist.
        */
      def removeVertex(vertex:T):Graph[T] = {

        if (vertex == null) {
          throw new IllegalArgumentException("Vertices cannot be null.")
        }

        if (!adjacencyList.contains(vertex)) {
          throw new IllegalArgumentException("Vertex does not exist.")
        }

        val newAdjacencyList = (adjacencyList - vertex).mapValues(map => map - vertex)

        new UndirectedGraphImpl[T](newAdjacencyList)

      }


      /** Creates an edge given a source, destination, and weight and returns
        * the updated graph.
        *
        * IllegalArgumentException is thrown if the given weight is not valid,
        * if either the source of the destination is null, if a loop is created,
        * or if either the source or destination of the new edge does not exist.
        */
      def addEdge(source:T, destination:T, weight:Int):Graph[T] = {

        if (source == null || destination == null) {
          throw new IllegalArgumentException("Vertices cannot be null.")
        }

        if (weight < 0) {
          throw new IllegalArgumentException("Weight cannot be negative.")
        }

        if (source == destination) {
          throw new IllegalArgumentException("Loops are not allowed.")
        }

        if (!adjacencyList.contains(source) || !adjacencyList.contains(destination)) {
          throw new IllegalArgumentException("One or both vertices do not exist.")
        }

        val newSourceMap = adjacencyList(source) + (destination -> weight)
        val newAdjacencyList = adjacencyList + (source -> newSourceMap)

        val newDestinationMap = adjacencyList(destination) + (source -> weight)
        val newerAdjacencyList = newAdjacencyList + (destination -> newDestinationMap)

        new UndirectedGraphImpl[T](newerAdjacencyList)

      }


      /** Removes an edge given a source and destination and returns the updated graph.
        *
        * IllegalArgumentException is thrown if either the source or destination
        * of the edge to be removed does not exist.
        */
      def removeEdge(source:T, destination:T):Graph[T] = {

        if (!edgeExists(source, destination)) {
          throw new IllegalArgumentException("Edge does not exist.")
        }

        val newSourceMap = adjacencyList(source) - destination
        val newDestinationMap = adjacencyList(destination) - source
        val newAdjacencyList = adjacencyList + (source -> newSourceMap,
          destination -> newDestinationMap)

        new UndirectedGraphImpl[T](newAdjacencyList)

      }


      /** Gets all adjacent vertices.
        *
        * IllegalArgumentException is thrown if the source does not exist or is null.
        */
      @throws(classOf[IllegalArgumentException])
      def getAdjacent(source:T):Iterable[T] = {

        if (source == null) {
          throw new IllegalArgumentException("Vertex is null.")
        }

        if (!adjacencyList.contains(source)) {
          throw new IllegalArgumentException("Vertex does not exist.")
        }

        adjacencyList(source).keys

      }


      /** Returns the weight of the path given a sequence of vertices or None
        * if no path exists. */
      def pathLength(path:Seq[T]):Option[Long] = {

        var pathSize = 0
        var missingEdge = false

        for (pair <- path.sliding(2)) {
          if(getAdjacent(pair.head).toSet.contains(pair.last)) {
            pathSize = pathSize + getEdgeWeight(pair.head, pair.last)
          }
          else {
            missingEdge = true
          }
        }

        if (missingEdge) {
          None
        }
        else {
          Some(pathSize.toLong)
        }

      }

      /** Determines the shortest path between two vertices.
        *
        * IllegalArgumentException is thrown if the source or destination are
        * null or do not exist or if there is no path between the source and destination.
        */
      @throws(classOf[IllegalArgumentException])
      def shortestPathBetween(source:T, destination:T):Option[Seq[Edge[T]]] = {

        var visited = Set[T]()
        var parent = Map[T, T]()
        var distance = Map[T, Int]()
        var current = source
        val infinity = scala.Int.MaxValue
        var bestCost = infinity
        var cost = 0
        var shortestPathBetween = Seq[Edge[T]]()
        var isValidPath = true
        var numCandidates = 0

        if (source == null || destination == null) {
          throw new IllegalArgumentException("Vertices cannot be null.")
        }

        if (!adjacencyList.contains(source) || !adjacencyList.contains(destination)) {
          throw new IllegalArgumentException("One or both vertices do not exist.")
        }

        if (getAdjacent(source).size < 1) {
          throw new IllegalArgumentException("Source is not connected to graph.")
        }

        // sets all parents to null and distances to infinity; primes the pump
        for (vertex <- getVertices) {
          distance += (vertex -> infinity)
        }

        // sets the distance to the source to zero as that's where the path begins
        distance += (source -> 0)

        // officially visits the source
        visited += current

        while (!visited.contains(destination) && isValidPath) {

          // compares current distance to each adjacent vertex to the distance
          // from the current vertex and replaces it with a smaller value if applicable
          for (vertex <- getAdjacent(current)) {
            if (!visited.contains(vertex)) {
              cost = getEdgeWeight(current, vertex) + distance(current)
              if (cost < distance(vertex)) {
                distance += (vertex -> cost)
                parent += (vertex -> current)
              }
            }

          }

          // resets best cost
          bestCost = infinity

          // resets number of available vertices to visit
          numCandidates = 0

          for (vertex <- distance.keys) {

            if (!visited.contains(vertex) && distance(vertex) < infinity) {
              numCandidates += 1
            }

            // selects smallest unvisited vertex from the distance map
            if (distance(vertex) < bestCost && !visited.contains(vertex)) {

              bestCost = distance(vertex)
              current = vertex

            }
          }

          // enters if there are still vertices left to visit
          if (numCandidates > 0) {
            // adds the current vertex to visited set
            visited += current
          }
          else {
            isValidPath = false
          }
        }

        // a path does not exist, None is returned
        if (!isValidPath) {
          None
        }
        else {
          // creates a sequence of edges that record the
          // shortest path between the source and destination
          while (current != source) {
            shortestPathBetween +:= new Edge[T](parent(current), current,
              getEdgeWeight(parent(current), current))
            current = parent(current)
          }
          Some(shortestPathBetween)
        }
      }


      /** Returns an edge object given a source and destination, or None
        * if the edge does not exist. */
      @throws(classOf[IllegalArgumentException])
      def getEdge(source:T, destination:T):Option[Edge[T]] = {

        if (edgeExists(source, destination)) {
          Some(new Edge[T](source, destination, getEdgeWeight(source, destination)))
        }
        else {
          None
        }

      }


      /** Returns an iterable of all edges within the graph. */
      def getEdges():Iterable[Edge[T]] = {

        var iterableOfEdges = Set[Edge[T]]()
        var potentialOppositeEdge:Edge[T] = null

        for (source <- getVertices) {
          for (destination <- getAdjacent(source)) {

            // only adds the edge if its opposite does not already exist in the set
            potentialOppositeEdge = new Edge[T](destination, source,
              getEdgeWeight(destination, source))
            if (!iterableOfEdges.contains(potentialOppositeEdge)) {
              iterableOfEdges += new Edge[T](source, destination,
                getEdgeWeight(source, destination))
            }

          }
        }

        iterableOfEdges

      }


      /** Returns a minimum spanning tree of the graph called upon. */
      @throws(classOf[IllegalStateException])
      def minimumSpanningTree:Option[Graph[T]] = {
        var visited = Set[T]()
        var distance = Map[T, Int]()
        var parent = Map[T, T]()
        var minimumSpanningTree = Graph[T](isDirected)
        var current = getVertices.head
        val infinity = scala.Int.MaxValue
        var cost = 0
        var bestCost = infinity
        var numCandidates = 0
        var isValidTree = true

        // sets distances to infinity; primes the pump
        for (vertex <- getVertices) {
          distance += (vertex -> infinity)
        }

        // sets the distance to the first vertex to zero as that's where the path begins
        distance = distance + (current -> 0)

        // officially visits the first vertex
        visited += current

        while (visited.size < getVertices.size && isValidTree) {

          // compares current distance to each adjacent vertex to the distance
          // from the current vertex and replaces it with a smaller value if applicable
          for (vertex <- getAdjacent(current)) {
            if (!visited.contains(vertex)) {
              cost = getEdgeWeight(current, vertex)
              if (cost < distance(vertex)) {
                distance += (vertex -> cost)
                parent += (vertex -> current)
              }
            }
          }

          // resets the best cost
          bestCost = infinity

          // resets the number of vertices available to visit
          numCandidates = 0

          for (vertex <- distance.keys) {

            if (!visited.contains(vertex) && distance(vertex) < infinity) {
              numCandidates += 1
            }

            if (distance(vertex) < bestCost && !visited.contains(vertex)) {
              bestCost = distance(vertex)
              current = vertex
            }

          }

          // if there are still vertices left to visit,
          // the current is officially visited
          if (numCandidates > 0) {
            visited += current
          }
          // otherwise, no minimum spanning tree can be created
          else {
            isValidTree = false
          }
        }

        if (!isValidTree) {
          None
        }
        else {
          // creates a tree based off the parent map
          for (vertex <- getVertices) {
            minimumSpanningTree = minimumSpanningTree.addVertex(vertex)
          }
          for (vertex <- parent.keys) {
            if (!minimumSpanningTree.edgeExists(parent(vertex), vertex)
              && parent(vertex) != None) {
              minimumSpanningTree = minimumSpanningTree.addEdge(parent(vertex),
                vertex, getEdgeWeight(parent(vertex), vertex))
            }
          }
          Some(minimumSpanningTree)
        }
      }


      /** Returns approximately the quickest way to visit all vertices in a graph. */
      def getLocalTSP():Seq[Edge[T]] = {

        var tour = Seq[T]()
        var stack = Seq[T]()
        var visited = Set[T]()

        stack +:= getVertices.head

        while (stack.nonEmpty) {

          // pops the first element from the stack
          var vertex = stack.head
          stack = stack.tail

          // adds vertex to the tour if it has not already been visited
          if (!visited.contains(vertex)) {
            tour :+= vertex
            visited += vertex
          }

          // looks at adjacent vertices and adds them to
          // the stack if they have not been visited
          for (adjacentVertex <- getAdjacent(vertex)) {
            if (!visited.contains(adjacentVertex)) {
              stack +:= adjacentVertex
            }
          }
        }

        tour :+= getVertices.head

        getLocalTSP(tour)

      }


      /** Returns approximately the quickest way to visit all vertices in a graph. */
      def getLocalTSP(initialTour:Seq[T]):Seq[Edge[T]] = {

        var current = initialTour
        var found = true
        var newTour = Seq[T]()
        var bestTour = Seq[Edge[T]]()
        var bestDistance = pathLength(current).get

        while (found) {

          found = false
          bestDistance = pathLength(current).get

          // iterates through all possible pairs of vertices
          for (i <- getVertices) {

            for (j <- getVertices) {

              // swaps the two vertices and the path
              // between them to create a potential tour
              newTour = twoOptSwap(current, i, j)
              val newDistance = pathLength(newTour)

              // determines whether or not the new tour is quicker
              if (newDistance.isDefined) {

                if (newDistance.get < bestDistance
                  && newTour.size == getVertices.size) {

                  current = newTour
                  bestDistance = newDistance.get
                  found = true

                }

              }

            }

          }

        }

        // creates a sequence of edges based off the best tour
        for (pair <- current.sliding(2)) {
          bestTour = bestTour ++ getEdge(pair.head, pair.last)
        }

        bestTour

      }


      /** Swaps the path between two elements and returns a new tour. */
      def twoOptSwap(tour:Seq[T], firstVertex:T, secondVertex:T):Seq[T] = {

        val prefix = tour.slice(0, tour.indexOf(firstVertex))
        val reverse = tour.slice(tour.indexOf(firstVertex), tour.indexOf(secondVertex))
        val suffix = tour.slice(tour.indexOf(secondVertex), tour.size)
        prefix ++ reverse ++ suffix

      }


      /** Returns a string literal of the graph. */
      override def toString:String = {

        var string = "\nSource -> [Destination, Edge Weight]\n"

        for (vertex <- getVertices) {
          string += (vertex + " -> (")
          for ((k, v) <- adjacencyList(vertex)) {
            string += ("[" + k + ", " + v + "]")
          }
          string += ")\n"
        }

        string

      }
    }
  }

  def main(args:Array[String]): Unit = {

    var graph = Graph[String](true)


    graph = graph.addVertex("chemex")
    graph = graph.addVertex("aeropress")
    graph = graph.addVertex("v-60")

    graph = graph.addEdge("chemex", "aeropress", 2)
    graph = graph.addEdge("aeropress", "v-60", 3)
    graph = graph.addEdge("v-60", "chemex", 4)

    graph = graph.addEdge("chemex", "v-60", 1)
    graph = graph.addEdge("v-60", "aeropress", 1)
    graph = graph.addEdge("aeropress", "chemex", 1)

    val tours = Seq("chemex", "aeropress", "v-60", "chemex")

  }

}
