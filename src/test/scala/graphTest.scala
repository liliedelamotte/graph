import org.scalatest.FunSuite
import graph.Graph

class graphTest extends FunSuite {

  test("testDirectedIsDirected") {

        var graph = Graph[Int](false)

        graph = graph.addVertex(1)
        graph = graph.addVertex(2)
        graph = graph.addVertex(3)
        graph = graph.addVertex(4)

        graph = graph.addEdge(1, 2, 1)
        graph = graph.addEdge(1, 3, 10)
        graph = graph.addEdge(1, 4, 1)
        graph = graph.addEdge(2, 3, 1)
        graph = graph.addEdge(2, 4, 10)
        graph = graph.addEdge(3, 4, 1)

//        var shortestPathLength:Long = 100000
//
//        val EIL101 = Graph.fromTSPFile("eil101.xml")
//
//        for (i <- 0 to 10) {
//
//          val tour = EIL101.getOptimalTour
//
//          if (EIL101.pathLength(tour).isDefined) {
//            val currentPathLength = EIL101.pathLength(tour).get
//            if (currentPathLength < shortestPathLength) {
//              shortestPathLength = currentPathLength
//            }
//          }
//
//        }
//
//        println("My path length: " + EIL101.pathLength(EIL101.getOptimalTour) + ".")
//
//
//        var KROA100 = Graph.fromTSPFile("kroA100.xml")
//
//        for (i <- 0 to 10) {
//          val tour = KROA100.getOptimalTour
//
//          if (KROA100.pathLength(tour).isDefined) {
//            val currentPathLength = EIL101.pathLength(tour).get
//            if (currentPathLength < shortestPathLength) {
//              shortestPathLength = currentPathLength
//            }
//          }
//        }
//
//        println("Path length: " + KROA100.pathLength(KROA100.getOptimalTour) + ".\n")

  }

  test("testDirectedGetVertices") {

    var graph = Graph[String](true)
    graph = graph.addVertex("steam buns")
    graph = graph.addVertex("tacos")
    graph = graph.addVertex("tots")
    graph = graph.addVertex("poke")
    graph = graph.addVertex("sushi")
    graph = graph.addEdge("steam buns", "tacos", 1)
    graph = graph.addEdge("steam buns", "tots", 10)
    graph = graph.addEdge("steam buns", "poke", 10)
    graph = graph.addEdge("steam buns", "sushi", 10)
    graph = graph.addEdge("tacos", "steam buns", 10)
    graph = graph.addEdge("tacos", "tots", 1)
    graph = graph.addEdge("tacos", "poke", 10)
    graph = graph.addEdge("tacos", "sushi", 10)
    graph = graph.addEdge("tots", "steam buns", 10)
    graph = graph.addEdge("tots", "tacos", 10)
    graph = graph.addEdge("tots", "poke", 1)
    graph = graph.addEdge("tots", "sushi", 10)
    graph = graph.addEdge("poke", "steam buns", 10)
    graph = graph.addEdge("poke", "tacos", 10)
    graph = graph.addEdge("poke", "tots", 10)
    graph = graph.addEdge("poke", "sushi", 1)
    graph = graph.addEdge("sushi", "steam buns", 1)
    graph = graph.addEdge("sushi", "tacos", 10)
    graph = graph.addEdge("sushi", "tots", 10)
    graph = graph.addEdge("sushi", "poke", 10)


    println("best: " + graph.getOptimalTour + "\n")

  }

  test("testDirectedEdgeExists") {

    var graph = Graph[Int](true)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.addEdge(1, 2, 3)
    assert(graph.edgeExists(1, 2))

  }

  test("testDirectedGetEdgeWeight") {

    var graph = Graph[Int](true)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.addEdge(1, 2, 3)
    assert(graph.getEdgeWeight(1, 2) == 3)

  }

  test("testDirectedAddVertex") {

    var graph = Graph[Int](true)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    assert(graph.getVertices.size == 2)

  }

  test("testDirectedRemoveVertex") {

    var graph = Graph[Int](true)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.removeVertex(1)
    assert(graph.getVertices.size == 1)

  }

  test("testDirectedAddEdge") {

    var graph = Graph[Int](true)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.addEdge(1, 2, 3)
    graph = graph.addEdge(2, 1, 3)
    assert(graph.edgeExists(1, 2) && graph.edgeExists(2, 1))

  }

  test("testDirectedRemoveEdge") {

    var graph = Graph[Int](true)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.addEdge(1, 2, 3)
    graph = graph.removeEdge(1, 2)
    println("Graph should be empty: " + graph)

  }

  test("testUndirectedIsDirected") {

    var graph = Graph[Int](false)
    assert(graph.isDirected == false)

  }

  test("testUndirectedGetVertices") {

    var graph = Graph[Int](false)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    assert(graph.getVertices.size == 2)

  }

  test("testUndirectedEdgeExists") {

    var graph = Graph[Int](false)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.addEdge(1, 2, 3)
    assert(graph.edgeExists(1, 2))

  }

  test("testUndirectedGetEdgeWeight") {

    var graph = Graph[Int](false)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.addEdge(1, 2, 3)
    assert(graph.getEdgeWeight(1, 2) == 3)

  }

  test("testUndirectedAddVertex") {

    var graph = Graph[Int](false)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    assert(graph.getVertices.size == 2)

  }

  test("testUndirectedRemoveVertex") {

    var graph = Graph[Int](false)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.removeVertex(2)
    assert(graph.getVertices.size == 1)

  }

  test("testUndirectedAddEdge") {

    var graph = Graph[Int](false)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.addEdge(1, 2, 3)
    assert(graph.edgeExists(1, 2))

  }

  test("testUndirectedRemoveEdge") {

    var graph = Graph[Int](false)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    graph = graph.addEdge(1, 2, 3)
    graph = graph.removeEdge(1, 2)
    assert(!graph.edgeExists(1, 2))

  }

}
