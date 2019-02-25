import org.scalatest.FunSuite
import graph.Graph

class graphTest extends FunSuite {

  test("testDirectedIsDirected") {

    val graph = Graph[Int](true)
    assert(graph.isDirected)

  }

  test("testDirectedGetVertices") {

    var graph = Graph[Int](true)
    graph = graph.addVertex(1)
    graph = graph.addVertex(2)
    assert(graph.getVertices.size == 2)

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
