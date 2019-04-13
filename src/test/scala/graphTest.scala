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

    val burma14 = Graph.fromTSPFile("burma14.xml")
    val gr24 = Graph.fromTSPFile("gr24.xml")
    val eil76 = Graph.fromTSPFile("eil76.xml")
    val bays29 = Graph.fromTSPFile("bays29.xml")
    val gr21 = Graph.fromTSPFile("gr21.xml")
    val swiss42 = Graph.fromTSPFile("swiss42.xml")
    val ulysses16 = Graph.fromTSPFile("ulysses16.xml")
    val ulysses22 = Graph.fromTSPFile("ulysses22.xml")


    /** burma14 */
    var currentTime = System.currentTimeMillis()
    println("burma14, getLocalTSP path length: " + burma14.edgesToVertices(burma14.getLocalTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("burma14, dynamicTSP path length: " + burma14.edgesToVertices(burma14.dynamicTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("burma14, getOptimalTour: " + burma14.edgesToVertices(burma14.getOptimalTour))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("burma14, branchBoundTSP: " + burma14.edgesToVertices(burma14.branchBoundTSP))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    println("------------------------------------------------------------------;")


    /** gr24 */
    currentTime = System.currentTimeMillis()
    println("gr24, getLocalTSP path length: " + gr24.edgesToVertices(gr24.getLocalTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("gr24, dynamicTSP path length: " + gr24.edgesToVertices(gr24.dynamicTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("gr24, getOptimalTour: " + gr24.edgesToVertices(gr24.getOptimalTour))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("gr24, branchBoundTSP: " + gr24.edgesToVertices(gr24.branchBoundTSP))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    println("------------------------------------------------------------------;")

    /** eil76 */
    currentTime = System.currentTimeMillis()
    println("eil76, getLocalTSP path length: " + eil76.edgesToVertices(eil76.getLocalTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("eil76, dynamicTSP path length: " + eil76.edgesToVertices(eil76.dynamicTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("eil76, getOptimalTour: " + eil76.edgesToVertices(eil76.getOptimalTour))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("eil76, branchBoundTSP: " + eil76.edgesToVertices(eil76.branchBoundTSP))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    println("------------------------------------------------------------------;")

    /** bays29 */
    currentTime = System.currentTimeMillis()
    println("bays29, getLocalTSP path length: " + bays29.edgesToVertices(bays29.getLocalTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("bays29, dynamicTSP path length: " + bays29.edgesToVertices(bays29.dynamicTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("bays29, getOptimalTour: " + bays29.edgesToVertices(bays29.getOptimalTour))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("bays29, branchBoundTSP: " + bays29.edgesToVertices(bays29.branchBoundTSP))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    println("------------------------------------------------------------------;")

    /** gr21 */
    currentTime = System.currentTimeMillis()
    println("gr21, getLocalTSP path length: " + gr21.edgesToVertices(bays29.getLocalTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("gr21, dynamicTSP path length: " + gr21.edgesToVertices(bays29.dynamicTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("gr21, getOptimalTour: " + gr21.edgesToVertices(bays29.getOptimalTour))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("gr21, branchBoundTSP: " + gr21.edgesToVertices(bays29.branchBoundTSP))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    println("------------------------------------------------------------------;")

    /** swiss42 */
    currentTime = System.currentTimeMillis()
    println("swiss42, getLocalTSP path length: " + swiss42.edgesToVertices(swiss42.getLocalTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("swiss42, dynamicTSP path length: " + swiss42.edgesToVertices(swiss42.dynamicTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("swiss42, getOptimalTour: " + swiss42.edgesToVertices(swiss42.getOptimalTour))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("swiss42, branchBoundTSP: " + swiss42.edgesToVertices(swiss42.branchBoundTSP))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    println("------------------------------------------------------------------;")

    /** ulysses16 */
    currentTime = System.currentTimeMillis()
    println("ulysses16, getLocalTSP path length: " + ulysses16.edgesToVertices(ulysses16.getLocalTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("ulysses16, dynamicTSP path length: " + ulysses16.edgesToVertices(ulysses16.dynamicTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("ulysses16, getOptimalTour: " + ulysses16.edgesToVertices(ulysses16.getOptimalTour))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("ulysses16, branchBoundTSP: " + ulysses16.edgesToVertices(ulysses16.branchBoundTSP))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    println("------------------------------------------------------------------;")

    /** ulysses22 */
    currentTime = System.currentTimeMillis()
    println("ulysses22, getLocalTSP path length: " + ulysses22.edgesToVertices(ulysses16.getLocalTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("ulysses22, dynamicTSP path length: " + ulysses22.edgesToVertices(ulysses16.dynamicTSP()))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("ulysses22, getOptimalTour: " + ulysses22.edgesToVertices(ulysses16.getOptimalTour))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

    currentTime = System.currentTimeMillis()
    println("ulysses22, branchBoundTSP: " + ulysses22.edgesToVertices(ulysses16.branchBoundTSP))
    println("total time: " + (System.currentTimeMillis() - currentTime) + "\n")

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
