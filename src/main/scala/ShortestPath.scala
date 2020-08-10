case class Edge(from: Char, to: Char, distance: Int)

object ShortestPath {

  /**
   * 頂点
   */
  val vertexes: Seq[Char] = 'A' to 'N'

  /**
   * 辺
   */
  //  val edges = Seq(
  //    Edge('A', 'B', 1),
  //    Edge('A', 'C', 8),
  //    Edge('B', 'A', 1),
  //    Edge('B', 'C', 6),
  //    Edge('B', 'D', 6),
  //    Edge('B', 'E', 6),
  //    Edge('C', 'A', 8),
  //    Edge('C', 'B', 6),
  //    Edge('C', 'D', 7),
  //    Edge('D', 'B', 6),
  //    Edge('D', 'C', 7),
  //    Edge('D', 'F', 2),
  //    Edge('E', 'B', 6),
  //    Edge('E', 'F', 6),
  //    Edge('E', 'G', 8),
  //    Edge('F', 'D', 2),
  //    Edge('F', 'E', 6),
  //    Edge('F', 'G', 5),
  //    Edge('G', 'E', 8),
  //    Edge('G', 'F', 5)
  //  )

  val edges = Seq(
    Edge('A', 'B', 9),
    Edge('B', 'A', 9),
    Edge('A', 'C', 6),
    Edge('C', 'A', 6),
    Edge('A', 'D', 6),
    Edge('D', 'A', 6),
    Edge('B', 'E', 2),
    Edge('E', 'B', 2),
    Edge('C', 'E', 9),
    Edge('E', 'C', 9),
    Edge('C', 'G', 6),
    Edge('G', 'C', 6),
    Edge('D', 'F', 3),
    Edge('F', 'D', 3),
    Edge('E', 'I', 1),
    Edge('I', 'E', 1),
    Edge('F', 'H', 5),
    Edge('H', 'F', 5),
    Edge('F', 'J', 9),
    Edge('J', 'F', 9),
    Edge('G', 'I', 3),
    Edge('I', 'G', 3),
    Edge('G', 'J', 9),
    Edge('J', 'G', 9),
    Edge('H', 'K', 5),
    Edge('K', 'H', 5),
    Edge('J', 'K', 4),
    Edge('K', 'J', 4),
    Edge('J', 'L', 7),
    Edge('L', 'J', 7),
    Edge('K', 'M', 1),
    Edge('M', 'K', 1),
    Edge('L', 'N', 3),
    Edge('N', 'L', 3),
    Edge('M', 'J', 6),
    Edge('J', 'M', 6),
    Edge('M', 'N', 2),
    Edge('N', 'M', 2)
  )


  def solveByHellmanFord(start: Char, goal: Char): Unit = {
    var distances = vertexes.map(v => v -> Int.MaxValue).toMap
    distances = distances + (start -> 0)

    var isUpdated = true
    while (isUpdated) {
      isUpdated = false
      edges.foreach { e =>
        if (distances(e.from) != Int.MaxValue && distances(e.to) > distances(e.from) + e.distance) {
          distances = distances + (e.to -> (distances(e.from) + e.distance))
          isUpdated = true
        }
      }
    }

    println(distances)
    println(distances(goal))
  }


  def solveByDijkstra(start: Char, goal: Char): Unit = {
    var distances = vertexes.map(v => v -> Int.MaxValue).toMap
    distances = distances + (start -> 0)
    var candidates: Set[Char] = Set()
    var fixedNodes = Set(start)
    var current = start
    while (fixedNodes.size != vertexes.length) {
      //候補の追加（現在地から移動できるかつ、確定していない頂点を新たに候補とする）
      edges.foreach(e =>
        if (e.from == current && !fixedNodes.contains(e.to)) candidates = candidates + e.to
      )

      //候補すべてに対して、コストを更新する
      edges.withFilter(e => {
        //現在地から候補までの経路に絞る
        e.from == current && candidates.contains(e.to)
      })
        .foreach(e =>
          distances = distances + (e.to -> Math.min(distances(e.to), distances(current) + e.distance))
        )
      //候補の中から、コスト最小のものへ移動する(確定リストに含まれる頂点を削除してから最小を求める、でもよさそう)
      current = distances.filter(d => candidates.contains(d._1)).minBy(d => d._2)._1
      //現在地を確定リストへ設定し、候補リストから削除
      fixedNodes = fixedNodes + current
      candidates = candidates - current
    }

    println(distances)
    println(distances(goal))
  }
}