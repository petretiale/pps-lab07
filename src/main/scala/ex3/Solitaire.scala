package ex3

object Solitaire extends App:
  type Pos = (Int, Int)
  type Solution = List[Pos]

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeMarks(w: Int, h: Int): List[Solution] =
    val totalCells = w * h
    val startPos = (w / 2, h / 2)

    def isSafe(p: Pos, currentSolution: Solution): Boolean =
      val (x, y) = p
      x >= 0 && x < w && y >= 0 && y < h && !currentSolution.contains(p)

    def getNextMoves(lastPos: Pos, currentSolution: Solution): List[Pos] =
      val (x, y) = lastPos
      val candidates = List(
        (x + 3, y), (x - 3, y), (x, y + 3), (x, y - 3),
        (x + 2, y + 2), (x + 2, y - 2), (x - 2, y + 2), (x - 2, y - 2)
      )
      candidates.filter(p => isSafe(p, currentSolution))

    def solve(currentSolution: Solution): List[Solution] =
      if currentSolution.length == totalCells then
        List(currentSolution)
      else
        val nextMoves = getNextMoves(currentSolution.head, currentSolution)
        for
          move <- nextMoves
          sol <- solve(move :: currentSolution)
        yield sol

    solve(List(startPos))

  println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))