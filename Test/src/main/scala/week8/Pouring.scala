package week8

class Pouring(capacity: Vector[Int]) {
  type State = Vector[Int]

  val initialState = capacity map (x => 0)

  trait Move {
    def change(state: State): State
  }

  case class Empty(n: Int) extends Move {
    def change(state: State): State = state updated (n , 0)
  }

  case class Fill(n: Int) extends Move {
    def change(state: State): State = state updated (n, capacity(n))
  }

  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = capacity(to) - state(to) min state(from)
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (glass <- glasses) yield Empty(glass)) ++
      (for(glass <- glasses) yield Fill(glass)) ++
        (for(from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)

    override def toString = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): LazyList[Set[Path]] =
    if (paths.isEmpty) LazyList.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  def solutions(target: Int): LazyList[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}
