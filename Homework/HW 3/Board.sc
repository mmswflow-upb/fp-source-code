
case class Board(val board: List[List[Player]], val player: Player) {

  type Line = List[Player]

  def isFree(x: Int, y: Int): Boolean = {
    this.board(x)(y) match {
      case Empty => true
      case _ => false
    }
  }
  def getColumns: Board = {
    new Board(this.board.transpose, this.player)
  }
  def getFstDiag: Line = {
    for {
      (row, indx) <- this.board.zipWithIndex
    } yield row(indx)
  }
  def getSndDiag: Line = {
    for {
      (row, indx) <- this.board.zipWithIndex
    } yield row(this.board.size-1 - indx)
  }

  def getAboveFstDiag: List[Line] = {
    (1 until board.size)
      .toList
      .map {
        offset => {
          for {
            (row, indx) <- this.board.zipWithIndex
            if (indx + offset <= this.board.size - 1)
          } yield row(indx + offset)
        }
      }
  }
  def getBelowFstDiag: List[Line] = getColumns.getAboveFstDiag
  def getAboveSndDiag: List[Line] = new Board(this.board.map(_.reverse), this.player).getAboveFstDiag
  def getBelowSndDiag: List[Line] = new Board(this.board.map(_.reverse), this.player).getBelowFstDiag

  def winner: Boolean = this.sequences(5) > 0

  def update(ln: Int, col: Int): Board = {
    new Board(
      for {
        (row, rowIndx) <- this.board.zipWithIndex
      } yield for {
        (elem, colIndx) <- row.zipWithIndex
      } yield if(rowIndx == ln && colIndx == col) this.player else elem
      , this.player.complement)
  }
  def next: List[Board] = {
    if(this.winner) Nil
    else for {
      row <- this.board.indices.toList
      col <- this.board.indices.toList
      if(this.isFree(row,col))
    } yield this.update(row, col)
  }

  def sequences: Map[Int,Int] = {



    def matchReqLen(line: Line, requiredLength: Int): Boolean = {
      if(line.size < 5) false
      else {
        line.sliding(5).count(
          window => {
            val pCount = window.count(_ == this.player)

            window.forall(p => (p == this.player || p == Empty) && pCount == requiredLength )
          }
        ) > 0
      }
    }

    def getAllCompleteableLines(requiredLength: Int): Int = {
      val lines = board.count(matchReqLen(_, requiredLength))
      val columns = getColumns.board.count(matchReqLen(_, requiredLength))
      val fstDiag = if(matchReqLen(getFstDiag, requiredLength)) 1 else 0
      val sndDiag = if(matchReqLen(getSndDiag, requiredLength)) 1 else 0

      val aboveFstDiag = getAboveFstDiag.count(matchReqLen(_, requiredLength))
      val belowFstDiag = getBelowFstDiag.count(matchReqLen(_, requiredLength))
      val aboveSndDiag = getAboveSndDiag.count(matchReqLen(_, requiredLength))
      val belowSndDiag = getBelowSndDiag.count(matchReqLen(_, requiredLength))




      lines + columns + fstDiag + sndDiag + aboveSndDiag + aboveFstDiag + belowSndDiag + belowFstDiag
    }



    val seq5: Int = getAllCompleteableLines(5)
    val seq4: Int = getAllCompleteableLines(4)
    val seq3: Int = getAllCompleteableLines(3)
    val seq2: Int = getAllCompleteableLines(2)

    Map(
      (5,seq5),
      (4, seq4),
      (3, seq3),
      (2, seq2),
    )
  }

  override def toString: String = {
    this.board
      .map(_.map(_.toString).mkString)
      .reduce(
        (row1, row2) => row1 + "\n" + row2
      )
  }
}

object Board {

  def tokenID:Int = 00

  def apply(s: String, p: Player): Board = {
    def toPos(c: Char): Player =
      c match {
        case 'X' => One
        case '0' => Two
        case _ => Empty
      }
    new Board(s
      .split('\n')
      .toList
      .map(_.toList.map(toPos))
      , p)
  }

  def apply(s: String): Board = {
    apply(s, One)
  }
}
