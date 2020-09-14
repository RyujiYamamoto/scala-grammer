package jp.co.dwango.marubatsu

import jp.co.dwango.marubatsu.board.{CellState, Empty, Maru => MaruState, Batsu => BatsuState}

package object game {

  private[game] def toWinner(cellState: CellState): Winner = cellState match {
    case MaruState => Maru
    case BatsuState => Batsu
    case Empty => NoWinner
  }
}
