package com.solution.api

import com.solution.util.MergeSort
import scala.annotation.tailrec

case class Leaf(weight: Int) {
  require(weight != 0, "Leaf weight should not be zero")
}

case class Node(leaves: List[Leaf], nodes: List[Node]) {

  private def addLeaves(leaf: List[Leaf]): Node = copy(leaves ++ leaf, nodes)

  implicit val ordering: Ordering[Leaf] = Ordering.by[Leaf, Int](_.weight)

  def balance(limit: Int): Node = {
    val sortedLeafs = MergeSort(leaves)
    val res = adjustLeafsForNode(sortedLeafs)(limit)
    res match {
      case (leave: List[Leaf], Nil) =>
        Node(leave, nodes)
      case (Nil, move: List[Leaf]) =>
        nodes match {
          case Nil =>
            Node.empty()
          case _ =>
            val updatedNode = nodes.head.addLeaves(move).balance(limit)
            Node(Nil, nodes.tail :+ updatedNode)
        }
      case (leave: List[Leaf], move: List[Leaf]) =>
        nodes match {
          case Nil =>
            Node(leave, nodes)
          case x :: xs =>
            val updatedNode = x.addLeaves(move).balance(limit)
            Node(leave, xs :+ updatedNode)
        }
    }
  }

  private def adjustLeafsForNode(leaves: List[Leaf])
                                (implicit limit: Int): (List[Leaf], List[Leaf]) = {
    @tailrec
    def loop(list: List[Leaf],
             leave: List[Leaf],
             move: List[Leaf],
             space: Int): (List[Leaf], List[Leaf]) = {
      list match {
        case h :: t if space - h.weight >= 0 =>
          loop(t, leave :+ h, move, space - h.weight)
        case h :: t =>
          loop(t, leave, move :+ h, space)
        case _ =>
          (leave, move)
      }
    }
    loop(leaves, Nil, Nil, limit)
  }
}

object Node {
  def empty(): Node = new Node(Nil, Nil)
}
