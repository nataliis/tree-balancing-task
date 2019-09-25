package com.solution

import com.solution.api.{Leaf, Node}
import org.scalatest.FunSuite

class BalanceTreeSpec extends FunSuite {

    test("Happy path: example from the task") {
      val tree = Node(
        List(Leaf(2), Leaf(4), Leaf(3), Leaf(1)),
        List(Node.empty(), Node.empty(), Node.empty())
      )
      val expected = Node(
        List(Leaf(1), Leaf(2)),
        List(
          Node.empty(),
          Node.empty(),
          Node(List(Leaf(3)), List())
        )
      )
      val result = tree.balance(3)
      assert(result == expected)
    }

  test("Happy path: another example") {
    val tree = Node(
      List(Leaf(2), Leaf(3),  Leaf(1)),
      List(Node(List(Leaf(4)), List(Node.empty())), Node.empty())
    )
    val expected = Node(
      List(Leaf(1), Leaf(2)),
      List(
        Node(List(),List()),
        Node(
          List(Leaf(3)),
          List(Node(List(Leaf(4)), List()))
        )
      )
    )
    val W = 4
    val result = tree.balance(W)
    assert(result == expected)
  }

  test("Happy path: include negative numbers") {
    val tree = Node(
      List(Leaf(-9), Leaf(7),  Leaf(9)),
      List(Node.empty(), Node(List(Leaf(-5)), List(Node.empty())))
    )
    val expected = Node(
      List(Leaf(-9), Leaf(7), Leaf(9)),
      List(
        Node(List(),List()),
        Node(
          List(Leaf(-5)),
          List(Node(List(), List()))
        )
      )
    )
    val W = 10
    val result = tree.balance(W)
    assert(result == expected)
  }

  test("Attempt to construct a leaf with zero weight should fail") {
    assertThrows[IllegalArgumentException] {
      Leaf(0)
    }
  }
}
