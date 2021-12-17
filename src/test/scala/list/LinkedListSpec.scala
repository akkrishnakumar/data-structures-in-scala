package list

import utils.BaseSpec
import SinglyLinkedList._

class LinkedListSpec extends BaseSpec {

  "A Singly Linked List" when {

    "not empty" should {
      val nonEmptyList = SinglyLinkedList("a", "b", "c", "d")

      "return first element" in {
        nonEmptyList.first shouldEqual Some("a")
      }

      "return last element" in {
        nonEmptyList.last shouldEqual Some("d")
      }

      "return element for specific position" in {
        nonEmptyList.get(3) shouldEqual Some("c")
      }

      "return None for position out of bound" in {
        nonEmptyList.get(5) shouldEqual None
      }

      "return None for negative position (out of bound)" in {
        nonEmptyList.get(-2) shouldEqual None
      }

      "return lenght of list" in {
        nonEmptyList.lenght shouldEqual 4
      }

      "be equal with another LinkedList with same values" in {
        nonEmptyList shouldEqual SinglyLinkedList("a", "b", "c", "d")
      }

      "not be equal to another LinkedList with different values" in {
        nonEmptyList shouldNot equal(SinglyLinkedList("a", "b", "c", "d", "e"))
      }

      "return formatted string" in {
        nonEmptyList.toString() shouldEqual "SinglyLinkedList(a, b, c, d)"
      }

      "add a specific element to the start" in {
        nonEmptyList.addFirst("1") shouldEqual SinglyLinkedList("1", "a", "b", "c", "d")
      }

      "add a specific element to the end" in {
        nonEmptyList.addLast("1") shouldEqual SinglyLinkedList("a", "b", "c", "d", "1")
      }

      "add a specific element to a position" in {
        nonEmptyList.addAt("zzzz", 2) shouldEqual SinglyLinkedList("a", "zzzz", "b", "c", "d")
      }

      "not add specific element to a negative position" in {
        nonEmptyList.addAt("a", -1) shouldEqual SinglyLinkedList("a", "b", "c", "d")
      }

      "delete first element and return new list" in {
        nonEmptyList.deleteFirst shouldEqual SinglyLinkedList("b", "c", "d")
      }

      "delete last element and return new list" in {
        nonEmptyList.deleteLast shouldEqual SinglyLinkedList("a", "b", "c")
      }

      "delete element from a specific position and return a new list" in {
        nonEmptyList.deleteAt(3) shouldEqual SinglyLinkedList("a", "b", "d")
      }

      "not delete element at a negative position" in {
        nonEmptyList.deleteAt(-2) shouldEqual SinglyLinkedList("a", "b", "c", "d")
      }

      "return length of non empty list" in {
        nonEmptyList.lenght shouldEqual 4
      }

    }

    "empty" should {

      val emptyList: SinglyLinkedList[String] = SinglyLinkedList.empty

      "return None for first element" in {
        emptyList.first shouldEqual None
      }

      "return None for last element" in {
        emptyList.last shouldEqual None
      }

      "return element for specific position" in {
        emptyList.get(2) shouldEqual None
      }

      "be equal with another LinkedList with same values" in {
        emptyList shouldEqual SinglyLinkedList()
      }

      "not be equal with another LinkedList with different values" in {
        emptyList shouldNot equal(SinglyLinkedList("a"))
      }

      "return formatted string" in {
        emptyList.toString() shouldEqual "EmptyList()"
      }

      "add a specific element to the start" in {
        emptyList.addFirst("a") shouldEqual SinglyLinkedList("a")
      }

      "add a specific element to the end" in {
        emptyList.addLast("b") shouldEqual SinglyLinkedList("b")
      }

      "not add specific element to a negative position" in {
        emptyList.addAt("a", -1) shouldEqual SinglyLinkedList.empty
      }

      "add a specific element to a position" in {
        emptyList.addAt("a", 2) shouldEqual SinglyLinkedList("a")
      }

      "delete first element and return new list" in {
        emptyList.deleteFirst shouldEqual SinglyLinkedList.empty
      }

      "delete last element and return new list" in {
        emptyList.deleteLast shouldEqual SinglyLinkedList.empty
      }

      "delete element from a specific position and return a new list" in {
        emptyList.deleteAt(2) shouldEqual SinglyLinkedList.empty
      }

      "not delete element at a negative position" in {
        emptyList.deleteAt(-2) shouldEqual SinglyLinkedList.empty
      }

      "return length of empty list" in {
        emptyList.lenght shouldEqual 0
      }

    }

  }

}
