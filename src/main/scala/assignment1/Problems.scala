package assignment1

import list.implementation.Cons
import list.traits.IntList

object Problems {

  /**
    * Given the weight in kilograms, that a bag can hold, and a list of items represented by their weights
    * in grams, calculate how many bags are needed to hold all of the given items.
    *
    * @param capacity   the capacity of a bag in kg
    * @param itemWeight weights of the items in grams
    * @return minimum number of bags required
    */
  def minBagsCount(capacity: Int, itemWeight: IntList): Int = {
    val sum = itemWeight.reduceLeft(_ + _)
    if (sum % capacity == 0) sum / capacity
    else sum / capacity + 1

  }


  /**
    * Given an amount of money and a list of coin values,
    * returns the number of possible ways that the change can be returned using
    * coins of those values.
    *
    * E.x. countChange(4,SinglyLinkedList(1,2)
    * has 3 solutions -> {1+1+1+1 ; 2+2 ; 1+2+1}
    *
    * @param money total amount of change to return
    * @param coins possible coins
    * @return number of possible ways the change can be returned
    */
  def countChange(money: Int, coins: IntList): Int = coins match {
    case _ if money == 0 => 1
    case Cons(head, tail) if money > 0 => countChange(money - head, Cons(head, tail)) + countChange(money, tail)
    case _ => 0
  }

  /**
    * A postman has a list of delivery addresses, for which he and his colleague are responsible.
    * The addresses they have to visit are split into odd and even - one part for each of them.
    * He can choose which part he wants to take. Being lazy, he wants to take the one,
    * where he will be done faster.
    *
    * All addresses are located on a very long street and all odd ones are on one side
    * and the even ones on the other.
    * The distance between 2 addresses can be computed based on their address number.
    *
    * E.x the distance between 40 and 42 ist 1, because they are next to each other
    * (all even numbers on one side, odd on the other).
    * The distance between 40 and 48 is 4.
    * The total distance for the addresses {40, 42, 52} is 6
    *
    * He has to stop at every address and drop off all the mail for this building,
    * which he computes as an extra 2 distance per address.
    * Also taking this into account the distance for the addresses {40, 42, 52} will be 6 + 3*2 = 12
    *
    * The postman starts from one end and goes to the other.
    *
    * Implement the method, which given the addresses predicts which path will be faster.
    *
    * @param addresses the address numbers
    * @return the faster path: true if even, false if odd
    */


  def shouldTakeEvenAddresses(addresses: IntList): Boolean = {

    val even = addresses.filter(_ % 2 == 0).insertionSort

    val odd = addresses.filter(_ % 2 != 0).insertionSort

    val ev = (even.get(even.size - 1) - (even.head)) / 2 + even.size * 2
    val od = (odd.get(odd.size - 1) - (odd.head)) / 2 + odd.size * 2

    ev < od

  }


  //Solving the following problem is optional, as it is a lot harder than the previous ones.
  // It is not a required part of the assignment, but will make all tests ins ProblemsTest turn green.

  /**
    * The postman has recognized, that addresses which are adjacent to the one
    * in front of which he stops with his delivery bike, take less time and
    * wants to also take this into account.
    *
    * The distance computation for the second condition is modified in the following way:
    * If he stops at 40 the additional distance between 40 and 42 as well as 40 and 38 is 1 instead of 2.
    * The one for 40 is still 2
    *
    * @param addresses the address numbers
    * @return the faster path: true if even, false if odd
    */
  def shouldTakeEvenAddressesExtended(addresses: IntList): Boolean =
 ???
}