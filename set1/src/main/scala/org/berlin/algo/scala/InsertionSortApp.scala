/**
 * Berlin Brown , berlin dot brown at gmail.com
 * 
 * Scala Insertion Sort based on Haskell impl (Convert from Haskell to Scala).
 * 
 */
package org.berlin.algo.scala;

/**
 * Insertion Sort using list of integers.
 * Based on logic from "Introduction to algorithms", coreman, stein.
 *
 * Using recursion for later use with Haskell.
 * 
 * Expected Output:
 * <pre>
 * Running main - insertion sort - with recursion
 * A {N=11}= [5, 2, 4, 6, 1, 3, 83, 12, 3, 5, 7, ].
 * A(after sort) = [1, 2, 3, 3, 4, 5, 5, 6, 7, 12, 83, ].
 * </pre>
 */
object InsertionSortApp {
  
/* 
 * Based on Haskell version:
  insert e [] = [e]
  insert e lst@(x:xs)
    | e < x     = e : lst
    | otherwise = x : (insert e xs)
  insertionSort lst = insertionSort' lst [] where
    insertionSort' [] lst = lst
    insertionSort' (x:xs) lst = insertionSort' xs (insert x lst)
 */
  
  def insert(e : Integer, lst : List[Int]) : List[Int] = {
      def insertPrime(xs: List[Int]) : List[Int] = xs match {
        case Nil => List(e)
        case x :: xs if (e < x) => e :: x :: xs        
        case x :: xs => x :: insertPrime(xs)               
      }   
      return insertPrime(lst)
  }
  
  def insertionSort(origList: List[Int]) : List[Int] = {
      def insertionSortPrime(xs: List[Int], lst: List[Int]) : List[Int] = xs match {
        case Nil => lst
        case x :: xs => insertionSortPrime(xs, insert(x, lst))
      }
      insertionSortPrime(origList, List())
  }
  
  /********************************************************
   * Revisited
   * @param args
   ********************************************************/
  object InsertionSortNewsAppRevisited {   
    def insert : (Int, List[Int]) => List[Int] = {
      case (e, List())          => List(e)
      case (e, lst @ (x :: xs)) =>
        if (e < x) e :: x :: xs
        else x :: insert(e, xs)
    }
    def insertionSort(lst: List[Int]) = {
        def `insertionSort'`: (List[Int], List[Int]) => List[Int] = {
          case (List(), lst)  => lst
          case (x :: xs, lst) => `insertionSort'`(xs, insert(x, lst))
        }
        `insertionSort'`(lst, Nil)
    }  
    def main(args : Array[String]) : Unit = {
      println("Running - Insertion Sort Test(2)")
      val lst = List(1, 7, 3, 4, 5)
      println("Test: " + (insertionSort(lst)))
    }     
  }
  
  def main(args : Array[String]) : Unit = {
    println("Running - Insertion Sort Test")
    val lst = List(1, 7, 3, 4, 5)
    println("Test: " + (insertionSort(lst)))
    println("Test[2]: " + (InsertionSortNewsAppRevisited.insertionSort(lst)))
  }
} // End of object // 
