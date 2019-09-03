/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.frogsandtoads

import doodle.core._
import doodle.syntax._
import doodle.image._


/**
  * A puzzle state is given as a 1-dimensional array of cell values.
  */
class PuzzleState private (
    board: Vector[PuzzleState.Cell],
    loc: Int
) {

  override def toString() = "[" ++ board.map(_.toString).reduceLeft(_ ++ "|" ++ _) + "]"

  import PuzzleState._

  val size = board.size
  val emptyLoc = loc

  def isTerminalState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Toad) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Frog)
  }

  def isInitialState(): Boolean = {
    board.slice(0, emptyLoc).forall(_ == Frog) &&
    board(emptyLoc) == Empty &&
    board.slice(emptyLoc + 1, size).forall(_ == Toad)
  }
  
  
  def toImage() : Image = boardImage(board);
  
  val box = Image.rectangle(40, 40)
  def boardImage(brd: Vector[PuzzleState.Cell]) : Image = {
    brd match {
      case hd +: tl => {
        val colouredBox = {
          hd match {
            case Frog  => box.fillColor(Color.green)
            case Toad  => box.fillColor(Color.brown)
            case Empty => box.fillColor(Color.white)
          }
        }
        boardImage(tl) beside colouredBox 
      }
      case _ => Image.rectangle(0,0)
    }
  }
  

  /**
   * Checks if the jumpFromLeft is a legal move. Only Frogs can jump 
   * from left to right, and not over another Frog. So the second left 
   * of the emptyLoc must be a Frog and immediate left must be a Toad
   * 
   * Creates a new board by slicing the existing board and concatenation the 
   * new Vector of Empty then Toad (previously there) then Frog since Frog has jumped
   * to the right.
   *  
   * @return the new PuzzleState as an Option
   */
  def jumpFromLeft(): Option[PuzzleState] = {
    try { 
      if(board(emptyLoc - 2) == Frog && board(emptyLoc - 1) == Toad) {
        val newBoard = board.slice(0, emptyLoc - 2) ++ Vector(Empty, Toad, Frog) ++ board.slice(emptyLoc + 1, size)
        val newState = new PuzzleState(newBoard, emptyLoc - 2)
        return Some(newState)
      }
      else return None
    }
    catch { 
        case x: IndexOutOfBoundsException => return None       
      }
  }
  
  /**
   * Checks if the jumpFromRight is a legal move. Only Toads can jump 
   * from right to left, and not over another Toad. So the second right 
   * of the emptyLoc must be a Toad and immediate right must be a Frog
   * 
   * Creates a new board by slicing the existing board and concatenation the 
   * new Vector of Toad then Frog (previously there) then Empty since Toad has jumped
   * to the left.
   *  
   * @return the new PuzzleState as an Option
   */
  def jumpFromRight(): Option[PuzzleState] = {
    try { 
      if(board(emptyLoc + 2) == Toad && board(emptyLoc + 1) == Frog) {
        val newBoard = board.slice(0, emptyLoc) ++ Vector(Toad, Frog, Empty) ++ board.slice(emptyLoc + 3, size)
        val newState = new PuzzleState(newBoard, emptyLoc + 2)
        return Some(newState)
      }
      else return None
    }
    catch { 
      case x: IndexOutOfBoundsException => return None       
    }
  }

  /**
   * Checks if the slideFromLeft is a legal move. Only Frogs can slide 
   * from left to right so the immediate left of the emptyLoc must be a Frog
   * 
   * Creates a new board by slicing the existing board and concatenation the 
   * new Vector of Empty then Frog since Frog has slid to the right.
   *  
   * @return the new PuzzleState as an Option
   */
  def slideFromLeft(): Option[PuzzleState] = {
    try {
      if(board(emptyLoc - 1) == Frog) {
        val newBoard = board.slice(0, emptyLoc - 1) ++ Vector(Empty, Frog) ++ board.slice(emptyLoc + 1, size)
        val newState = new PuzzleState(newBoard, emptyLoc - 1)
        return Some(newState)
      }
      else return None
    }
    catch { 
      case x: IndexOutOfBoundsException => return None       
    }
  }

  /**
   * Checks if the slideFromRight is a legal move. Only Toads can slide 
   * from right to left so the immediate right of the emptyLoc must be a Toad
   * 
   * Creates a new board by slicing the existing board and concatenation the 
   * new Vector of Toad then Empty since Toad has slid to the left.
   *  
   * @return the new PuzzleState as an Option
   */
  def slideFromRight(): Option[PuzzleState] = {
    try {
      if(board(emptyLoc + 1) == Toad) {
        val newBoard = board.slice(0, emptyLoc) ++ Vector(Toad, Empty) ++ board.slice(emptyLoc + 2, size)
        val newState = new PuzzleState(newBoard, emptyLoc + 1)
        return Some(newState)
      }
      else return None
    }
    catch { 
      case x: IndexOutOfBoundsException => return None       
    }
  }
}


/**
  * Companion object for the [[PuzzleState]] class, provides a public constructor.
  */
object PuzzleState {
  import scala.util.matching.Regex

  /**
    * Case class for case objects to represent the possible contents of a
    * cell in the puzzle.
    */
  sealed abstract class Cell {
    override def toString(): String = 
        this match {
            case Frog  => "F"
            case Toad  => "T"
            case Empty => " "
        }
  }
  case object Frog extends Cell
  case object Toad extends Cell
  case object Empty extends Cell

  /**
    * Construct a [[PuzzleState]] object in the initial state for a
    * puzzle with specified numbers of frogs and toads.
    *
    * @param frogs number of frogs to place on the left of the [[PuzzleState]]
    * to be constructed
    * @param toads number of toads top place on the right of the [[PuzzleState]]
    * to be constructed
    */
  def apply(frogs: Int, toads: Int): PuzzleState = {
    if (frogs <= 0 || frogs > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    if (toads <= 0 || toads > 10)
      throw new Exception("The number of frogs must be between 1 and 10.")

    new PuzzleState(
      Vector.fill(frogs)(Frog) ++ Vector(Empty) ++
        Vector.fill(toads)(Toad),
      frogs
    )
  }

  /*
   * Regular expression for stripping a pair of square brackets off
   * of either end of a string.
   */
  val brackReg: Regex = """^\s*\[(.*)\]\s*$""".r

  /**
    * Construct a [[PuzzleState]] object by parsing a string representation
    * consisting of the symbols `F`, `T` and `<space>` separated by pipes `|`
    * and surrounded by square brackets.
    *
    * This must contain at least one `F`, at least one `T` and exactly one
    * `<space>`, otherwise an exception is raised.
    *
    * This apply method is inverse, in the manifest sese, to the pretty
    * printing [[PuzzleState#toString]] method.
    *
    * @param str the string to be parsed into a [[PuzzleState]] object.
    */
  def apply(str: String): PuzzleState = {
    val brackReg(s) = str
    val board: Vector[Cell] = s
      .split('|')
      .map(
        s =>
          s.trim match {
            case "F" => Frog
            case "T" => Toad
            case ""  => Empty
            case s =>
              throw new Exception("Unexpected cell contents '" + s + "'.")
          }
      )
      .toVector

    if (board.count(_ == Frog) < 1)
      throw new Exception("A puzzle state must have at least one frog.")

    if (board.count(_ == Toad) < 1)
      throw new Exception("A puzzle state must have at least one toad.")

    if (board.count(_ == Empty) != 1)
      throw new Exception("A puzzle state must have one empty cell.")

    new PuzzleState(board, board.indexOf(Empty))

  }

  /**
    * Find a sequence of legal moves of the frogs and toads puzzle from a specified starting
    * [[PuzzleState]] to the terminal [[PuzzleState]].
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[PuzzleState]] objects passed through in the transit from
    * state `start` to the terminal state (inclusive). Returns the empty sequence if no solution
    * is found.
    */
  def solve(start: PuzzleState): Seq[PuzzleState] = {
    val curState = start

    if(curState.isTerminalState()) {
      return Seq(curState)
    }

    curState.jumpFromLeft() match {
      case Some(newState) =>
          solve(newState) match {
            case Seq() => //try the next move
            case list  => return Seq(curState) ++ list
          }
      case None => //try the next move
    }

    curState.jumpFromRight() match {
      case Some(newState) =>
          solve(newState) match {
            case Seq() => //try the next move
            case list  => return Seq(curState) ++ list
          }
      case None => //try the next move
    }

    curState.slideFromRight() match {
      case Some(newState) =>
          solve(newState) match {
              case Seq() => //try the next move
              case list  => return Seq(curState) ++ list
          }
      case None => //try the next move
    }

    curState.slideFromLeft() match {
        case Some(newState) =>
            solve(newState) match {
              case Seq() => //return Seq()
              case list  => return Seq(curState) ++ list
            }
        case None =>  //return Seq()
    }
    return Seq()
  }

  /**
    * Call [[solve]] to generate a sequence of legal moves from a specified
    * starting [[PuzzleState]] to the terminal [[PuzzleState]]. Render each state in that solution as
    * an image and return the resulting sequence of images.
    *
    * @param start the starting [[PuzzleState]]
    * @return the sequence of [[Image]] objects depicting the sequence of puzzle states
    * passed through in the transit from the `start` state to the terminal state.
    */
  def animate(start: PuzzleState): Seq[Image] = {
    val solution = solve(start)
    return solution.map(_.toImage())
  }

  /**
    * Create an animation of a solution to the frogs and toads puzzle, starting from the initial
    * [[PuzzleState]] and ending at the terminal [[PuzzleState]].
    * 
    * @param frogs the number of frogs in the puzzle (between 1 and 10 inclusive)
    * @param toads the number of toads in the puzzle (between 1 and 10 inclusive)
    */
  def animate(frogs: Int, toads: Int): Seq[Image] =
    animate(PuzzleState(frogs, toads))
}
