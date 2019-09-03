/*
 * This file is part of COMP332 Assignment 1.
 *
 * Copyright (C) 2019 Dominic Verity, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the Frogs and Toads puzzle solver.
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.frogsandtoads
import doodle.core._
import doodle.syntax._
import doodle.image._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FrogsAndToadsTests extends FlatSpec with Matchers {

  import PuzzleState._

  //Testing Puzzle State Construction
  "A puzzle state with 5 frogs and 8 toads:" should
    "have 5 + 8 + 1 = 14 cells" in {
    assert(PuzzleState(5, 8).size == 14)
  }
  it should "have its empty cell at position 5" in {
    assertResult(5) {
      PuzzleState(5, 8).emptyLoc
    }
  }
  it should "be constructed in the initial puzzle state" in {
    assert(PuzzleState(5, 8).isInitialState())
  }
  it should "not be constructed in the terminal puzzle state" in {
    assert(!PuzzleState(5, 8).isTerminalState())
  }
  it should "have initial state as [F|F|F|F|F| |T|T|T|T|T|T|T|T]" in {
    assertResult("[F|F|F|F|F| |T|T|T|T|T|T|T|T]") {
      PuzzleState(5,8).toString()
    }
  }

  //Testing slide and jump
  "The puzzle state obtained by sliding a single frog" should
  "have 3 frogs, a space, a frog, and 4 toads" in {
    assertResult("[F|F|F| |F|T|T|T|T]") {
      PuzzleState(4,4).slideFromLeft.get.toString()
    }
  }
  "The puzzle state obtained by sliding a single toad" should
  "have 4 frogs, a toad, a space, and 3 toads" in {
    assertResult("[F|F|F|F|T| |T|T|T]") {
      PuzzleState(4,4).slideFromRight.get.toString()
    }
  }
  "The puzzle state obtained by making an invalid frog jump" should
  "return None" in {
    assertResult(None) {
      PuzzleState(4,4).jumpFromLeft()
    }
  }
  "The puzzle state obtained by making an invalid toad jump" should
  "return None" in {
    assertResult(None) {
      PuzzleState(4,4).jumpFromRight()
    }
  }
  "Applying jumpFromLeft to the state [F|T| |F|T]" should
  "be legal and produce the state [ |T|F|F|T]" in {
    assertResult(Some("[ |T|F|F|T]")) {
      PuzzleState("[F|T| |F|T]").jumpFromLeft().map(_.toString())
    }
  }
  "Applying jumpFromLeft to the state [F|F|F|F|T| |T|T|T]" should
  "be legal and produce the state [F|F|F| |T|F|T|T|T]" in {
    assertResult(Some("[F|F|F| |T|F|T|T|T]")) {
      PuzzleState("[F|F|F|F|T| |T|T|T]").jumpFromLeft().map(_.toString())
    }
  }
  "Applying jumpFromLeft to the state [T|F| |T|F]" should
  "be illegal" in {
    assertResult(None) {
      PuzzleState("[T|F| |T|F]").jumpFromLeft()
    }
  }
  "Applying jumpFromRight to the state [F|T| |F|T]" should
  "be legal and produce the state [F|T|T|F| ]" in {
    assertResult(Some("[F|T|T|F| ]")) {
      PuzzleState("[F|T| |F|T]").jumpFromRight().map(_.toString())
    }
  }
  "Applying jumpFromRight to the state [T|F| |T|F]" should
  "be illegal" in {
    assertResult(None) {
      PuzzleState("[T|F| |T|F]").jumpFromRight()
    }
  }

 //Solver
 "Calling the solve method on a PuzzleState" should
 "find a solution (1, 1)" in {
   assertResult(true) {
     val puzzle = solve(PuzzleState(1,1))
     val last = puzzle(puzzle.length-1)
     last.isTerminalState
   }
 }
 for(i <- 1 until 11) {
   for(k <- 1 until 11) {
    it should "find a solution(" + i +", "+ k +")" in {
      assertResult(true) {
        val puzzle = solve(PuzzleState(i,k))
        val last = puzzle(puzzle.length-1)
        last.isTerminalState
      }
    }
  }
}

 "Calling the solve method on a 4,4 PuzzleState" should
 "find a solution" in {
   assertResult("[T|T|T|T| |F|F|F|F]") {
     val sol = solve(PuzzleState(4,4)).map(_.toString())
     sol(sol.length-1)
   }
 }
 "Calling the solve method on a 5,6 PuzzleState" should
 "find a solution" in {
   assertResult("[T|T|T|T|T|T| |F|F|F|F|F]") {
     val sol = solve(PuzzleState(5,6)).map(_.toString())
     sol(sol.length-1)
   }
 }


  /*
val box = Image.rectangle(40, 40)
val image = box.fillColor(Color.green) beside box.fillColor(Color.green) beside box.fillColor(Color.white) beside box.fillColor(Color.brown) beside box.fillColor(Color.brown)
 //Animate
 "Calling the animate method on Seq[PuzzleState]" should
 "return a Seq[Images]" in {
   assertResult(image) {
     val images = animate(PuzzleState(2,2))
     images(images.length-1)
   }
 }
 */

  
}
