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

  
}
