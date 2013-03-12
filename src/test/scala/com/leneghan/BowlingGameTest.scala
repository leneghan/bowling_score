package com.leneghan

import org.scalatest.FunSuite
import org.scalatest.matchers.{ShouldMatchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BowlingGameTest extends FunSuite with ShouldMatchers {

  test("given no special score when given two scores should add them"){
    val game = new BowlingGame("")
    game.addFrame(6,3)
    game.addFrame(7,2)

    game.total should equal(18)
  }

  test("given a half strike when given two scores should add them doubling the first score"){
    val game = new BowlingGame("")
    game.addFrame(6,4)
    game.addFrame(7,2)

    game.total should equal(26)
  }

  test("given a strike when given two scores should add them after doubling the second score"){
    val game = new BowlingGame("")
    game.addFrame(10)
    game.addFrame(7,2)

    game.total should equal(28)
  }

  test("given a strike when last ball in partial game should ignore score"){
    val game = new BowlingGame("")
    game.addFrame(10)
    game.addFrame(7,2)
    game.addFrame(10)

    game.total should equal(28)
  }

  test("given a perfect game should calculate a score of 300"){
    perfectGame.total should equal(300)
  }

  test("given a partial game when 5 frames complete should calculate frames"){
    createPartialGame.totalFrames should equal(5)
  }

  test("given a perfect game should calculate a total frames of 10"){
    perfectGame.totalFrames should equal(10)
  }

  test("given a partial game should print scores"){
    createPartialGame.formattedFrames.fold("")((x,y)=>x + y) should equal("[X][7,2][0,0][0,/][4,/]")
  }

  test("given a perfect game should print frames"){
    perfectGame.formattedFrames.fold("")((x,y)=>x + y) should equal("[X][X][X][X][X][X][X][X][X][X,X,X]")
  }

  test("given a partial game should pretty print frames"){
    createPartialGame.prettyPrint should equal("([X],19)([7,2],9)([0,0],0)([0,/],14)([4,/],10)")
  }

  def createPartialGame
   = new BowlingGame("")
    .addFrame(10).right.get
    .addFrame(7, 2).right.get
    .addFrame(0, 0).right.get
    .addFrame(0, 10).right.get
    .addFrame(4, 6).right.get

  def perfectGame(): BowlingGame ={
    val game = new BowlingGame("")
    game.addFrame(10)
    game.addFrame(10)
    game.addFrame(10)
    game.addFrame(10)
    game.addFrame(10)
    game.addFrame(10)
    game.addFrame(10)
    game.addFrame(10)
    game.addFrame(10)
    game.addFinalFrame(10, 10, 10)
    game
  }


}

