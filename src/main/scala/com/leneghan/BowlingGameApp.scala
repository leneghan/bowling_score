package com.leneghan

import scala.Predef._

object BowlingGameApp {
  val TotalFrames = 10

  def main(args : Array[String]) {

    if(args.length == 0){
      println("Args should contain players names")
    }
    println("Enter ball scores one at a time after being prompted with player name")

    val players = args.map(new BowlingGame(_))

    (1 to TotalFrames).foreach{ f =>
      players foreach {p =>
        println("[%d] %s".format(f, p.name))
        if (f == 10)
          addFinalFrame(p)
        else
          addFrame(p)
      }
      players foreach{
        p => println("%s - %s - TOTAL %d".format(p.name, p.prettyPrint, p.total))
      }
      println("Team Total %s".format(players.foldLeft(0)((acc,p)=> acc + p.total())) )
    }

    println("The winner is %s".format(findWinner(players)))

  }

  def findWinner(players: Array[BowlingGame]) = players.maxBy(_.total()).name

  def addFrame(game: BowlingGame) {
    val first = readBall()
    val second = readBall()
    var reply = game.addFrame(first, second)
    while (reply.isLeft){
      println(reply.left.get)
      val first = readBall()
      val second = readBall()
      reply = game.addFrame(first, second)
    }
  }

  def addFinalFrame(game: BowlingGame) {
    val first = readBall()
    val second = readBall()
    val third = readBall()
    var reply = game.addFinalFrame(first, second, third)
    while (reply.isLeft){
      println(reply.left.get)
      val first = readBall()
      val second = readBall()
      val third = readBall()
      reply = game.addFinalFrame(first, second, third)
    }
  }

  def readBall() = {
    var ball = Console.readInt()
    while (ball > 10){
      println("Ball must be under 10")
      ball = Console.readInt()
    }
    ball
  }
}
