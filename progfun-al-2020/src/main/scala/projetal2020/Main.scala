package projetal2020

import better.files._

import play.api.libs.json.Json

object Main extends App {

  val f = File("./progTondeuse")
  val content = f.lines.toList

  val grid = Programme.parseGrid(content.headOption.getOrElse("4 4"))

  val t = Programme.position(content.drop(1))

  val result = t.map(res => {
    (Programme.moove(res, grid), res)
  })

  val jsonFile = File("./result.json")

  jsonFile
    .createIfNotExists()
    .overwrite(
      Json.prettyPrint(
        Json.toJson(
          Json.obj(
            "limite" -> Json.obj(
              "x" -> grid.width,
              "y" -> grid.height
            ),
            "tondeuses" -> result.map(
              res =>
                Json.obj(
                  "debut" -> Json.obj(
                    "point" -> Json.obj(
                      "x" -> res._2.x,
                      "y" -> res._2.y
                    ),
                    "direction" -> res._2.o
                  ),
                  "instructions" -> res._2.action,
                  "fin" -> Json.obj(
                    "point" -> Json.obj(
                      "x" -> res._1.x,
                      "y" -> res._1.y
                    ),
                    "direction" -> res._1.orientation
                  )
                )
            )
          )
        )
      )
    )

}

object Programme {
  def parseGrid(grid: String): Grid = {
    val splitGrid = grid
      .split(" ")
      .map(elem => {
        elem.toInt
      })
    new Grid(splitGrid(0), splitGrid(1))
  }

  def position(tondeuses: List[String]): List[Tondeuse] = {
    val t = List[Tondeuse]()
    positionOne(t, tondeuses)
  }

  def positionOne(result: List[Tondeuse], input: List[String]): List[Tondeuse] =
    (input.headOption, input.drop(1).headOption) match {
      case (Some(first), Some(second)) => {
        val f = first.split(" ")
        val s = second.split("")
        positionOne(
          result :+ new Tondeuse(f(0).toInt, f(1).toInt, f(2), s.toList),
          input.drop(2)
        )
      }
      case _ => result
    }
  
  def moove(tondeuse: Tondeuse, grid: Grid): Coordonne = {
    mooveCoordonne(
      new Coordonne(tondeuse.x, tondeuse.y, tondeuse.o),
      tondeuse.action,
      grid
    )
  }

  def mooveCoordonne(
      c: Coordonne,
      instruction: List[String],
      grid: Grid
  ): Coordonne =
    instruction match {
      case value :: rest if value == "A" =>
        mooveCoordonne(stayPosition(c, newPosition(c, value), grid), rest, grid)
      case value :: rest => mooveCoordonne(newOrientation(c, value), rest, grid)
      case _             => c
    }

  def newOrientation(c: Coordonne, instruction: String): Coordonne =
    (c, instruction) match {
      case _ if instruction == "G" && c.orientation == "N" =>
        new Coordonne(c.x, c.y, "W")
      case _ if instruction == "G" && c.orientation == "W" =>
        new Coordonne(c.x, c.y, "S")
      case _ if instruction == "G" && c.orientation == "S" =>
        new Coordonne(c.x, c.y, "E")
      case _ if instruction == "G" && c.orientation == "E" =>
        new Coordonne(c.x, c.y, "N")

      case _ if instruction == "D" && c.orientation == "N" =>
        new Coordonne(c.x, c.y, "E")
      case _ if instruction == "D" && c.orientation == "E" =>
        new Coordonne(c.x, c.y, "S")
      case _ if instruction == "D" && c.orientation == "S" =>
        new Coordonne(c.x, c.y, "W")
      case _ if instruction == "D" && c.orientation == "W" =>
        new Coordonne(c.x, c.y, "N")
    }

  def newPosition(c: Coordonne, instruction: String): Coordonne =
    (c, instruction) match {
      case _ if c.orientation == "N" => Coordonne(c.x, c.y + 1, c.orientation)
      case _ if c.orientation == "S" => Coordonne(c.x, c.y - 1, c.orientation)
      case _ if c.orientation == "W" => Coordonne(c.x - 1, c.y, c.orientation)
      case _ if c.orientation == "E" => Coordonne(c.x + 1, c.y, c.orientation)
    }

  def stayPosition(
      current: Coordonne,
      newCoordonne: Coordonne,
      grid: Grid
  ): Coordonne = newCoordonne match {
    case _ if newCoordonne.y > grid.height || newCoordonne.y < 0 => current
    case _ if newCoordonne.x > grid.width || newCoordonne.x < 0  => current
    case _                                                       => newCoordonne
  }

}

case class Grid(width: Int, height: Int)

case class Tondeuse(x: Int, y: Int, o: String, action: List[String])

case class Coordonne(x: Int, y: Int, orientation: String)
