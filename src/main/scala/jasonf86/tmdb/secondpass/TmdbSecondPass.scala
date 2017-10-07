package jasonf86.tmdb.secondpass

import slick.jdbc.H2Profile.api._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.collection.mutable.ListBuffer



// Definition of the Movies table
class Movies(tag: Tag) extends Table[(Int, String, String)](tag, "MOVIES") {
  def id = column[Int]("MOVIE_ID", O.PrimaryKey)
  def name = column[String]("MOVIE_NAME")
  def description = column[String]("MOVIE_DESC")
  def * = (id, name, description)
}
// needed for foreignKey
//val movies = TableQuery[Movies]


// Definition of the Actors table
class Actors(tag: Tag) extends Table[(Int, String, Int, String)](tag, "ACTORS") {
  def id = column[Int]("ACTOR_ID", O.PrimaryKey)
  def name = column[String]("ACTOR_NAME")
  def age = column[Int]("ACTOR_AGE")
  def birthcity = column[String]("ACTOR_CITY")
  def * = (id, name, age, birthcity)
  // A reified foreign key relation that can be navigated to create a join
  //def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
}
// needed for foreignKey
//val actors = TableQuery[Actors]


// Definition for movie to actor cross references
class ActorsInMovies(tag: Tag) extends Table[(Int, Int, Int)] (tag, "ACTORSINMOVIES") {
  def id = column[Int]("XREF_ID", O.PrimaryKey)
  def actorID = column[Int]("ACTOR_ID")
  def movieID = column[Int]("MOVIE_ID")
  def * = (id, actorID, movieID)

  // todo - having trouble with the setting up the foreignKeys with this design
  //def actor = foreignKey("ACTOR_FK", actorID , actors)(_.id)
  //def movie = foreignKey("MOVID_FK", movieID, movies)(_.id)
}


class TmdbDatabase() {
  val movies = TableQuery[Movies]
  val actors = TableQuery[Actors]
  val actorsInMovies = TableQuery[ActorsInMovies]

  val db = Database.forConfig("h2mem1")
  val setup = DBIO.seq(
    // Create the tables, including primary and foreign keys
    (movies.schema ++ actors.schema ++ actorsInMovies.schema).create,

    // Insert some movies
    movies ++= Seq(
      (1, "Die Hard", "NYPD officer tries to save his wife and others that were taken hostage"),
      (2, "Groundhog Day", "Weatherman finds himself living the same day over and over"),
      (10, "The Princess Bride", "A young boy's grandfather reads him a story"),
      (20, "Ghostbusters", "Former parapsycology professors set up a shop as ghost removal service")
    ),

    // Insert some actors
    actors ++= Seq(
      (1, "Cary Elwes", 43, "London"),
      (2, "Mandy Patinkin", 59, "Chicago, IL"),
      (30, "Bruce Willis", 75, "Idar-Oberstein"),
      (40, "Bill Murray", 77, "Wilmette, IL"),
      (50, "Dan Aykroyd", 49, "Ontario")
    ),

    // insert which movies each actor was in (not really accurate - added some extras for testing)
    //id, actor_id, movie_id
    actorsInMovies ++= Seq(
      (1, 1, 10),
      (2, 2, 10),
      (3, 30, 1),
      (4, 40, 20),
      (5, 40, 2),
      (6, 1, 20),
      (7, 50, 20),
      (8, 50, 10)
    )
  )
  val setupFuture = db.run(setup)
  val debugDump = false
  if (debugDump) {
    println("======= Debug dump of database =======")
    val resultFuture = setupFuture.flatMap { _ =>

      //#readall
      // Read all coffees and print them to the console
      println("Actors:")
      db.run(actors.result).map(_.foreach {
        case (id, name, age, birthCity) =>
          println(id + "  " + name + "\t" + age + "\t" + birthCity)
      }).flatMap { _ =>

        println("Movies:")
        db.run(movies.result).map(_.foreach {
          case (id, name, description) =>
            println(id + ": " + name + "\t" + description)
        })
      }
    }
    Await.result(resultFuture, Duration.Inf)
    println("======= Debug dump of database =======")
  } else {
    Await.result(setupFuture, Duration.Inf)
  }

  // main access methods
  def getActorsForMovie(movieName: String): List[String] = {
    val notFound:String = "<"+movieName+" not found>"

    var actorList = new ListBuffer[String]()

    // list of names of actors in this movie
    val q1 = for {
      m <- movies if m.name === movieName
      am <- actorsInMovies if am.movieID === m.id
      an <- actors if am.actorID === an.id
    } yield (an.name)
    val actorListFuture = db.run(q1.result).map(_.foreach(act =>
      actorList += act.toString
    ))
    Await.result(actorListFuture, Duration.Inf)
    if (actorList.isEmpty) actorList += notFound
    actorList.toList
  }

  def getMoviesForActor(actorName: String): List[String] = {
    val notFound:String = "<"+actorName+" not found>"

    var movieList = new ListBuffer[String]()

    // list of names of movies this actor has performed in
    val q1 = for {
      a <- actors if a.name === actorName
      am <- actorsInMovies if am.actorID === a.id
      mn <- movies if am.movieID === mn.id
    } yield (mn.name)
    val movieListFuture = db.run(q1.result).map(_.foreach(act =>
      movieList += act.toString
    ))
    Await.result(movieListFuture, Duration.Inf)
    if (movieList.isEmpty) movieList += notFound
    movieList.toList
  }

  def getActorDetails(actorName: String): String = {
    val notFound:String = "<"+actorName+" not found>"

    var myInfo: String = notFound

    val q1 = for {
      a <- actors if a.name === actorName
    } yield (a.name, a.age, a.birthcity)
    val myInfoFuture = db.run(q1.result).map(_.foreach(act =>
      myInfo = act._1 + " is " + act._2 + " years old and was born in " + act._3
    ))
    Await.result(myInfoFuture, Duration.Inf)
    myInfo
  }

  def getActorCostars(actorName: String): List[String] = {
    // list of names of actors that have been in movies with this actor
    // first get the list of movies
    val movieList: List[String] = this.getMoviesForActor(actorName)
    var costarList = new ListBuffer[String]()

    for (movie <- movieList) {
      val actors = getActorsForMovie(movie)
      costarList ++= actors
    }
    costarList = costarList.distinct
    costarList -= actorName
    costarList.toList
  }


}

object TmdbSecondPass extends App {


  val tmdb = new TmdbDatabase()
  println("\n### Quick test:")
  println("ActorsForMovie: " + tmdb.getActorsForMovie("Ghostbusters"))
  println("MoviesForActor: " + tmdb.getMoviesForActor("Cary Elwes"))
  println("DetailsForActor: " + tmdb.getActorDetails("Cary Elwes"))
  println("CostarsForActor: " + tmdb.getActorCostars("Cary Elwes"))

}

