package jasonf86.tmdb.firstpass

import slick.jdbc.H2Profile.api._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.collection.mutable.ListBuffer

object TmdbFirstPass extends App {


  // Definition of the Movies table
  class Movies(tag: Tag) extends Table[(Int, String, String)](tag, "MOVIES") {
    def id = column[Int]("MOVIE_ID", O.PrimaryKey)
    def name = column[String]("MOVIE_NAME")
    def description = column[String]("MOVIE_DESC")
    def * = (id, name, description)
  }
  val movies = TableQuery[Movies]


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
  val actors = TableQuery[Actors]

  // Definition for movie to actor cross references
  class ActorsInMovies(tag: Tag) extends Table[(Int, Int, Int)] (tag, "ACTORSINMOVIES") {
    def id = column[Int]("XREF_ID", O.PrimaryKey)
    def actorID = column[Int]("ACTOR_ID")
    def movieID = column[Int]("MOVIE_ID")
    def * = (id, actorID, movieID)

    def actor = foreignKey("ACTOR_FK", actorID , actors)(_.id)
    def movie = foreignKey("MOVID_FK", movieID, movies)(_.id)
  }
  val actorsInMovies = TableQuery[ActorsInMovies]


  class Movie(myMovieName: String, db: Database) {
    val movieName = myMovieName
    private val notFound:String = "<"+myMovieName+" not found>"

    def getMyInfo(): String = {
      // some details about this movie in a simple string
      val q1 = for {
        m <- movies if m.name === myMovieName
      } yield (m.name, m.description)
      var myInfo: String = notFound
      val myInfoFuture = db.run(q1.result).map(_.foreach(mm =>
          myInfo = "Name:" + mm._1 + " Desc: " + mm._2
      ))
      Await.result(myInfoFuture, Duration.Inf)
      myInfo
    }

    def getActors(): List[String] = {
      // list of names of actors in this movie
      val q1 = for {
        m <- movies if m.name === myMovieName
        am <- actorsInMovies if am.movieID === m.id
        an <- actors if am.actorID === an.id
      } yield (an.name)
      var actorList = new ListBuffer[String]()
      val actorListFuture = db.run(q1.result).map(_.foreach(act =>
        actorList += act.toString
      ))
      Await.result(actorListFuture, Duration.Inf)
      if (actorList.isEmpty) actorList += notFound
      actorList.toList
    }

  }

  class Actor(myActorName: String, db: Database) {
    val actorName = myActorName
    private val notFound:String = "<"+actorName+" not found>"

    def getMovies(): List[String] = {
      // list of names of movies this actor has performed in
      val q1 = for {
        a <- actors if a.name === myActorName
        am <- actorsInMovies if am.actorID === a.id
        mn <- movies if am.movieID === mn.id
      } yield (mn.name)
      var movieList = new ListBuffer[String]()
      val movieListFuture = db.run(q1.result).map(_.foreach(act =>
        movieList += act.toString
      ))
      Await.result(movieListFuture, Duration.Inf)
      if (movieList.isEmpty) movieList += notFound
      movieList.toList
    }


    def getCostars(): List[String] = {
      // list of names of actors that have been in movies with this actor
      // first get the list of movies
      val movieList: List[String] = this.getMovies()
      var costarList = new ListBuffer[String]()
      for (movie <- movieList) {
        val m1 = new Movie(movie,db)
        val actors = m1.getActors()
        costarList ++= actors
      }
      costarList = costarList.distinct
      costarList -= myActorName
      costarList.toList
     }


    def getMyInfo(): String = {
      val q1 = for {
        a <- actors if a.name === myActorName
      } yield (a.name, a.age, a.birthcity)
      var myInfo: String = notFound
      val myInfoFuture = db.run(q1.result).map(_.foreach(act =>
        myInfo = act._1 + " is " + act._2 + " years old and was born in " + act._3
      ))
      Await.result(myInfoFuture, Duration.Inf)
      myInfo
    }

  }


  val db = Database.forConfig("h2mem1")
  try {

    val setup = DBIO.seq(
      // Create the tables, including primary and foreign keys
      (movies.schema ++ actors.schema ++ actorsInMovies.schema).create,

      // Insert some movies
      movies += (1, "Die Hard", "NYPD officer tries to save his wife and others that were taken hostage"),
      movies += (2, "Groundhog Day", "Weatherman finds himself living the same day over and over"),
      movies += (10, "The Princess Bride", "A young boy's grandfather reads him a story"),
      movies += (20, "Ghostbusters", "Former parapsycology professors set up a shop as ghost removal service"),

      // Insert some actors
      actors ++= Seq(
        (1, "Cary Elwes",         43, "London"),
        (2, "Mandy Patinkin",     59, "Chicago, IL"),
        (30, "Bruce Willis",      75, "Idar-Oberstein"),
        (40, "Bill Murray",       77, "Wilmette, IL"),
        (50, "Dan Aykroyd",       49, "Ontario")
      ),

      //               id, actor_id, movie_id
      actorsInMovies += (1, 1, 10),
      actorsInMovies += (2, 2, 10),
      actorsInMovies += (3, 30, 1),
      actorsInMovies += (4, 40, 20),
      actorsInMovies += (5, 40, 2),
      actorsInMovies += (6, 1, 20),
      actorsInMovies += (7, 50, 20),
      actorsInMovies += (8, 50, 10),
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

    println("\n### Movie test:")
    val m1 = new Movie("Ghostbusters", db)
    println(m1.getMyInfo())
    // leaving output in List format to show actual return value
    println("actors in " + m1.movieName + ": " + m1.getActors())


    println("\n###  Actor test:")
    val a1 = new Actor("Cary Elwes", db)
    println(a1.getMyInfo)
    // leaving output in List format to show actual return value
    println(a1.actorName + " has been in : " + a1.getMovies())
    println(a1.actorName + " has costared with : " + a1.getCostars())


    // todo - move these into test directory as test cases
    println("\n### test unknown movie")
    val m2 = new Movie("Aliens", db)
    println(m2.getMyInfo())
    println("actors in " + m2.movieName + ": " + m2.getActors())


    println("\n### test unknown actor")
    val a2 = new Actor("Fred Jones", db)
    println(a2.getMyInfo)
    println(a2.actorName + " has been in : " + a2.getMovies())



  } finally db.close

}
