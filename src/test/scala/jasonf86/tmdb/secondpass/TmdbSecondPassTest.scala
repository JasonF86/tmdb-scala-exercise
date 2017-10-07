package jasonf86.tmdb.secondpass

class TmdbSecondPassTest extends org.specs2.mutable.Specification {
  "tmdb tests" >> {
    val tmdb = new TmdbDatabase()
    "- getActorsForMovie tests" >> {
      "-- only one actor in list" >> {
        val m1 = tmdb.getActorsForMovie("Die Hard")
        m1.contains("Bruce Willis") must_== true
        m1.contains("not found") must_== false
      }
      "-- many actors in list" >> {
        val m1 = tmdb.getActorsForMovie("Ghostbusters")
        "--- Dan" >> {
          m1.contains("Dan Aykroyd") must_== true
        }
        "--- Bill" >> {
          m1.contains("Bill Murray") must_== true
        }
      }
      "-- unknown movie" >> {
        val m1 = tmdb.getActorsForMovie("Bogus")
        m1.toString() must contain("not found")
      }
    }
    "- getMoviesForActor tests" >> {
      "-- only one movie in list" >> {
        val a1 = tmdb.getMoviesForActor("Dan Aykroyd")
        a1.contains("Ghostbusters") must_== true
      }
      "-- many movies in list" >> {
        val a1 = tmdb.getMoviesForActor("Bill Murray")
        a1.contains("Groundhog Day") must_== true
        a1.contains("Ghostbusters") must_== true
      }
      "-- unknown actor" >> {
        val a1 = tmdb.getMoviesForActor("Fred Bogus")
        a1.toString() must contain("not found")
      }
    }
    "- getActorDetails tests" >> {
      "-- check city" >> {
        val a1 = tmdb.getActorDetails("Mandy Patinkin")
        a1 must contain("Chicago, IL")
      }
      "-- check age" >> {
        val a1 = tmdb.getActorDetails("Bill Murray")
        a1 must contain("77 years old")
      }
      "-- unknown actor" >> {
        val a1 = tmdb.getActorDetails("Jane Bogus")
        a1 must contain("not found")
      }
    }
    "- getActorCostars tests" >> {
      "-- many actors in list" >> {
        val a1 = tmdb.getActorCostars("Bill Murray")
        a1.contains("Dan Aykroyd") must_== true
        a1.contains("Cary Elwes") must_== true
      }
      "-- no actors in list" >> {
        val a1 = tmdb.getActorCostars("Bruce Willis")
        a1.isEmpty must_== true
      }
      "-- unknown actor" >> {
        val a1 = tmdb.getActorCostars("Sachin Bogus")
        a1.toString() must contain("not found")
      }
    }
  }

}
