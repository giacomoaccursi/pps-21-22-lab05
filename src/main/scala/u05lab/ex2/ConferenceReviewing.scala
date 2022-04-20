package u05lab.ex2

object ConferenceRevs:
  enum Question:
    case Relevance, Significance, Confidence, Final

  trait ConferenceReviewing:
    def loadReview(article: Int, scores: Map[Question, Int]): Unit
    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
    def orderedScores(article: Int, question: Question): List[Int]
    def averageFinalScore(article: Int): Double
    def acceptedArticles: Set[Int]
    def sortedAcceptedArticles: List[(Int, Double)]
    def averageWeightedFinalScoreMap: Map[Int, Double]

  object ConferenceReviewing:
    def apply(): ConferenceReviewing = ConferenceReviewingImpl()
    private class ConferenceReviewingImpl extends ConferenceReviewing:
      import ConferenceRevs.Question.*

      var revs: List[(Int, Map[Question, Int])] = List()

      override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
        if scores.size < Question.values.length then throw IllegalArgumentException() else revs = (article, scores) :: revs

      override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
        revs = (article, Map(Relevance -> relevance, Significance -> significance, Confidence -> confidence, Final -> fin)) :: revs

      override def orderedScores(article: Int, question: Question): List[Int] =
        getScore(article, question).sorted

      override def averageFinalScore(article: Int): Double =
        avg(getScore(article, Question.Final))

      override def acceptedArticles: Set[Int] =
        def _accepted(article: Int): Boolean =
          averageFinalScore(article) > 5.0 &&
            revs.collect({case (a, q) if a == article => q.get(Relevance).get}).exists(_ >= 8)
        revs.map(_._1).toSet.filter(_accepted(_))

      override def sortedAcceptedArticles: List[(Int, Double)] =
        acceptedArticles.toList.map(art => (art,  averageFinalScore(art))).sortBy((a, b) => b)

      override def averageWeightedFinalScoreMap: Map[Int, Double] =
        // Method nested because used only here
        def _averageWeightedFinalScore(art: Int): Double =
          avg(revs.collect({ case (a, q) if a == art => q.get(Final).get * q.get(Confidence).get / 10.0}))
        revs.map(_._1).distinct.map(a => (a, _averageWeightedFinalScore(a))).toMap

      //generic implementation of avg over list of numerics, found on stackoverflow
      def avg[T](list: Iterable[T] )(implicit num: Numeric[T]): Double =
        num.toDouble(list.sum) / list.size

      def getScore(article: Int, question: Question): List[Int] = revs.collect({case (a, q) if a == article => q.get(question).get})