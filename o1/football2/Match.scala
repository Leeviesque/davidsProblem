package o1.football2

import scala.collection.mutable.Buffer


/** The class `Match` represents match results in a football match statistics program.
  * A match is played between teams from two clubs: a home club and an away club.
  * Goals scored by players of either team can be added to the match object with the
  * method `addGoal`.
  *
  * The class is expected to be used so that a match object with no goals is initially
  * created as a real-life match starts. Goals are added incrementally as the match
  * progresses. (A match object has mutable state.)
  *
  * @param home  the club whose team plays at home in the match
  * @param away  the club whose team plays away in the match */
class Match(val home: Club, val away: Club):

  private val homeScorers = Buffer[Player]()    // container: goalscorers of the home team are added here
  private val awayScorers = Buffer[Player]()    // container: goalscorers of the away team are added here
  

  def winnerName =
    if this.goalDifference < 0 then
      this.away.name
    else if this.goalDifference > 0 then
      this.home.name
    else
      "no winner"

  def winningScorerName: String =
    if goalDifference == 0 then
      "no winning goal"
    else if goalDifference > 0 then
      homeScorers(awayGoals).name
    else if goalDifference < 0 then
      awayScorers(homeGoals).name
    
  def addGoal(scorer: Player): Unit =
    if scorer.employer == home then
      homeScorers += scorer
    else awayScorers += scorer

  def awayGoals = awayScorers.size
  def homeGoals = homeScorers.size
  def allScorers: Vector[Player] = (homeScorers ++ awayScorers).toVector
  def goalDifference =
    homeGoals - awayGoals
  def hasScorer(possibleScorer: Player) =
    allScorers.contains(possibleScorer)
  def isAwayWin = awayGoals > homeGoals 
  def isGoalless = totalGoals == 0
  def isHigherScoringThan(anotherMatch: Match) = this.totalGoals > anotherMatch.totalGoals 
  def isHomeWin = homeGoals > awayGoals 
  def isTied = awayGoals == homeGoals 
  def location = home.stadium
  override def toString =
  if this.isGoalless then
    "" + this.home + " vs. " + this.away + " at " + this.home.stadium + ": tied at nil-nil"
  else if this.homeGoals == this.awayGoals then
    "" + this.home + " vs. " + this.away + " at " + this.home.stadium + ": tied at " + this.homeGoals + "-all"
  else if this.homeGoals > this.awayGoals then
    "" + this.home + " vs. " + this.away + " at " + this.home.stadium + s": ${this.homeGoals}-${this.awayGoals} to " + this.home
  else "" + this.home + " vs. " + this.away + " at " + this.home.stadium + s": ${this.awayGoals}-${this.homeGoals} to " + this.away
  def totalGoals = awayGoals + homeGoals

end Match

