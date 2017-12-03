best<-function(state, outcome)
{
  outcome <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s', gsub(' ', '.',outcome))
  outcome_file <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  if(outcome %in% names(outcome_file))
  {
      require(sqldf)
      outcome <- sprintf('[%s]', outcome)
      sql<-sprintf('select [Hospital.Name] from outcome_file where State = "%s" order by %s, [Hospital.Name] limit 1', state, outcome)
      res<-sqldf(sql)
      if(nrow(res) > 0)
      {
        res[1,1]
      }
      else
      {
        sprintf('Error in best("%s", "%s") : invalid state', state, outcome)
      }
  }
  else
  {
    sprintf('Error in best("%s", "%s") : invalid outcome', state, outcome)
  }
}