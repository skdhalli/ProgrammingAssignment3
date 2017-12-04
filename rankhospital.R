rankhospital<-function(state, outcome, num = "best")
{
  outcome <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s', gsub(' ', '.',outcome))
  outcome_file <- read.csv("outcome-of-care-measures.csv")
  if(outcome %in% names(outcome_file))
  {
    outcome_file[,outcome] <- as.numeric(outcome_file[,outcome])
    require(sqldf)
    outcome <- sprintf('[%s]', outcome)
    if(num == "best")
    {
      limit <- 1
    }
    else if(num == "worst")
    {
      limit <- -1
    }
    else
    {
      limit <- num
    }
    
    sql<-sprintf('select State, [Hospital.Name], %s from outcome_file where State = "%s" and %s != "Not Available" order by cast(%s as numeric), [Hospital.Name] asc limit %s', outcome, state,outcome, outcome, limit)
    res<-sqldf(sql)
    if(nrow(res) > 0)
    {
      if(limit == nrow(res))
      {
        res[limit,]
      }
      else if(limit == -1)
      {
        res[nrow(res),]
      }
      else if(limit == nrow(res))
      {
        print("Unhandled scenario")
      }
      else
      {
        print("NA")
      }
    }
    else
    {
      sprintf('Error in rankhospital("%s", "%s") : invalid state', state, outcome)
    }
  }
  else
  {
    sprintf('Error in rankhospital("%s", "%s") : invalid outcome', state, outcome)
  }
}