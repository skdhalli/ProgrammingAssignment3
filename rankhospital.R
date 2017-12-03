rankhospital<-function(state, outcome, num = "best")
{
  outcome <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s', gsub(' ', '.',outcome))
  outcome_file <- read.csv("outcome-of-care-measures.csv")
  if(outcome %in% names(outcome_file))
  {
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
    
    sql<-sprintf('select [Hospital.Name] from outcome_file where State = "%s" and %s != "Not Available" order by cast(%s as numeric), [Hospital.Name] asc limit %s', state,outcome, outcome, limit)
    res<-sqldf(sql)
    if(nrow(res) > 0)
    {
      if(limit == nrow(res))
      {
        res[limit,1]
      }
      else if(limit == -1)
      {
        res[nrow(res),1]
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