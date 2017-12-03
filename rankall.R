rankall<-function(outcome, num = "best")
{
  if(getwd() != "/Users/saidhalli/Downloads/rprog-data-ProgAssignment3-data")
  {
    setwd("/Users/saidhalli/Downloads/rprog-data-ProgAssignment3-data")
  }
  outcome <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s', gsub(' ', '.',outcome))
  outcome_file <- read.csv("outcome-of-care-measures.csv")
  if(outcome %in% names(outcome_file))
  {
    require(dplyr)
    if(num == "best")
    {
      rank <- 1
      outcome_file <- outcome_file %>% arrange(State, outcome_file[[outcome]]) %>% group_by(State) %>% mutate(InnerRank=row_number())
    }
    else if(num == "worst")
    {
      rank <- 1
      outcome_file <- outcome_file %>% arrange(State, -outcome_file[[outcome]]) %>% group_by(State) %>% mutate(InnerRank=row_number())
      
    }
    else
    {
      rank <- num
      outcome_file <- outcome_file %>% arrange(State, outcome_file[[outcome]]) %>% group_by(State) %>% mutate(InnerRank=row_number())
    }
    
    require(sqldf)
    outcome <- sprintf('[%s]', outcome)
    #outcome_file
    sql<-sprintf('select State, [Hospital.Name] from outcome_file where InnerRank = %d and %s != "Not Available"',rank,outcome)
    sqldf(sql)
  }
  else
  {
    print("NA")
  }
}