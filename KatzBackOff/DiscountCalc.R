#
# --------------------------------------------------------------------------------------
# Dicount and LeftOve Calc fo 1 to 5-grams
# These n-grams should be contained in DataTable 
# These n-grams are contained in DataTable with the following format
# 1-gram   -> term1 (character), count (integer)
# 2-gram   -> term1 (character), term2 (character), count (integer)
# 3-gram   -> term1 (character), term2 (character), term3 (character), count (integer)
# 4-gram   -> term1 (character), term2 (character), term3 (character), term4 (character), count (integer)
# 5-gram   -> term1 (character), term2 (character), term3 (character), term4 (character), term5 (character), count (integer)
# For each of this n-gram will be added a whole column to contain the value of the descounto and also will be created a dataTable of leftovers




# --------------------------------------------------------------------------------------
# Katz Backoff
#
# The following 2 routines have been adapted from the implementation contained in 
# https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR
#
# Calculating the discount

# Create a the discount column and calculate its value
discountCalc = function(dtGram,reliableCount) {
  newDtGram = cbind(dtGram,discount=rep(1, nrow(dtGram)))
  #  Only taking in count n-grams that have 0 < count <= reliableCount, 
  #  if > reliableCount => "Reliable enough".
  for(i in reliableCount:1){
    atualCount = i
    nextCount = atualCount + 1
    
    rowsWithCurrentCount = nrow(newDtGram[count == atualCount])
    rowsWithNexttCount = nrow(newDtGram[count == nextCount])
    
    theDiscount = (nextCount / atualCount) * (rowsWithNexttCount / rowsWithCurrentCount) 
    
    # updating discout for rows with count = atualCount
    newDtGram[count == atualCount, discount := theDiscount]
  } 
  newDtGram
}
#
# Calculate the remaining probability
leftOverProbCalc  = function(count, discount) {
  totalCount = sum(count)
  
  return(1-sum((count*discount)/totalCount))
}


# --------------------------------------------------------------------------------------
#
# Calculating the discount and the leftover for each ngram
# # For my application DataTables objects must have the following names
# 1-gram -> dt1gram
# 2-gram -> dt2gram
# 3-gram -> dt3gram
# 4-gram -> dt4gram
# 5-gram -> dt5gram
#---------------------------
#
# 5-gram
print(paste0("Discount and the leftover to the 5-gram"))
dt5gram=discountCalc(dt5gram,5)
lo5gram=dt5gram[, .(leftOver=leftOverProbCalc(count, discount))
                ,by=list(term1,term2,term3,term4)]
#
# 4-gram
print(paste0("Discount and the leftover to the 4-gram"))
dt4gram=discountCalc(dt4gram,5)
lo4gram=dt4gram[, .(leftOver=leftOverProbCalc(count, discount))
                ,by=list(term1,term2,term3)]
#
# 3-gram
print(paste0("Discount and the leftover to the 3-gram"))
dt3gram=discountCalc(dt3gram,5)
lo3gram=dt3gram[, .(leftOver=leftOverProbCalc(count, discount))
                ,by=list(term1,term2)]
#
# 2-gram
print(paste0("Discount and the leftover to the 2-gram"))
dt2gram=discountCalc(dt2gram,5)
lo2gram=dt2gram[, .(leftOver=leftOverProbCalc(count, discount)),by=term1]
#
# 1-gram
print(paste0("Discount and the leftover to the 1-gram"))
dt1gram=discountCalc(dt1gram,5)
lo1gram=dt1gram[, .(leftOver=leftOverProbCalc(count, discount))]
