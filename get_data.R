load('data.rda')

unintended <- rd[rd$Break_Out == 'Unintended',]
intended <- rd[rd$Break_Out == 'Intended',]


unintended.bp <- droplevels(unintended[unintended$QuestionId == 'QUO130',])
intended.bp <- droplevels(intended[intended$QuestionId == 'QUO130',])

untab <- aggregate(unintended.bp$Sample_Size, by = list(Response = unintended.bp$Response), FUN = sum)
tab <- aggregate(intended.bp$Sample_Size, by = list(Response = intended.bp$Response), FUN = sum)

