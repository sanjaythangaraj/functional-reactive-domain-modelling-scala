package chap5.domain.service

trait InterestPostingService[Account, Amount]
    extends InterestCalculation[Account, Amount]
    with TaxCalculation[Amount]
