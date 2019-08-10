package week9

import week10.Publisher

class BankAccount extends Publisher {
  val balance = Var(0)

  def currentBalance: Int = balance

  def deposit(amount: Int) = {
    if (amount > 0) {
      balance = balance + amount
    }
  }

  def withdraw(amount: Int) = {
    if (0 < amount && amount <= balance) {
      balance = balance - amount
    }
    else throw new Error("Insufficient funds")
  }
}
