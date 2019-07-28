package week9

class BankAccount {
  private var balance = 0

  def deposit(amount: Int) = {
    if (amount > 0) balance = balance + amount
  }

  def withdraw(amount: Int) = {
    if (0 < amount && amount <= balance) {
      balance = balance - amount
    }
    else throw new Error("Insufficient funds")
  }
}
