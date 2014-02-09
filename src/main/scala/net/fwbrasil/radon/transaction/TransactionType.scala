package net.fwbrasil.radon.transaction

sealed trait TransactionType {
    val validateCommit: Boolean
    val validateReads: Boolean
}
case class ReadOnly(validateCommit: Boolean = true, validateReads: Boolean = true) extends TransactionType
case class ReadWrite(validateCommit: Boolean = true, validateReads: Boolean = true) extends TransactionType