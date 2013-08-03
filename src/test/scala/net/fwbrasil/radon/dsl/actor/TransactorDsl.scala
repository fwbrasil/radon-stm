package net.fwbrasil.radon.dsl.actor

import net.fwbrasil.radon.transaction.TransactionContext

abstract class TransactorMessage[A]() extends ActorMessage[A]
case class CommitMessage() extends TransactorMessage[Unit]
case class RollbackMessage() extends TransactorMessage[Unit]
case class StartTransactionIfNotStartedMessage() extends TransactorMessage[Unit]

class ExecutorTransactor(override val oneActorPerThread: Boolean)(implicit context: TransactionContext) extends ExecutorActor(oneActorPerThread) {

    import context._

    val transaction = new Transaction

    override def processExecuteMessage(execute: ExecuteMessage[_]) =
        transactional(transaction) {
            super.processExecuteMessage(execute)
        }

    override def processOtherMessage[A](other: ActorMessage[A]): Unit =
        other match {
            case commit: CommitMessage =>
                processWithReply(transaction.commit)
            case rollback: RollbackMessage =>
                processWithReply(transaction.rollback)
            case start: StartTransactionIfNotStartedMessage =>
                processWithReply(transaction.startIfNotStarted)
        }

    def commit =
        syncExec(CommitMessage())

    def rollback =
        syncExec(RollbackMessage())

    def startTransactionIfNotStarted =
        syncExec(StartTransactionIfNotStartedMessage())

}

class TransactorDsl(implicit context: TransactionContext) extends AbstractActorDsl[ExecutorTransactor] {
    def newActor = new ExecutorTransactor(oneActorPerThread)
}

trait OneTransactorPerThread extends AbstractOneActorPerThread[ExecutorTransactor]
trait ManyTransactors extends AbstractManyActors[ExecutorTransactor]
trait TwoTransactors extends AbstractTwoActors[ExecutorTransactor]
