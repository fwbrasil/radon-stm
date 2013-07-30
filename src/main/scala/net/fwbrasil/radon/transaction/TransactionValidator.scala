package net.fwbrasil.radon.transaction

import net.fwbrasil.radon.ref.Ref

abstract class TransactionValidator extends RefSnapshooter {

    protected def isAnOutdatedSnapshot(ref: Ref[Any], snapshot: RefSnapshot) = {
        val originalContent = snapshot.originalContent
        (originalContent.writeTimestamp != ref.refContent.writeTimestamp
            && originalContent.readTimestamp != ref.refContent.readTimestamp)
    }

    protected def validateWrite(ref: Ref[Any]) =
        retryIfTrue(
            isRefReadAfterTheStartOfTransaction(ref),
            List(ref))

    protected def validateRead(ref: Ref[Any]) =
        retryIfTrue(
            isRefWroteAfterTheStartOfTransaction(ref) ||
                isAnOutdatedSnapshot(ref, getSnapshot(ref, false)),
            List(ref))

    protected def validateDestroyed(ref: Ref[Any]) = 
        if (ref.refContent.destroyedFlag) 
            if (isRefWroteAfterTheStartOfTransaction(ref))
                context.retry(ref)
            else
                throw new IllegalStateException("A destroyed ref was used.")

    protected def validateContext(ref: Ref[Any]) =
        if (ref.context != context)
            throw new IllegalStateException("Ref is from another context!")

    protected def validateConcurrentRefCreation(ref: Ref[Any]) =
        retryIfTrue(isRefCreatingInAnotherTransaction(ref), List(ref))

    private[this] def isRefReadAfterTheStartOfTransaction(ref: Ref[Any]) =
        ref.refContent.readTimestamp > startTimestamp

    private[this] def isRefCreatingInAnotherTransaction(ref: Ref[Any]) =
        ref.isCreating && ref.creationTransactionId != transactionId

    private[this] def isRefWroteAfterTheStartOfTransaction(ref: Ref[Any]) =
        ref.refContent.writeTimestamp > startTimestamp

    protected def retryIfTrue(condition: Boolean, refs: => List[Ref[_]]) =
        if (condition)
            context.retry(refs)
}