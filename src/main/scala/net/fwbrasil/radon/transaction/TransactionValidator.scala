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
            isRefReadAfterTheStartOfTransaction(ref) ||
                isRefDestroyedAfterTheStartOfTransaction(ref),
            List(ref))

    protected def validateRead(ref: Ref[Any]) =
        retryIfTrue(
            (isRefWroteAfterTheStartOfTransaction(ref) ||
                isAnOutdatedSnapshot(ref, getSnapshot(ref, false))),
            List(ref))

    protected def validateContext(ref: Ref[Any]) =
        if (ref.context != context)
            throw new IllegalStateException("Ref is from another context!")

    protected def validateConcurrentRefCreation(ref: Ref[Any]) =
        retryIfTrue(isRefCreatingInAnotherTransaction(ref), List(ref))

    private[this] def isRefReadAfterTheStartOfTransaction(ref: Ref[Any]) =
        ref.refContent.readTimestamp > startTimestamp

    private[this] def isRefCreatingInAnotherTransaction(ref: Ref[Any]) =
        ref.isCreating && ref.creationTransaction != this &&
            (ref.creationTransaction match {
                case nested: NestedTransaction =>
                    nested.rootTransaction == this
                case normal =>
                    false
            })

    private[this] def isRefWroteAfterTheStartOfTransaction(ref: Ref[Any]) =
        ref.refContent.writeTimestamp > startTimestamp

    private[this] def isRefDestroyedAfterTheStartOfTransaction(ref: Ref[Any]) =
        isRefWroteAfterTheStartOfTransaction(ref) && ref.destroyedFlag

    protected def retryIfTrue(condition: Boolean, refs: => List[Ref[_]])(implicit ctx: TransactionContext) =
        if (condition)
            ctx.retry(refs)
}