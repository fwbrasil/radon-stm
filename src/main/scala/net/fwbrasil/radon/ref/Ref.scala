package net.fwbrasil.radon.ref

import java.util.concurrent.locks._
import net.fwbrasil.radon.transaction.TransactionContext
import net.fwbrasil.radon.transaction.Transaction
import net.fwbrasil.radon.transaction.NestedTransaction
import scala.collection.mutable.WeakHashMap
import java.util.Collections
import scala.collection.mutable.SynchronizedMap
import net.fwbrasil.radon.util.Lockable

trait Source[+T] {
    def unary_! = get.getOrElse(null.asInstanceOf[T])
    def get: Option[T]
}

trait Sink[-T] {
    def :=(value: T) = put(Option(value))
    def put(value: Option[T]): Unit
}

object notifyingFlag

class Ref[T](pValueOption: Option[T], initialize: Boolean)(implicit val context: TransactionContext)
    extends Source[T] with Sink[T] with Lockable with java.io.Serializable {

    import context.transactionManager._

    def this(pValue: T)(implicit context: TransactionContext) = this(Option(pValue), true)
    def this(pValueOption: Option[T])(implicit context: TransactionContext) = this(pValueOption, true)
    def this()(implicit context: TransactionContext) = this(None, true)

    private[this] var _refContent: RefContent[T] = new RefContent(None, 0l, 0l, false)

    @transient
    private[this] var _weakListenersMap: WeakHashMap[RefListener[T], Int] = _

    def weakListenersMap = {
        if (_weakListenersMap == null)
            _weakListenersMap = new WeakHashMap[RefListener[T], Int] with SynchronizedMap[RefListener[T], Int]
        else
            _weakListenersMap
        _weakListenersMap
    }

    @transient
    private[radon] val (creationTransactionId, creationTransactionIsTransient) = {
        val transaction =
            getRequiredTransaction match {
                case nested: NestedTransaction =>
                    nested.rootTransaction
                case normal =>
                    normal
            }
        (transaction.transactionId, transaction.transient)
    }

    def getRequiredTransaction =
        getRequiredActiveTransaction

    def getTransaction =
        getActiveTransaction

    if (initialize)
        put(pValueOption, getRequiredTransaction)

    def refContent =
        _refContent

    private[fwbrasil] def setRefContent(pValue: Option[T]): Unit = {
        val content = this.refContent
        setRefContent(pValue, content.readTimestamp, content.writeTimestamp, content.destroyedFlag)
    }

    private[fwbrasil] def destroyInternal =
        setRefContent(None, readTimestamp, writeTimestamp, true)

    private[fwbrasil] def setRefContent(
        pValue: Option[T],
        pReadTimestamp: Long,
        pWriteTimestamp: Long,
        pDestroyedFlag: Boolean): Unit =
        _refContent = new RefContent[T](pValue, pReadTimestamp, pWriteTimestamp, pDestroyedFlag)

    private[radon] def readTimestamp = refContent.readTimestamp
    private[radon] def writeTimestamp = refContent.writeTimestamp

    def isCreating =
        writeTimestamp == 0 &&
            creationTransactionId != 0 &&
            !creationTransactionIsTransient

    def get: Option[T] =
        getTransaction.map { transaction =>
            nestTransactionIfHasListeners {
                val result = transaction.get(this)
                if (_weakListenersMap != null)
                    for (listener <- _weakListenersMap.keys)
                        listener.notifyGet(this)
                result
            }
        }.getOrElse(refContent.value)

    def getOriginalValue: Option[T] =
        getRequiredTransaction.getOriginalValue(this)

    def put(pValue: Option[T], pTransaction: => Transaction): Unit =
        nestTransactionIfHasListeners {
            val value = if (pValue == null) None else pValue
            pTransaction.put(this, value)
            if (_weakListenersMap != null)
                for (listener <- _weakListenersMap.keys)
                    listener.notifyPut(this, value)
        }

    private def nestTransactionIfHasListeners[R](f: => R) = {
        if (_weakListenersMap == null)
            f
        else {
            import context._
            def transaction = transactionManager.getRequiredActiveTransaction
            if (!transaction.attachments.contains(notifyingFlag))
                transactional(nested) {
                    transaction.attachments += notifyingFlag
                    f
                }
            else
                f
        }
    }

    def put(pValue: Option[T]): Unit =
        put(pValue, getRequiredTransaction)

    def destroy: Unit =
        getRequiredTransaction.destroy(this)

    def isDestroyed: Boolean =
        getRequiredTransaction.isDestroyed(this)

    def isDirty: Boolean =
        getRequiredTransaction.isDirty(this)

    def addWeakListener(listener: RefListener[T]) =
        weakListenersMap += (listener -> listener.hashCode())

    protected def snapshot =
        if (getTransaction.isDefined)
            get
        else
            _refContent.value

    protected def toStringSnapshot =
        if (getTransaction.isDefined) {
            if (!isDestroyed)
                get
            else
                Option("destroyed")
        } else if (!_refContent.destroyedFlag)
            _refContent.value
        else
            Option("destroyed")

    override def toString =
        "Ref(" + toStringSnapshot + ")"
}

case class RefContent[T](
    value: Option[T],
    readTimestamp: Long,
    writeTimestamp: Long,
    destroyedFlag: Boolean)

trait RefContext {

    import language.implicitConversions

    type Ref[T] = net.fwbrasil.radon.ref.Ref[T]

    implicit def refToValue[A](ref: Ref[A]): A =
        if (ref == null)
            null.asInstanceOf[A]
        else !ref

    implicit def valueToRef[A](value: A)(implicit context: TransactionContext): Ref[A] =
        new Ref(value)
}