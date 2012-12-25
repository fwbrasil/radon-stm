package net.fwbrasil.radon.ref

import java.util.concurrent.locks._
import net.fwbrasil.radon.transaction.TransactionContext
import net.fwbrasil.radon.transaction.Transaction
import net.fwbrasil.radon.util.Lockable
import net.fwbrasil.radon.transaction.NestedTransaction
import scala.collection.mutable.WeakHashMap

trait Source[+T] {
	def unary_! = get.getOrElse(null.asInstanceOf[T])
	def get: Option[T]
}

trait Sink[-T] {
	def :=(value: T) = put(Option(value))
	def put(value: Option[T]): Unit
}

trait RefListener[T] {
	def notifyGet(ref: Ref[T]) = {

	}
	def notifyPut(ref: Ref[T], obj: Option[T]) = {

	}
	def notifyRollback(ref: Ref[T]) = {

	}
	def notifyCommit(ref: Ref[T]) = {

	}
}

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
			_weakListenersMap = new WeakHashMap[RefListener[T], Int]
		else
			_weakListenersMap
		_weakListenersMap
	}

	@transient
	private[radon] val creationTransaction = getRequiredTransaction

	def getRequiredTransaction =
		getRequiredActiveTransaction

	def getTransaction =
		getActiveTransaction

	if (initialize)
		put(pValueOption, getRequiredTransaction)

	private[fwbrasil] def refContent =
		_refContent

	private[fwbrasil] def setRefContent(pValue: Option[T]): Unit = {
		val content = this.refContent
		setRefContent(pValue, content.readTimestamp, content.writeTimestamp, content.destroyedFlag)
	}

	private[fwbrasil] def destroyInternal =
		setRefContent(None, readTimestamp, writeTimestamp, true)

	private[fwbrasil] def setRefContent(pValue: Option[T], pReadTimestamp: Long, pWriteTimestamp: Long, pDestroyedFlag: Boolean): Unit = {
		if (_weakListenersMap != null) {
			for (listener <- _weakListenersMap.keys)
				listener.notifyCommit(this)
		}
		_refContent = new RefContent[T](pValue, pReadTimestamp, pWriteTimestamp, pDestroyedFlag)
	}

	private[radon] def readTimestamp = refContent.readTimestamp
	private[radon] def writeTimestamp = refContent.writeTimestamp
	private[radon] def destroyedFlag = refContent.destroyedFlag
	private[radon] def isCreating =
		writeTimestamp == 0 &&
			creationTransaction != null &&
			!creationTransaction.transient

	def get: Option[T] = {
		val result = getRequiredTransaction.get(this)
		if (_weakListenersMap != null)
			for (listener <- _weakListenersMap.keys)
				listener.notifyGet(this)
		result
	}

	def put(pValue: Option[T], pTransaction: Transaction): Unit = {
		val value = if (pValue == null) None else pValue
		if (_weakListenersMap == null)
			pTransaction.put(this, value)
		else {
			import context._
			transactional(nested) {
				getRequiredTransaction.put(this, value)
				for (listener <- _weakListenersMap.keys)
					listener.notifyPut(this, value)
			}
		}
	}

	def put(pValue: Option[T]): Unit =
		put(pValue, getRequiredTransaction)

	private[radon] def notifyRollback =
		if (_weakListenersMap != null)
			for (listener <- _weakListenersMap.keys)
				listener.notifyRollback(this)

	def destroy: Unit =
		getRequiredTransaction.destroy(this)

	def isDestroyed: Boolean =
		getRequiredTransaction.isDestroyed(this)

	def isDirty: Boolean =
		getRequiredTransaction.isDirty(this)

	def addWeakListener(listener: RefListener[T]) =
		weakListenersMap += (listener -> listener.hashCode())

	protected def snapshot =
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
		"Ref(" + snapshot + ")"
}

object Ref {
	def apply[T](value: T)(implicit context: TransactionContext) = new Ref(value)
	def apply[T](implicit context: TransactionContext) = new Ref
}

case class RefContent[T](
	value: Option[T],
	readTimestamp: Long,
	writeTimestamp: Long,
	destroyedFlag: Boolean)

trait RefContext {

	type Ref[T] = net.fwbrasil.radon.ref.Ref[T]

	implicit def refToValue[A](ref: Ref[A]): A =
		if (ref == null)
			null.asInstanceOf[A]
		else !ref

	implicit def valueToRef[A](value: A)(implicit context: TransactionContext): Ref[A] =
		new Ref(value)
}