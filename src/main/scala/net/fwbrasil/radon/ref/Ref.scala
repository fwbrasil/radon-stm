package net.fwbrasil.radon.ref

import java.util.concurrent.locks._
import net.fwbrasil.radon.transaction.TransactionContext
import net.fwbrasil.radon.transaction.Transaction
import net.fwbrasil.radon.util.Lockable

trait Source[+T] {
	def unary_! = get.getOrElse(null.asInstanceOf[T])
	def get: Option[T]
}

trait Sink[-T] {
	def :=(value: T) = put(Option(value))
	def put(value: Option[T]): Unit
}

class Ref[T](pValueOption: Option[T])(implicit val context: TransactionContext) 
    extends Source[T] with Sink[T] with Lockable with java.io.Serializable {
	
	import context.transactionManager._
	
	def this(pValue: T)(implicit context: TransactionContext) = this(Option(pValue))
	def this()(implicit context: TransactionContext) = this(None)
	
	private[this] var _refContent: RefContent[T] = RefContent(None, 0l, 0l, false)

	@transient
	private[radon] val creationTransaction = getRequiredActiveTransaction
	
	put(pValueOption)
	
	private[radon] def refContent = 
		_refContent

	private[fwbrasil] def setRefContent(value: Option[T]): Unit =
		setRefContent(value, readTimestamp, writeTimestamp, destroyedFlag)
		
	private[fwbrasil] def setRefContent(pValue: Option[T], pReadTimestamp: Long, pWriteTimestamp: Long, pDestroyedFlag: Boolean): Unit =
		_refContent = RefContent[T](pValue, pReadTimestamp, pWriteTimestamp, pDestroyedFlag)
	
	private[radon] def readTimestamp = refContent.readTimestamp 
	private[radon] def writeTimestamp = refContent.writeTimestamp 
	private[radon] def destroyedFlag = refContent.destroyedFlag
	private[radon] def isCreating = 
		writeTimestamp == 0 && 
		creationTransaction !=null && 
		!creationTransaction.transient
	
	def get: Option[T] = 
		getRequiredActiveTransaction.get(this)

	def put(value: Option[T]): Unit =
		getRequiredActiveTransaction.put(this, Option(value).getOrElse(None))
	
	def destroy: Unit =
	    getRequiredActiveTransaction.destroy(this)
	   
	def isDestroyed: Boolean =
		getRequiredActiveTransaction.isDestroyed(this)
	
	override def toString = "Ref("+refContent.toString+")"

}

object Ref {
	def apply[T](value: T)(implicit context: TransactionContext)  = new Ref(value)
	def apply[T](implicit context: TransactionContext)  = new Ref
}

case class RefContent[T](value: Option[T], readTimestamp: Long, writeTimestamp: Long, destroyedFlag: Boolean)

trait RefContext {
	
	type Ref[T] = net.fwbrasil.radon.ref.Ref[T]
	
	implicit def refToValue[A](ref: Ref[A]): A = 
		if(ref==null)
			null.asInstanceOf[A]
		else !ref
	
	implicit def valueToRef[A](value: A)(implicit context: TransactionContext): Ref[A] =
		new Ref(value)
}