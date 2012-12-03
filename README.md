[![Build Status](https://secure.travis-ci.org/fwbrasil/radon-stm.png)](http://travis-ci.org/fwbrasil/radon-stm)

Introduction
============

RadonSTM is a timestamp based Software Transaction Memory ([STM](http://en.wikipedia.org/wiki/Software_transactional_memory "STM")) implementation in Scala.
The main framework objective is providing a simple and flexible way to deal with concurrency. STM is a wide known solution implemented in many languages.

This project was initially  based on [Daniel Spiewak STM](http://www.codecommit.com/blog/scala/improving-the-stm-multi-version-concurrency-control "Daniel Spiewalk STM") implementation and grew up focused on providing the basis for Activate Persistence Framework.

Radon has features like propagation control, nested transactions and ref destroying.

Dependency
==========

SBT

	val radonStm = "net.fwbrasil" %% "radon-stm" % "0.0.1"
	val fwbrasil = "fwbrasil.net" at "http://fwbrasil.net/maven/"

Maven

	<dependency>
    	<groupId>net.fwbrasil</groupId>
	    <artifactId>radon-stm</artifactId>
    	<version>0.0.1</version>
	</dependency>
	
	<repository>
		<id>fwbrasil</id>
		<url>http://fwbrasil.net/maven/</url>
    </repository>

Getting Started
===============

Put the radon jar in the classpath and create a Radon context:


	object TestRadonContext extends RadonContext


Import the context object for the classes where you want to use Radon:


	import TestRadonContext._


Concurrency control is achieved by using Ref objects in transactional blocks like:

	transactional {
	    val ref = new Ref(100)
	    ref.set(200)
	    ref :== 300
	    println(ref.get)
	    println(!ref)
	}

To set a ref value use set or :==, and to get a ref value use get or the unary ! method.

********************************************************
*Important: Make sure to use immutable values inside Ref.*
********************************************************

You can use ref objects on many threads without caring about concurrency. RadonSTM detects conflicts and retries the transaction if it is needed. Transaction is hold on a thread local while it is active, so you can have code under a transaction without declaring the "transactional" keyword.

Typically transactional blocks are controlled by the framework. However, it's possible to control a transaction as follows:

	val transaction = new Transaction
	transactional(transaction) {
	    val ref = new Ref(100)
	}
	transaction.commit

You can define a transaction propagation:

	transactional {
	    val ref = new Ref(100)
	    transactional(mandatory) {
	        ref := 200
	    }
	    println(!ref)
	}

Nested transactions are a type of propagation:

	transactional {
	    val ref = new Ref(100)
	    transactional(nested) {
	        ref := 200
	    }
	    println(!ref)
	}

The available propagations are based on EJB propagations:
 * required
 * requiresNew
 * mandatory
 * notSupported
 * supports
 * never
 * nested

It is possible to destroy a Ref and, after that, it can't be read or written:


	transactional {
	    val ref = new Ref(100)
	    ref.destroy
	}

Take a look in  [Sleeping Barber Spec](https://github.com/fwbrasil/radon-stm/blob/master/src/test/scala/net/fwbrasil/radon/problems/SleepingBarber.scala "Sleeping Barber Spec").

License
=======

All code in this repository is licensed under the LGPL. See the LICENSE-LGPL file for more details.