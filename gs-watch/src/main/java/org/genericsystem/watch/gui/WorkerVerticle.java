package org.genericsystem.watch.gui;

import org.genericsystem.common.Root;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;

/**
 * This class extends {@link AbstractVerticle} in order to provide a worker verticle. One must override the default {@link #start()} or {@link #start(io.vertx.core.Future)} methods.
 * 
 * @author Pierrik Lassalas
 *
 */
public class WorkerVerticle extends AbstractVerticle {

	private Root root;

	public WorkerVerticle() {
		this(null);
	}

	public WorkerVerticle(Root root) {
		this.root = root;
	}

	@Override
	public void start(Future<Void> startFuture) throws Exception {
		System.out.println("Worker thread: " + Thread.currentThread().getName());
		start();
		startFuture.complete();
	}

	@Override
	public void start() throws Exception {
		throw new IllegalStateException("You need to implement the start() method of the worker verticle.");
	}
}
