package org.genericsystem.distributed;

import io.vertx.core.Vertx;

public class GSVertx {

	private static GSVertx instance = new GSVertx();
	private Vertx vertx;

	private GSVertx() {
		vertx = Vertx.vertx();
	};

	public static GSVertx vertx() {
		return instance;
	}

	public Vertx getVertx() {
		return vertx;
	}

	public void close() {
		vertx.close();
		vertx = null;

	}

}
