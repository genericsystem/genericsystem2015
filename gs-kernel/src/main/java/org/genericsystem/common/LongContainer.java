package org.genericsystem.common;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import javafx.beans.property.SimpleLongProperty;

public class LongContainer extends SimpleLongProperty implements Consumer<Long> {

	private CompletableFuture<Long> promise = new CompletableFuture<>();

	@Override
	public void accept(Long t) {
		promise.complete(t);
		super.set(t);
	}

	// à supprimer à terme
	public long blockAndGet() {
		try {
			long result = promise.get();
			System.out.println("and now i give " + result);
			return result;
		} catch (InterruptedException | ExecutionException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public String toString() {
		return "LongContainer [" + super.toString() + "]";
	}

}
