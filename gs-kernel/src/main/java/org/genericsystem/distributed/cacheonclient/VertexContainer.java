package org.genericsystem.distributed.cacheonclient;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import org.genericsystem.common.Vertex;

import com.sun.javafx.collections.ObservableListWrapper;

public class VertexContainer extends ObservableListWrapper<Vertex> implements Consumer<Vertex[]> {

	private CompletableFuture<Vertex[]> promise = new CompletableFuture();

	public VertexContainer() {
		super(new ArrayList<>());
	}

	@Override
	public void accept(Vertex[] requestedValues) {
		addAll(Arrays.asList(requestedValues));
		promise.complete(requestedValues);
	}

	public Vertex[] blockAndGet() {
		try {
			System.out.println("and now i give");
			return promise.get();
		} catch (InterruptedException | ExecutionException e) {
			throw new RuntimeException(e);
		}
	}

}
