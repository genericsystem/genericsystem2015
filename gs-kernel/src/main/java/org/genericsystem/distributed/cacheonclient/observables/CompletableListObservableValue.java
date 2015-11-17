package org.genericsystem.distributed.cacheonclient.observables;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Vertex;

public class CompletableListObservableValue<E> extends SimpleObjectProperty<List<E>> implements ObservableValue<List<E>> {

	private final Function<Vertex, E> extractor;

	public CompletableListObservableValue(Function<Vertex, E> extractor) {
		super(new ArrayList<>());
		this.extractor = extractor;
	}

	void launch(CompletableFuture<Vertex[]> promise) {
		promise.thenAccept(elements -> {
			setValue(Arrays.stream(elements).map(extractor::apply).collect(Collectors.toList()));
		});
	}
}