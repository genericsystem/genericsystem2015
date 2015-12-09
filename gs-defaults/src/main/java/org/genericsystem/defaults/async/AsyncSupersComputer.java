package org.genericsystem.defaults.async;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.genericsystem.defaults.DefaultVertex;

public class AsyncSupersComputer<T extends DefaultVertex<T>> extends CompletableFuture<LinkedHashSet<T>> {

	public AsyncSupersComputer(T meta, List<T> overrides, Serializable value, List<T> components) {
		throw new UnsupportedOperationException("AsyncSupersComputer");
	}
}
