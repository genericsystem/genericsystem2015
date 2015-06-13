package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

import javassist.util.proxy.ProxyObject;

import org.genericsystem.common.AbstractRoot.AbstractRootWrapper;
import org.genericsystem.defaults.DefaultVertex;

public interface TProxy<T extends DefaultVertex<T>> extends DefaultVertex<T> {
	@Override
	AbstractRoot<T> getRoot();

	@Override
	default AbstractContext<T> getCurrentCache() {
		return (AbstractContext<T>) DefaultVertex.super.getCurrentCache();
	}

	@SuppressWarnings("rawtypes")
	default Vertex getVertex() {
		return ((AbstractRootWrapper) ((ProxyObject) this).getHandler()).getVertex();
	}

	@Override
	default long getTs() {
		return getVertex().getTs();
	}

	@Override
	default T getMeta() {
		return getRoot().getGenericByTs(getVertex().getMeta());
	}

	@Override
	default List<T> getSupers() {
		return getVertex().getSupers().stream().map(getRoot()::getGenericByTs).collect(Collectors.toList());
	}

	@Override
	default Serializable getValue() {
		return getVertex().getValue();
	}

	@Override
	default List<T> getComponents() {
		return getVertex().getComponents().stream().map(getRoot()::getGenericByTs).collect(Collectors.toList());
	}

}
