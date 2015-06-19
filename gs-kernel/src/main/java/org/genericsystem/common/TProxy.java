package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;

import javassist.util.proxy.ProxyObject;

import org.genericsystem.defaults.DefaultVertex;

public interface TProxy<T extends DefaultVertex<T>> extends DefaultVertex<T> {
	@Override
	default AbstractRoot<T> getRoot() {
		return getRootWrapper().getRoot();
	}

	@Override
	default AbstractContext<T> getCurrentCache() {
		return (AbstractContext<T>) DefaultVertex.super.getCurrentCache();
	}

	@SuppressWarnings("unchecked")
	default AbstractRoot<T>.Wrapped getRootWrapper() {
		return ((AbstractRoot<T>.Wrapped) ((ProxyObject) this).getHandler());
	}

	@Override
	default long getTs() {
		return getRootWrapper().getTs();
	}

	@Override
	default T getMeta() {
		return getRootWrapper().getMeta();
	}

	@Override
	default List<T> getSupers() {
		return getRootWrapper().getSupers();
	}

	@Override
	default Serializable getValue() {
		return getRootWrapper().getValue();
	}

	@Override
	default List<T> getComponents() {
		return getRootWrapper().getComponents();
	}

	@Override
	default long[] getOtherTs() {
		return getRootWrapper().getOtherTs();
	}

	default Vertex getVertex() {
		return getRootWrapper().getVertex();
	}
}
