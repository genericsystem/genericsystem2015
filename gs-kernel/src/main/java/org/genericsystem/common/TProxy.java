package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;
import javassist.util.proxy.ProxyObject;
import org.genericsystem.defaults.DefaultVertex;

public interface TProxy<T extends DefaultVertex<T>> extends DefaultVertex<T> {
	@Override
	default AbstractRoot<T> getRoot() {
		return getProxyHandler().getRoot();
	}

	@Override
	default AbstractContext<T> getCurrentCache() {
		return (AbstractContext<T>) DefaultVertex.super.getCurrentCache();
	}

	@SuppressWarnings("unchecked")
	default AbstractRoot<T>.Wrapped getProxyHandler() {
		return ((AbstractRoot<T>.Wrapped) ((ProxyObject) this).getHandler());
	}

	@Override
	default long getTs() {
		return getProxyHandler().getTs();
	}

	@Override
	default T getMeta() {
		return getProxyHandler().getMeta();
	}

	@Override
	default List<T> getSupers() {
		return getProxyHandler().getSupers();
	}

	@Override
	default Serializable getValue() {
		return getProxyHandler().getValue();
	}

	@Override
	default List<T> getComponents() {
		return getProxyHandler().getComponents();
	}

	@Override
	default long[] getOtherTs() {
		return getProxyHandler().getOtherTs();
	}

	default Vertex getVertex() {
		return getProxyHandler().getVertex();
	}
}
