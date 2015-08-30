package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;

import javassist.util.proxy.ProxyObject;

import org.genericsystem.defaults.DefaultVertex;

public interface GenericProxy<T extends DefaultVertex<T>> extends DefaultVertex<T> {
	@Override
	default AbstractRoot<T> getRoot() {
		return getProxyHandler().getRoot();
	}

	@Override
	default AbstractContext<T> getCurrentCache() {
		return (AbstractContext<T>) DefaultVertex.super.getCurrentCache();
	}

	@SuppressWarnings("unchecked")
	default AbstractRoot<T>.DefaultHandler getProxyHandler() {
		return ((AbstractRoot<T>.DefaultHandler) ((ProxyObject) this).getHandler());
	}

	@Override
	default long getTs() {
		return getProxyHandler().getTs();
	}

	@SuppressWarnings("unchecked")
	@Override
	default T getMeta() {
		T result = getProxyHandler().getMeta();
		return result != null ? result : (T) this;
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
	default long getBirthTs() {
		return getProxyHandler().getBirthTs();
	}

	default Vertex getVertex() {
		return getProxyHandler().getVertex();
	}
}
