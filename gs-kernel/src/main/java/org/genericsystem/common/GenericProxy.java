package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;

import javassist.util.proxy.ProxyObject;

import org.genericsystem.defaults.DefaultVertex;

public interface GenericProxy extends DefaultVertex<Generic> {
	@Override
	default AbstractRoot getRoot() {
		return getProxyHandler().getRoot();
	}

	// @Override
	// default DefaultCache<Generic> getCurrentCache() {
	// return DefaultVertex.super.getCurrentCache();
	// }

	default AbstractRoot.DefaultHandler getProxyHandler() {
		return ((AbstractRoot.DefaultHandler) ((ProxyObject) this).getHandler());
	}

	@Override
	default long getTs() {
		return getProxyHandler().getTs();
	}

	@Override
	default Generic getMeta() {
		Generic result = getProxyHandler().getMeta();
		return result != null ? result : (Generic) this;
	}

	@Override
	default List<Generic> getSupers() {
		return getProxyHandler().getSupers();
	}

	@Override
	default Serializable getValue() {
		return getProxyHandler().getValue();
	}

	@Override
	default List<Generic> getComponents() {
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
