package org.genericsystem.common;

import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.util.List;

import org.genericsystem.defaults.DefaultGeneric;

import javassist.util.proxy.ProxyObject;

public interface GenericProxy extends DefaultGeneric<Generic> {
	@Override
	default Root getRoot() {
		return getProxyHandler().getRoot();
	}

	// @Override
	// default DefaultCache<Generic> getCurrentCache() {
	// return DefaultVertex.super.getCurrentCache();
	// }

	default Root.DefaultHandler getProxyHandler() {
		return ((Root.DefaultHandler) ((ProxyObject) this).getHandler());
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

	default <A extends Annotation> A getAnnotation(Class<A> annotation) {
		return getRoot().getAnnotedClass((Generic) this).getAnnotation(annotation);
	}

}
