package org.genericsystem.reactor;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.gscomponents.TagImpl;

public interface RootTag extends Tag {

	default RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
	}

	@Override
	default RootTag getRootTag() {
		return this;
	}

	AnnotationsManager getAnnotationsManager();

	default void processAnnotations(Tag tag) {
		getAnnotationsManager().processAnnotations(tag);
	}

	TagNode buildTagNode(Tag child);

	default void processChildren(Tag tag, Class<? extends TagImpl>[] classes) {
		for (Class<? extends TagImpl> clazz : classes) {
			TagImpl result = createChild(tag, clazz);
			result.setTagNode(buildTagNode(result));
			processAnnotations(result);
			result.init();
		}
	}

	default <T extends TagImpl> TagImpl createChild(Tag tag, Class<T> clazz) {
		T result = null;
		try {
			result = clazz.newInstance();
		} catch (IllegalAccessException | InstantiationException e) {
			throw new IllegalStateException(e);
		}
		result.setParent(tag);
		return result;
	}

	default void processStyle(Tag tag, String name, String value) {
		tag.addStyle(name, value);
	}

}