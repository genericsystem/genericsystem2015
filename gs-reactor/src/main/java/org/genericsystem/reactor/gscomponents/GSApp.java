package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import io.vertx.core.http.ServerWebSocket;

import java.lang.annotation.Annotation;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.annotations.CustomAnnotations;

public class GSApp extends RootTagImpl implements SelectionDefaults {

	public GSApp() {
		createSelectionProperty();
	}

	@Override
	public void beforeProcessAnnotations() {
		super.beforeProcessAnnotations();
		Annotation annotations = getClass().getAnnotation(CustomAnnotations.class);
		if (annotations != null)
			for (Class<? extends Annotation> annotation : ((CustomAnnotations) annotations).value())
				getAnnotationsManager().registerAnnotation(annotation);
	}

	@Override
	public RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
	}

}
