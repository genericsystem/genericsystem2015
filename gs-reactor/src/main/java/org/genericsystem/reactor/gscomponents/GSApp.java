package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.UserRoleDefaults;

import java.lang.annotation.Annotation;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.annotations.CustomAnnotations;

import io.vertx.core.http.ServerWebSocket;

public class GSApp extends RootTagImpl implements SelectionDefaults, UserRoleDefaults {

	public GSApp() {
		createSelectionProperty();
		createLoggedUserProperty();
		createAdminModeProperty();
	}

	@Override
	public void beforeProcessAnnotations() {
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
