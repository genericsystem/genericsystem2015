package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.UserRoleDefaults;

import java.lang.annotation.Annotation;

import org.genericsystem.reactor.AnnotationsManager;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.Tag.RootTag;
import org.genericsystem.reactor.annotations.CustomAnnotations;

import io.vertx.core.http.ServerWebSocket;

public class RootTagImpl extends FlexDiv implements RootTag, SelectionDefaults, UserRoleDefaults {

	private AnnotationsManager annotationsManager;

	public RootTagImpl() {
		createSelectionProperty();
		createLoggedUserProperty();
		createAdminModeProperty();
		annotationsManager = new AnnotationsManager();
		Annotation annotations = getClass().getAnnotation(CustomAnnotations.class);
		if (annotations != null)
			for (Class<? extends Annotation> annotation : ((CustomAnnotations) annotations).value())
				annotationsManager.registerAnnotation(annotation);
		annotationsManager.processAnnotations(this);
		init();
	}

	@Override
	public AnnotationsManager getAnnotationsManager() {
		return annotationsManager;
	}

	@Override
	public RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
	}
}
