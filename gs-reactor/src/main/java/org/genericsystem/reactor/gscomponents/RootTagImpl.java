package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.UserRoleDefaults;

import java.lang.annotation.Annotation;

import org.genericsystem.reactor.AnnotationsManager;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.RootTag;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.annotations.CustomAnnotations;

import io.vertx.core.http.ServerWebSocket;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class RootTagImpl extends FlexDiv implements RootTag, SelectionDefaults, UserRoleDefaults {

	private AnnotationsManager annotationsManager;

	public RootTagImpl() {
		tagNode = buildTagNode(null);
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

	@Override
	public TagNode buildTagNode(Tag parent) {
		return new TagNode() {

			private final ObservableList<Tag> children = FXCollections.observableArrayList();

			@SuppressWarnings("unchecked")
			@Override
			public <COMPONENT extends Tag> COMPONENT getParent() {
				return (COMPONENT) parent;
			}

			@Override
			public ObservableList<Tag> getObservableChildren() {
				return children;
			}
		};

	};

	@Override
	public final <COMPONENT extends Tag> COMPONENT getParent() {
		return null;
	}

	@Override
	public RootTag getRootTag() {
		return this;
	}
}
