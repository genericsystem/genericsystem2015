package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;
import org.genericsystem.reactor.modelproperties.UserRoleDefaults;

import java.lang.annotation.Annotation;

import org.genericsystem.reactor.AnnotationsManager;
import org.genericsystem.reactor.RootTag;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.annotations.CustomAnnotations;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

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
		initRoot();
	}

	protected void initRoot() {
		setTagNode(buildTagNode(this));
		processAnnotations(this);
		init();
	}

	@Override
	public AnnotationsManager getAnnotationsManager() {
		return annotationsManager;
	}

	public static class SimpleTableNode implements TagNode {
		private final ObservableList<Tag> children = FXCollections.observableArrayList();

		@Override
		public ObservableList<Tag> getObservableChildren() {
			return children;
		}
	}

	@Override
	public TagNode buildTagNode(Tag child) {
		Tag parent = child.getParent();
		if (parent != null)
			parent.getObservableChildren().add(child);
		return new SimpleTableNode();
	}

	@Override
	public final <COMPONENT extends Tag> COMPONENT getParent() {
		return null;
	}

	@Override
	public RootTag getRootTag() {
		return this;
	}
}
