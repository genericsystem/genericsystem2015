package org.genericsystem.reactor.gscomponents;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;

import org.genericsystem.reactor.AnnotationsManager;
import org.genericsystem.reactor.RootTag;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.contextproperties.UserRoleDefaults;

public class RootTagImpl extends FlexDiv implements RootTag, SelectionDefaults, UserRoleDefaults {

	protected AnnotationsManager annotationsManager;

	public RootTagImpl() {
		createSelectionProperty();
		createLoggedUserProperty();
		createAdminModeProperty();
		initRoot();
	}

	protected void initRoot() {
		annotationsManager = new AnnotationsManager(getClass());
		setTagNode(buildTagNode(this));
		createSubTree();
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

		@Override
		public ObservableMap<String, String> getObservableStyles() {
			return FXCollections.observableHashMap();
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
