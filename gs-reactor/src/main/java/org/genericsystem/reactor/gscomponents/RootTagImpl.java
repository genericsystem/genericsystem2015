package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.AnnotationsManager;
import org.genericsystem.reactor.RootTag;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.contextproperties.UserRoleDefaults;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

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
		processAnnotations(this);
		init();
	}

	@Override
	public AnnotationsManager getAnnotationsManager() {
		return annotationsManager;
	}

	public static class SimpleTagNode implements TagNode {
		private final ObservableList<Tag> children = FXCollections.observableArrayList();

		@Override
		public ObservableList<Tag> getObservableChildren() {
			return children;
		}
	}

	@Override
	public TagNode buildTagNode(Tag child) {
		return new SimpleTagNode();
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
