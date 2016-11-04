package org.genericsystem.reactor;

import java.util.List;
import java.util.function.Consumer;

import javafx.collections.ObservableList;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.TagNode.GenericTagType.GTagAttribute;
import org.genericsystem.reactor.model.TagSwitcher;

public interface TagNode extends Tag {

	// private MetaBinding<?> metaBinding;
	// private final List<Consumer<Context>> preFixedBindings = new ArrayList<>();
	// private final List<Consumer<Context>> postFixedBindings = new ArrayList<>();
	// private Tag parent;
	// private final ObservableList<Tag> children = FXCollections.observableArrayList();
	// protected TagSwitcher switcher;

	@SystemGeneric
	public static interface GenericTagType extends Generic {
		@SystemGeneric
		@Components(GenericTagType.class)
		public static interface GTagAttribute extends Generic {

		}
	}

	@SystemGeneric
	@Meta(GenericTagType.class)
	public static interface GenericTag extends Generic {

	}

	default Root getGenericRootTag() {
		return getGenericTag().getRoot();
	}

	GenericTag getGenericTag();

	default GenericTagType getGenericTagType() {
		return getGenericRootTag().find(GenericTagType.class);
	}

	@Override
	default String getTag() {
		return (String) getGenericTag().getValue(getGenericRootTag().find(GTagAttribute.class));
	}

	default void setTag(String tag) {
		getGenericTag().setHolder(getGenericRootTag().find(GTagAttribute.class), tag);
	}

	default void setParent(Tag parent) {
		// this.parent = parent;
		// if (parent != null)
		// parent.getObservableChildren().add(this);
	}

	@Override
	default List<Consumer<Context>> getPreFixedBindings() {
		return null;
	}

	@Override
	default List<Consumer<Context>> getPostFixedBindings() {
		return null;
	}

	@Override
	@SuppressWarnings("unchecked")
	default <BETWEEN> MetaBinding<BETWEEN> getMetaBinding() {
		return null;
	}

	@Override
	default <BETWEEN> void setMetaBinding(MetaBinding<BETWEEN> metaBinding) {
		// if (this.metaBinding != null)
		// throw new IllegalStateException("MetaBinding already defined");
		// this.metaBinding = metaBinding;
	}

	@Override
	default ObservableList<Tag> getObservableChildren() {
		return null;
	}

	@Override
	@SuppressWarnings("unchecked")
	default <COMPONENT extends Tag> COMPONENT getParent() {
		return null;
	}

	@Override
	default void addSwitcher(TagSwitcher switcher) {
		// this.switcher = switcher;
	}

	@Override
	default List<TagSwitcher> getSwitchers() {
		return null;
	}
}
