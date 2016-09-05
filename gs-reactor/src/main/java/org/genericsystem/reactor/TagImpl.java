package org.genericsystem.reactor;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.gs.GSDiv;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class TagImpl implements Tag {

	private final String tag;
	private MetaBinding<?> metaBinding;
	private final List<BiConsumer<Context, HtmlDomNode>> preFixedBindings = new ArrayList<>();
	private final List<BiConsumer<Context, HtmlDomNode>> postFixedBindings = new ArrayList<>();
	private final Tag parent;
	private final ObservableList<Tag> children = FXCollections.observableArrayList();

	protected TagImpl(Tag parent, String tag) {
		this.tag = tag;
		this.parent = parent;
		if (parent != null)
			parent.getObservableChildren().add(this);
	}

	@Override
	public String toString() {
		return tag + " " + getClass().getName();
	}

	@Override
	public String getTag() {
		return tag;
	}

	@Override
	public List<BiConsumer<Context, HtmlDomNode>> getPreFixedBindings() {
		return preFixedBindings;
	}

	@Override
	public List<BiConsumer<Context, HtmlDomNode>> getPostFixedBindings() {
		return postFixedBindings;
	}

	@Override
	@SuppressWarnings("unchecked")
	public <BETWEEN> MetaBinding<BETWEEN> getMetaBinding() {
		return (MetaBinding<BETWEEN>) metaBinding;
	}

	@Override
	public <BETWEEN> void setMetaBinding(MetaBinding<BETWEEN> metaBinding) {
		if (this.metaBinding != null)
			throw new IllegalStateException("MetaBinding already defined");
		this.metaBinding = metaBinding;
	}

	@Override
	public ObservableList<Tag> getObservableChildren() {
		return children;
	}

	@Override
	@SuppressWarnings("unchecked")
	public <COMPONENT extends Tag> COMPONENT getParent() {
		return (COMPONENT) parent;
	}

	public static class TreeRootTagImpl extends GSDiv {

		private final HashMap<Class<? extends TagImpl>, TagImpl> nodes = new LinkedHashMap<Class<? extends TagImpl>, TagImpl>();

		public TreeRootTagImpl(Tag parent) {
			super(parent);
		}

		public TagImpl find(Class<? extends TagImpl> tagClass) {
			if (nodes.get(tagClass) == null) {
				Parent parent = tagClass.getAnnotation(Parent.class);
				try {
					if (parent != null)
						nodes.put(tagClass, tagClass.getConstructor(Tag.class).newInstance(find(parent.value()[0])));
					else
						nodes.put(tagClass, tagClass.getConstructor(Tag.class).newInstance(this));
				} catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | InstantiationException e) {
					throw new IllegalStateException(e);
				}
			}
			return nodes.get(tagClass);
		}
	}
}