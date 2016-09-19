package org.genericsystem.reactor.gs;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.MetaBinding;
import org.genericsystem.reactor.Tag;

public class GSTagImpl implements Tag {

	private final String tag;
	private MetaBinding<?> metaBinding;
	private final List<BiConsumer<Context, HtmlDomNode>> preFixedBindings = new ArrayList<>();
	private final List<BiConsumer<Context, HtmlDomNode>> postFixedBindings = new ArrayList<>();
	private Tag parent;
	private final ObservableList<Tag> children = FXCollections.observableArrayList();

	protected GSTagImpl(Tag parent, String tag) {
		this.tag = tag;
		setParent(parent);
	}

	public void setParent(Tag parent) {
		this.parent = parent;
		if (parent != null)
			parent.getObservableChildren().add(this);
	}

	protected GSTagImpl(String tag) {
		this.tag = tag;
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
}