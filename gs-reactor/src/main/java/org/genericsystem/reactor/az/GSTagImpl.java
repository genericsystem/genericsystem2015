package org.genericsystem.reactor.az;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.MetaBinding;
import org.genericsystem.reactor.Tag;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public abstract class GSTagImpl implements Tag {

	private MetaBinding<?> metaBinding;
	private final List<Consumer<Context>> preFixedBindings = new ArrayList<>();
	private final List<Consumer<Context>> postFixedBindings = new ArrayList<>();
	private Tag parent;
	private final ObservableList<Tag> children = FXCollections.observableArrayList();

	protected GSTagImpl(Tag parent) {
		setParent(parent);
	}

	public void setParent(Tag parent) {
		this.parent = parent;
		if (parent != null)
			parent.getObservableChildren().add(this);
	}

	protected GSTagImpl() {
	}

	@Override
	public String toString() {
		return getTag() + " " + getClass().getName();
	}

	@Override
	public List<Consumer<Context>> getPreFixedBindings() {
		return preFixedBindings;
	}

	@Override
	public List<Consumer<Context>> getPostFixedBindings() {
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