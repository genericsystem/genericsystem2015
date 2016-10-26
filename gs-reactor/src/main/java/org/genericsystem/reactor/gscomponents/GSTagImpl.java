package org.genericsystem.reactor.gscomponents;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import org.genericsystem.defaults.tools.ObservableListWrapperExtended;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.MetaBinding;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.ModeSelector;

import javafx.beans.Observable;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;

public abstract class GSTagImpl implements Tag {

	private MetaBinding<?> metaBinding;
	private final List<Consumer<Context>> preFixedBindings = new ArrayList<>();
	private final List<Consumer<Context>> postFixedBindings = new ArrayList<>();
	private Tag parent;
	private final ObservableList<Tag> children = FXCollections.observableArrayList();
	protected ModeSelector modeSelector;

	@FunctionalInterface
	public static interface TriFunction<T, S, R> {
		public R apply(T t, S s);
	}

	protected GSTagImpl(Tag parent) {
		setParent(parent);
		beforeProcessAnnotations();
		getRootTag().getAnnotationsManager().processAnnotations(this);
		init();
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
	public ObservableList<Tag> getObservableChildren(Context context) {
		return new FilteredList<Tag>(new ObservableListWrapperExtended<Tag>(children, child -> child.getModeSelector() != null ? new Observable[] { child.getModeSelector().apply(context, child) } : new Observable[] { new SimpleBooleanProperty(true) }),
				child -> child.getModeSelector() != null ? Boolean.TRUE.equals(child.getModeSelector().apply(context, child).getValue()) : true);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <COMPONENT extends Tag> COMPONENT getParent() {
		return (COMPONENT) parent;
	}

	@Override
	public void setModeSelector(ModeSelector modeSelector) {
		this.modeSelector = modeSelector;
	}

	@Override
	public ModeSelector getModeSelector() {
		return modeSelector;
	}
}