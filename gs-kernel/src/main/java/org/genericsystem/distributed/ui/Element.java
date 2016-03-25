package org.genericsystem.distributed.ui;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Element<N> {
	private final Class<N> nodeClass;
	private List<Boot<N>> boots = new ArrayList<>();
	public final List<MetaBinding<N, ?>> metaBindings = new ArrayList<>();
	public final List<Binding<N, ?, ?>> bindings = new ArrayList<>();
	private final Element<?> parent;
	private final List<Element<?>> children = new ArrayList<>();

	@Override
	public String toString() {
		return "Element<" + nodeClass.getSimpleName() + ">";
	}

	protected <PARENTNODE, W> Element(Element<PARENTNODE> parent, Class<N> nodeClass) {
		this.nodeClass = nodeClass;
		this.parent = parent;
		if (parent != null)
			parent.<N> getChildren().add(this);
		initChildren();
	}

	protected void initChildren() {

	}

	@SuppressWarnings("rawtypes")
	protected abstract Function<N, List> getGraphicChildren();

	protected <VALUE> Element<N> addBoot(Function<N, Property<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.setProperty(applyOnNode, value));
		return this;
	}

	protected <VALUE> Element<N> addObservableListBoot(Function<N, ObservableList<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.addProperty(applyOnNode, value));
		return this;
	}

	protected List<Boot<N>> getBootList() {
		return boots;
	}

	protected <M, W> Element<N> addBidirectionalBinding(Function<N, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		bindings.add(Binding.bindBiDirectionalProperty(applyOnModel, applyOnNode));
		return this;
	}

	protected <M, T> Element<N> addBinding(Function<N, Property<T>> applyOnNode, Function<M, ObservableValue<T>> applyOnModel) {
		bindings.add(Binding.bindProperty(applyOnModel, applyOnNode));
		return this;
	}

	protected <M, T> Element<N> setObservableListBinding(Function<N, Property<ObservableList<T>>> applyOnNode, Function<M, ObservableList<T>> applyOnModel) {
		bindings.add(Binding.bindObservableList(applyOnModel, applyOnNode));
		return this;
	}

	protected <M, T> Element<N> addActionBinding(Function<N, Property<T>> applyOnNode, Consumer<M> applyOnModel) {
		bindings.add(Binding.bindAction(applyOnModel, applyOnNode));
		return this;
	}

	protected <M, T> Element<N> addReversedBinding(Function<N, ObservableValue<T>> applyOnNode, Function<M, Property<T>> applyOnModel) {
		bindings.add(Binding.bindReversedProperty(applyOnModel, applyOnNode));
		return this;
	}

	protected <M> Element<N> addObservableListToObservableValueBinding(Function<N, ObservableList<String>> applyOnNode, Function<M, ObservableValue<String>> applyOnModel) {
		bindings.add(Binding.bindObservableListToObservableValue(applyOnModel, applyOnNode));
		return this;
	}

	protected <M, T> Element<N> addObservableListBinding(Function<N, ObservableList<T>> applyOnNode, Function<M, ObservableValue<Boolean>> applyOnModel, T styleClass) {
		bindings.add(Binding.bindObservableList(applyOnModel, styleClass, applyOnNode));
		return this;
	}

	protected <M extends Model, T extends Model> Element<N> forEach(Function<M, ObservableList<T>> applyOnModel) {
		metaBindings.add(MetaBinding.forEach(applyOnModel));
		return this;
	}

	protected <M extends Model, T extends Model> Element<N> select(Function<M, ObservableValue<T>> applyOnModel) {
		metaBindings.add(MetaBinding.selector(applyOnModel));
		return this;
	}

	protected N createNode(Object parent) {
		try {
			return nodeClass.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	<CHILDNODE> List<Element<CHILDNODE>> getChildren() {
		return (List) children;
	}

	Element<?> getParent() {
		return parent;
	}

}
