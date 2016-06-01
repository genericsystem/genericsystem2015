package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.CompositeModel.StringExtractor;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 */
public abstract class Element<M extends Model, N> {
	private final Class<N> nodeClass;
	private List<Boot<N>> boots = new ArrayList<>();
	public final List<MetaBinding<N, ?>> metaBindings = new ArrayList<>();
	public final List<Binding<N, ?, ?>> bindings = new ArrayList<>();
	private final Element<?, ?> parent;
	private final List<Element<?, ?>> children = new ArrayList<>();

	@Override
	public String toString() {
		return "Element<" + nodeClass.getSimpleName() + ">";
	}

	protected <PARENTNODE, W> Element(Element<?, PARENTNODE> parent, Class<N> nodeClass) {
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

	protected <VALUE> Element<M, N> addBoot(Function<N, Property<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.setProperty(applyOnNode, value));
		return this;
	}

	protected <VALUE> Element<M, N> addObservableSetBoot(Function<N, ObservableSet<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.addProperty(applyOnNode, value));
		return this;
	}

	protected <VALUE> Element<M, N> addObservableMapBoot(Function<N, ObservableMap<VALUE, VALUE>> applyOnNode, VALUE attr, VALUE value) {
		this.boots.add(Boot.addProperty(applyOnNode, attr, value));
		return this;
	}

	protected List<Boot<N>> getBootList() {
		return boots;
	}

	protected <W> Element<M, N> addBidirectionalBinding(Function<N, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		bindings.add(Binding.bindBiDirectionalProperty(applyOnModel, applyOnNode));
		return this;
	}

	protected <T> Element<M, N> addBinding(Function<N, Property<T>> applyOnNode, Function<M, ObservableValue<T>> applyOnModel) {
		bindings.add(Binding.bindProperty(applyOnModel, applyOnNode));
		return this;
	}

	protected <T> Element<M, N> setObservableListBinding(Function<N, Property<ObservableList<T>>> applyOnNode, Function<M, ObservableList<T>> applyOnModel) {
		bindings.add(Binding.bindObservableList(applyOnModel, applyOnNode));
		return this;
	}

	protected <T> Element<M, N> addActionBinding(Function<N, Property<T>> applyOnNode, Consumer<M> applyOnModel) {
		bindings.add(Binding.bindAction(applyOnModel, applyOnNode));
		return this;
	}

	protected <T> Element<M, N> addReversedBinding(Function<N, ObservableValue<T>> applyOnNode, Function<M, Property<T>> applyOnModel) {
		bindings.add(Binding.bindReversedProperty(applyOnModel, applyOnNode));
		return this;
	}

	protected Element<M, N> addObservableSetToObservableValueBinding(Function<N, ObservableSet<String>> applyOnNode,
			Function<M, ObservableValue<String>> applyOnModel) {
		bindings.add(Binding.bindObservableSetToObservableValue(applyOnModel, applyOnNode));
		return this;
	}

	protected <T> Element<M, N> addObservableSetBinding(Function<N, ObservableSet<T>> applyOnNode, Function<M, ObservableValue<Boolean>> applyOnModel,
			T styleClass) {
		bindings.add(Binding.bindObservableSet(applyOnModel, styleClass, applyOnNode));
		return this;
	}

	protected <T> Element<M, N> addObservableMapBinding(Function<N, ObservableMap<String, String>> applyOnNode,
			Function<M, ObservableValue<Number>> applyOnModel, String attr, String[] value) {
		bindings.add(Binding.bindObservableMap(applyOnModel, attr, value, applyOnNode));
		return this;
	}

	public <T extends Model> Element<M, N> forEach(Function<T, ObservableList<M>> applyOnModel) {
		metaBindings.add(MetaBinding.forEach(applyOnModel));
		return this;
	}

	public <T extends CompositeModel> Element<M, N> forEach(Function<T, ObservableList<CompositeModel>> applyOnModel, StringExtractor stringExtractor,
			ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		metaBindings.add(MetaBinding.forEach(applyOnModel, stringExtractor, observableListExtractor, constructor));
		return this;
	}

	public <T extends Model> Element<M, N> select(Function<T, ObservableValue<M>> applyOnModel) {
		metaBindings.add(MetaBinding.selector(applyOnModel));
		return this;
	}

	public <T extends CompositeModel> Element<M, N> select(Function<T, Property<CompositeModel>> applyOnModel, StringExtractor stringExtractor,
			Supplier<Generic> genericSupplier, ModelConstructor<CompositeModel> constructor) {
		metaBindings.add(MetaBinding.selector(applyOnModel, stringExtractor, genericSupplier, constructor));
		return this;
	}

	public <T extends CompositeModel> Element<M, N> select(Function<T, Property<CompositeModel>> applyOnModel, StringExtractor stringExtractor,
			Class<?> genericClass, ModelConstructor<CompositeModel> constructor) {
		metaBindings.add(MetaBinding.selector(applyOnModel, stringExtractor, genericClass, constructor));
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
	protected <CHILDNODE> List<Element<?, CHILDNODE>> getChildren() {
		return (List) children;
	}

	public <COMPONENT extends Element<?, ?>> COMPONENT getParent() {
		return (COMPONENT) parent;
	}
}
