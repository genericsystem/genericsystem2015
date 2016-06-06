package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

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
	}

	@SuppressWarnings("rawtypes")
	protected abstract Function<N, List> getGraphicChildren();

	protected <VALUE> void addBoot(Function<N, Property<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.setProperty(applyOnNode, value));
	}

	protected <VALUE> void addSetBoot(Function<N, Set<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.addProperty(applyOnNode, value));
	}

	protected List<Boot<N>> getBootList() {
		return boots;
	}

	protected <W> void addBidirectionalBinding(Function<N, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		bindings.add(Binding.bindBiDirectionalProperty(applyOnModel, applyOnNode));

	}

	protected <T> void addBinding(Function<N, Property<T>> applyOnNode, Function<M, ObservableValue<T>> applyOnModel) {
		bindings.add(Binding.bindProperty(applyOnModel, applyOnNode));

	}

	protected <T> void setObservableListBinding(Function<N, Property<ObservableList<T>>> applyOnNode, Function<M, ObservableList<T>> applyOnModel) {
		bindings.add(Binding.bindObservableList(applyOnModel, applyOnNode));

	}

	protected <T> void addActionBinding(Function<N, Property<T>> applyOnNode, Consumer<M> applyOnModel) {
		bindings.add(Binding.bindAction(applyOnModel, applyOnNode));

	}

	protected <T> void addReversedBinding(Function<N, ObservableValue<T>> applyOnNode, Function<M, Property<T>> applyOnModel) {
		bindings.add(Binding.bindReversedProperty(applyOnModel, applyOnNode));

	}

	// protected void addObservableSetToObservableValueBinding(Function<N, ObservableSet<String>> applyOnNode, Function<M, ObservableValue<String>>
	// applyOnModel) {
	// bindings.add(Binding.bindObservableSetToObservableValue(applyOnModel, applyOnNode));
	// }

	protected <T> void addSetBinding(Function<N, Set<T>> applyOnNode, Function<M, ObservableValue<Boolean>> applyOnModel, T styleClass) {
		bindings.add(Binding.bindSet(applyOnModel, styleClass, applyOnNode));

	}

	// protected <T> void addObservableMapBinding(Function<N, ObservableMap<String, String>> applyOnNode, Function<M, ObservableValue<Number>> applyOnModel,
	// String attr, String[] value) {
	// bindings.add(Binding.bindObservableMap(applyOnModel, attr, value, applyOnNode));
	//
	// }
	//
	// protected <T> void addObservableMapBinding(Function<N, ObservableMap<String, String>> applyOnNode,
	// Function<M, ObservableMap<String, String>> applyOnModel) {
	// bindings.add(Binding.bindObservableMap(applyOnModel, applyOnNode));
	//
	// }

	public <T extends Model> void forEach(Function<T, ObservableList<M>> applyOnModel) {
		metaBindings.add(MetaBinding.forEach(applyOnModel));

	}

	public <T extends CompositeModel> void forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor,
			ModelConstructor<CompositeModel> constructor) {
		metaBindings.add(MetaBinding.forEach(stringExtractor, observableListExtractor, constructor));
	}

	public <T extends Model> void select(Function<T, ObservableValue<M>> applyOnModel) {
		metaBindings.add(MetaBinding.selector(applyOnModel));

	}

	public <T extends CompositeModel> void select(StringExtractor stringExtractor, Supplier<Generic> genericSupplier,
			ModelConstructor<CompositeModel> constructor) {
		metaBindings.add(MetaBinding.selector(stringExtractor, genericSupplier, constructor));

	}

	public <T extends CompositeModel> void select(StringExtractor stringExtractor, Class<?> genericClass, ModelConstructor<CompositeModel> constructor) {
		metaBindings.add(MetaBinding.selector(this, stringExtractor, genericClass, constructor));

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

	@SuppressWarnings("unchecked")
	public <COMPONENT extends Element<?, ?>> COMPONENT getParent() {
		return (COMPONENT) parent;
	}
}
