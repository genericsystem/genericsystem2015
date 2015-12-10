package org.genericsystem.ui;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.scene.layout.Pane;

public class Element<N> {
	public final Class<N> nodeClass;
	public final List<Binding<N, ?>> metaBindings = new ArrayList<>();
	public final List<Binding<N, ?>> bindings = new ArrayList<>();
	private final Element<?> parent;
	private final List<Element<?>> children = new ArrayList<>();
	private final Function<?, ObservableList<?>> getGraphicChildren;

	private List<Boot<N>> boots = new ArrayList<>();

	@Override
	public String toString() {
		return "Element<" + nodeClass.getSimpleName() + ">";
	}

	public <PARENTNODE extends Pane> Element(Element<PARENTNODE> parent, Class<N> nodeClass) {
		this(parent, nodeClass, Pane::getChildren);
	}

	public <PARENTNODE, W> Element(Element<PARENTNODE> parent, Class<N> nodeClass, Function<? super PARENTNODE, ObservableList<W>> getGraphicChildren) {
		this.nodeClass = nodeClass;
		this.parent = parent;
		this.getGraphicChildren = (Function) getGraphicChildren;
		if (parent != null)
			parent.<N> getChildren().add(this);
	}

	@Deprecated
	@SafeVarargs
	public <PARENTNODE extends Pane> Element(Element<PARENTNODE> parent, Class<N> nodeClass, Binding<N, ?>... binding) {
		this(parent, nodeClass, Pane::getChildren, binding);
	}

	@SafeVarargs
	@Deprecated
	public <PARENTNODE extends Pane> Element(Element<PARENTNODE> parent, Class<N> nodeClass, List<Binding<N, ?>> metaBindings, Binding<N, ?>... binding) {
		this(parent, nodeClass, Pane::getChildren, metaBindings, binding);
	}

	@SafeVarargs
	@Deprecated
	public <PARENTNODE> Element(Element<PARENTNODE> parent, Class<N> nodeClass, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren, Binding<N, ?>... binding) {
		this(parent, nodeClass, getGraphicChildren, Collections.emptyList(), binding);
	}

	@SafeVarargs
	@Deprecated
	public <PARENTNODE> Element(Element<PARENTNODE> parent, Class<N> nodeClass, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren, List<Binding<N, ?>> metaBindings, Binding<N, ?>... binding) {
		this.nodeClass = nodeClass;
		this.parent = parent;
		this.metaBindings.addAll(metaBindings);
		this.bindings.addAll(Arrays.asList(binding));
		this.getGraphicChildren = getGraphicChildren;
		if (parent != null)
			parent.<N> getChildren().add(this);
	}

	@SafeVarargs
	@Deprecated
	public final void addBoots(Boot<N>... boot) {
		this.boots.addAll(Arrays.asList(boot));
	}

	public <VALUE> Element<N> addBoot(Function<N, Property<VALUE>> getProperty, VALUE value) {
		this.boots.add(Boot.setProperty(getProperty, value));
		return this;
	}

	public <VALUE> Element<N> addObservableListBoot(Function<N, ObservableList<VALUE>> getProperty, VALUE value) {
		this.boots.add(Boot.addProperty(getProperty, value));
		return this;
	}

	public List<Boot<N>> getBootList() {
		return boots;
	}

	@Deprecated
	public void addMetaBindings(Binding<N, ?> metaBinding) {
		metaBindings.add(metaBinding);
	}

	@SafeVarargs
	@Deprecated
	public final void addBindings(Binding<N, ?>... binding) {
		bindings.addAll(Arrays.asList(binding));
	}

	public <M, W> Element<N> addBidirectionalBinding(Function<N, Property<W>> getProperty, Function<M, Property<W>> function) {
		bindings.add(Binding.bindBiDirectionalProperty(getProperty, function));
		return this;
	}

	public <M, T> Element<N> addBinding(Function<N, Property<T>> getProperty, Function<M, ObservableValue<T>> function) {
		bindings.add(Binding.bindProperty(getProperty, function));
		return this;
	}

	public <M, T extends Event> Element<N> addActionBinding(Function<N, ObjectProperty<EventHandler<T>>> propAction, Consumer<M> consumer) {
		bindings.add(Binding.bindAction(propAction, consumer));
		return this;
	}

	public <M, T extends Event> Element<N> addGenericActionBinding(Function<N, ObjectProperty<T>> propAction, Consumer<M> consumer) {
		bindings.add(Binding.bindGenericAction(propAction, consumer));
		return this;
	}

	public <M, T> Element<N> addReversedBinding(Function<N, Property<T>> getProperty, Function<M, Property<T>> function) {
		bindings.add(Binding.bindReversedProperty(getProperty, function));
		return this;
	}

	public <M, T> Element<N> addObservableListBinding(Function<N, ObservableList<T>> getObservable, Function<M, ObservableValue<Boolean>> function, T styleClass) {
		bindings.add(Binding.bindObservableList(getObservable, function, styleClass));
		return this;
	}

	public <M, T> Element<N> addForEachMetaBinding(Function<M, ObservableList<T>> function) {
		metaBindings.add(Binding.forEach(function));
		return this;
	}

	public <M, T> Element<N> addSelectorMetaBinding(Function<M, ObservableValue<T>> function) {
		metaBindings.add(Binding.selector(function));
		return this;
	}

	@SuppressWarnings("unchecked")
	public <PARENTNODE> ObservableList<N> getGraphicChildren(PARENTNODE graphicParent) {
		return ((Function<PARENTNODE, ObservableList<N>>) (Function<?, ?>) getGraphicChildren).apply(graphicParent);
	}

	public N apply(Object model) {
		return new ViewContext<>(null, new ModelContext(null, this, model), this, null).getNode();
	}

	N createNode(Object parent) {
		try {
			if (parent != null && !Modifier.isStatic(nodeClass.getModifiers()) && nodeClass.getEnclosingClass() != null)
				return nodeClass.getDeclaredConstructor(new Class[] { parent.getClass() }).newInstance(new Object[] { parent });
			else
				return nodeClass.getDeclaredConstructor(new Class[] {}).newInstance(new Object[] {});
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public <CHILDNODE> List<Element<CHILDNODE>> getChildren() {
		return (List) children;
	}

	public Element<?> getParent() {
		return parent;
	}

	private Map<List, Map<Element, Integer>> map = new IdentityHashMap<List, Map<Element, Integer>>() {
		@Override
		public Map<Element, Integer> get(Object key) {
			Map<Element, Integer> internal = super.get(key);
			if (internal == null)
				put((List) key, internal = new IdentityHashMap<Element, Integer>() {
					@Override
					public Integer get(Object key) {
						Integer size = super.get(key);
						if (size == null)
							put((Element) key, size = 0);
						return size;
					};
				});
			return internal;
		};
	};

	void incrementSize(List graphicChildren, Element child) {
		Map<Element, Integer> internal = map.get(graphicChildren);
		internal.put(child, internal.get(child) + 1);
	}

	void decrementSize(List graphicChildren, Element child) {
		Map<Element, Integer> internal = map.get(graphicChildren);
		int size = internal.get(child) - 1;
		assert size >= 0;
		if (size == 0)
			internal.remove(child);// remove map if 0 for avoid heap pollution
		else
			internal.put(child, size);
	}

	int computeIndex(List graphicChildren, Element childElement) {
		int indexInChildren = 0;
		for (Element child : getChildren()) {
			indexInChildren += map.get(graphicChildren).get(child);
			if (child == childElement)
				break;
		}
		return indexInChildren;
	}
}
