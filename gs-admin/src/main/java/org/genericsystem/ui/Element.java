package org.genericsystem.ui;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.scene.Group;

import org.genericsystem.ui.utils.Utils;

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

	public <PARENTNODE extends Group> Element(Class<N> nodeClass) {
		this(null, nodeClass, Group::getChildren);
	}

	// must be protected
	protected <PARENTNODE> Element(Element<PARENTNODE> parent, Class<N> nodeClass) {
		this(parent, nodeClass, Utils.getClassChildren(parent));
	}

	// must be protected
	protected <PARENTNODE, W> Element(Element<PARENTNODE> parent, Class<N> nodeClass, Function<PARENTNODE, ObservableList<?>> getGraphicChildren) {
		this.nodeClass = nodeClass;
		this.parent = parent;
		this.getGraphicChildren = getGraphicChildren;
		if (parent != null)
			parent.<N> getChildren().add(this);
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

	public <M, W> Element<N> addBidirectionalBinding(Function<N, Property<W>> getProperty, Function<M, Property<W>> function) {
		bindings.add(Binding.bindBiDirectionalProperty(getProperty, function));
		return this;
	}

	public <M, T> Element<N> addBinding(Function<N, Property<T>> getProperty, Function<M, ObservableValue<T>> function) {
		bindings.add(Binding.bindProperty(getProperty, function));
		return this;
	}

	public <M, T> Element<N> addSuperBinding(Function<N, Property<T>> getProperty, Function<M, ObservableValue<T>> function) {
		bindings.add(Binding.bindSuperProperty(getProperty, function));
		return this;
	}

	public <M, T> Element<N> setObservableList(Function<N, Property<ObservableList<T>>> getProperty, Function<M, ObservableList<T>> function) {
		bindings.add(Binding.bindObservableList(getProperty, function));
		return this;
	}

	public <M, T extends Event> Element<N> addActionBinding(Function<N, ObjectProperty<EventHandler<T>>> propAction, Consumer<M> consumer) {
		bindings.add(Binding.bindAction(propAction, consumer));
		return this;
	}

	public <M, T> Element<N> addGenericActionBinding(Function<N, ObjectProperty<T>> propAction, Consumer<M> consumer) {
		bindings.add(Binding.bindGenericAction(propAction, consumer));
		return this;
	}
	
	public <SUPERMODEL, M, T> Element<N> addGenericMouseActionBinding(Function<N, ObjectProperty<T>> propAction, BiConsumer<SUPERMODEL,M> biConsumer) {
		bindings.add(Binding.bindGenericMouseAction(propAction, biConsumer));
		return  this;
	}

	public <M, T> Element<N> addReversedBinding(Function<N, Property<T>> getProperty, Function<M, Property<T>> function) {
		bindings.add(Binding.bindReversedProperty(getProperty, function));
		return this;
	}

	public <M> Element<N> addObservableListBinding(Function<N, ObservableList<String>> getObservable, Function<M, ObservableValue<String>> function) {
		bindings.add(Binding.bindObservableListToObservableValue(getObservable, function));
		return this;
	}

	public <M, T> Element<N> addObservableListBinding(Function<N, ObservableList<T>> getObservable, Function<M, ObservableValue<Boolean>> function, T styleClass) {
		bindings.add(Binding.bindObservableList(getObservable, function, styleClass));
		return this;
	}

	protected <M, T> Element<N> forEach(Function<M, ObservableList<T>> function) {
		metaBindings.add(Binding.forEach(function));
		return this;
	}

	protected <M, T> Element<N> forEach(Function<M, ObservableList<T>> function, Function<T, Property<M>> injectedProperty) {
		forEach(function);
		bindings.add(Binding.bind(Binder.injectBinder(), injectedProperty));
		return this;
	}

	public <M, T> Element<N> select(Function<M, ObservableValue<T>> function) {
		metaBindings.add(Binding.selector(function));
		return this;
	}

	public <M, T> Element<N> select(Function<M, ObservableValue<T>> function, Function<T, Property<M>> injectedProperty) {
		select(function);
		bindings.add(Binding.bind(Binder.injectBinder(), injectedProperty));
		return this;
	}

	@SuppressWarnings("unchecked")
	public <PARENTNODE> ObservableList<N> uiChildren(PARENTNODE uiParent) {
		return ((Function<PARENTNODE, ObservableList<N>>) (Function<?, ?>) getGraphicChildren).apply(uiParent);
	}

	public N apply(Object model, Object parentNode) {
		return new ViewContext<>(null, new ModelContext(null, this, model), this, (N) parentNode).getNode();
	}

	protected N createNode(Object parent) {
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

	void incrementSize(List uiChildren, Element child) {
		Map<Element, Integer> internal = map.get(uiChildren);
		internal.put(child, internal.get(child) + 1);
	}

	void decrementSize(List uiChildren, Element child) {
		Map<Element, Integer> internal = map.get(uiChildren);
		int size = internal.get(child) - 1;
		assert size >= 0;
		if (size == 0)
			internal.remove(child);// remove map if 0 for avoid heap pollution
		else
			internal.put(child, size);
	}

	int computeIndex(List uiChildren, Element childElement) {
		int indexInChildren = 0;
		for (Element child : getChildren()) {
			indexInChildren += map.get(uiChildren).get(child);
			if (child == childElement)
				break;
		}
		return indexInChildren;
	}

}
