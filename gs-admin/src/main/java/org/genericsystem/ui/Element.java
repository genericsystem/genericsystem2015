package org.genericsystem.ui;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import org.genericsystem.ui.utils.Utils;

public class Element<N> {
	public final Class<N> nodeClass;
	public final List<MetaBinding<N, ?>> metaBindings = new ArrayList<>();
	public final List<Binding<N, ?, ?>> bindings = new ArrayList<>();
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
		initChildren();
	}

	protected void initChildren() {

	}

	public <VALUE> Element<N> addBoot(Function<N, Property<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.setProperty(applyOnNode, value));
		return this;
	}

	public <VALUE> Element<N> addObservableListBoot(Function<N, ObservableList<VALUE>> applyOnNode, VALUE value) {
		this.boots.add(Boot.addProperty(applyOnNode, value));
		return this;
	}

	public List<Boot<N>> getBootList() {
		return boots;
	}

	public <M, W> Element<N> addBidirectionalBinding(Function<N, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		bindings.add(Binding.bindBiDirectionalProperty(applyOnModel, applyOnNode));
		return this;
	}

	public <M, T> Element<N> addBinding(Function<N, Property<T>> applyOnNode, Function<M, ObservableValue<T>> applyOnModel) {
		bindings.add(Binding.bindProperty(applyOnModel, applyOnNode));
		return this;
	}

	public <M, T> Element<N> addSuperBinding(Function<N, Property<T>> applyOnNode, Function<M, ObservableValue<T>> applyOnModel) {
		bindings.add(Binding.bindMetaProperty(applyOnModel, applyOnNode));
		return this;
	}

	public <M, T> Element<N> setObservableList(Function<N, Property<ObservableList<T>>> applyOnNode, Function<M, ObservableList<T>> applyOnModel) {
		bindings.add(Binding.bindObservableList(applyOnModel, applyOnNode));
		return this;
	}

	public <M, T> Element<N> addActionBinding(Function<N, Property<T>> applyOnNode, Consumer<M> applyOnModel) {
		bindings.add(Binding.bindAction(applyOnModel, applyOnNode));
		return this;
	}

	public <SUPERMODEL, M, T> Element<N> addBindMetaAction(Function<N, Property<T>> propAction, BiConsumer<SUPERMODEL, M> biConsumer) {
		bindings.add(Binding.bindMetaAction(biConsumer, propAction));
		return this;
	}

	public <M, T> Element<N> addReversedBinding(Function<N, ObservableValue<T>> applyOnNode, Function<M, Property<T>> applyOnModel) {
		bindings.add(Binding.bindReversedProperty(applyOnModel, applyOnNode));
		return this;
	}

	public <M> Element<N> addObservableListBinding(Function<N, ObservableList<String>> applyOnNode, Function<M, ObservableValue<String>> applyOnModel) {
		bindings.add(Binding.bindObservableListToObservableValue(applyOnModel, applyOnNode));
		return this;
	}

	public <M, T> Element<N> addObservableListBinding(Function<N, ObservableList<T>> applyOnNode, Function<M, ObservableValue<Boolean>> applyOnModel, T styleClass) {
		bindings.add(Binding.bindObservableList(applyOnModel, styleClass, applyOnNode));
		return this;
	}

	protected <M, T> Element<N> forEach(Function<M, ObservableList<T>> applyOnModel) {
		metaBindings.add(MetaBinding.forEach(applyOnModel));
		return this;
	}

	protected <M, T> Element<N> forEach(Function<M, ObservableList<T>> applyOnModel, Function<T, Property<M>> injectedProperty) {
		forEach(applyOnModel);
		bindings.add(Binding.bind(null, injectedProperty, Binder.injectBinder()));
		return this;
	}

	public <M, T> Element<N> select(Function<M, ObservableValue<T>> applyOnModel) {
		metaBindings.add(MetaBinding.selector(applyOnModel));
		return this;
	}

	public <M, T> Element<N> select(Function<M, ObservableValue<T>> applyOnModel, Function<T, Property<M>> injectedProperty) {
		select(applyOnModel);
		bindings.add(Binding.bind(null, injectedProperty, Binder.injectBinder()));
		return this;
	}

	// for scrollpane, cache is mandatory!
	private final Map<Object, ObservableList<N>> uiChildreCachen = new HashMap<Object, ObservableList<N>>() {
		private static final long serialVersionUID = 1L;

		@Override
		public ObservableList<N> get(Object uiParent) {
			ObservableList<N> result = super.get(uiParent);
			if (result == null) {
				put(uiParent, result = (ObservableList<N>) ((Function) getGraphicChildren).apply(uiParent));
				assert result != null;
			}
			return result;
		};
	};

	public <PARENTNODE> ObservableList<N> uiChildren(PARENTNODE uiParent) {
		return uiChildreCachen.get(uiParent);
	}

	@Deprecated
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
		// System.out.println(" INCREMENT : this : " + this + " uichildren : " + System.identityHashCode(uiChildren) + " child : " + child);
		Map<Element, Integer> internal = map.get(uiChildren);
		internal.put(child, internal.get(child) + 1);
	}

	void decrementSize(List uiChildren, Element child) {
		// System.out.println(" DECREMENT : this : " + this + " uichildren : " + System.identityHashCode(uiChildren) + " child : " + child);
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
