package org.genericsystem.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import javafx.collections.ObservableList;
import javafx.scene.layout.Pane;

public class Element<N> {
	public final Class<N> classNode;
	public List<? extends Binding<?, ?>> metaBindings = new ArrayList<>();
	public List<Binding<?, ?>> bindings = new ArrayList<>();

	private final List<Element<?>> children = new ArrayList<>();
	private final Function<?, ObservableList<?>> getGraphicChildren;

	private List<Boot> boots = new ArrayList<>();

	public <PARENTNODE extends Pane> Element(Element<PARENTNODE> parent, Class<N> classNode, Binding<?, ?>... binding) {
		this(parent, classNode, Pane::getChildren, binding);
	}

	public <PARENTNODE> Element(Element<PARENTNODE> parent, Class<N> classNode, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren, Binding<?, ?>... binding) {
		this(parent, classNode, getGraphicChildren, Collections.emptyList(), binding);
	}

	public <PARENTNODE> Element(Element<PARENTNODE> parent, Class<N> classNode, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren, List<? extends Binding<?, ?>> metaBindings, Binding<?, ?>... binding) {
		this.classNode = classNode;
		this.metaBindings = metaBindings;
		this.bindings.addAll(Arrays.asList(binding));
		this.getGraphicChildren = getGraphicChildren;
		if (parent != null)
			parent.<N> getChildren().add(this);
	}

	public void addBoots(Boot... boot) {
		this.boots.addAll(Arrays.asList(boot));
	}

	public List<Boot> getBootList() {
		return boots;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void addMetaBinding(Binding<?, ?> metaBinding) {
		((List<Binding>) metaBindings).add(metaBinding);
	}

	public void addBinding(Binding<?, ?>... binding) {
		bindings.addAll(Arrays.asList(binding));
	}

	@SuppressWarnings("unchecked")
	public <PARENTNODE> ObservableList<N> getGraphicChildren(PARENTNODE graphicParent) {
		return ((Function<PARENTNODE, ObservableList<N>>) (Function<?, ?>) getGraphicChildren).apply(graphicParent);
	}

	public N apply(Object model) {
		N node = createNode();
		new ViewContext<>(new ModelContext<Object>(null, model), this, node, null);
		return node;
	}

	N createNode() {
		try {
			return classNode.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public <CHILDNODE> List<Element<CHILDNODE>> getChildren() {
		return (List) children;
	}

}
