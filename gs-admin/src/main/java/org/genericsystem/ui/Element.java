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
	public List<Binding<N, ?, ?>> metaBindings = new ArrayList<>();
	public List<Binding<N, ?, ?>> bindings = new ArrayList<>();

	private final List<Element<?>> children = new ArrayList<>();
	private final Function<?, ObservableList<?>> getGraphicChildren;

	private List<Boot<N>> boots = new ArrayList<>();

	@SafeVarargs
	public <PARENTNODE extends Pane> Element(Element<PARENTNODE> parent, Class<N> classNode, Binding<N, ?, ?>... binding) {
		this(parent, classNode, Pane::getChildren, binding);
	}

	@SafeVarargs
	public <PARENTNODE> Element(Element<PARENTNODE> parent, Class<N> classNode, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren, Binding<N, ?, ?>... binding) {
		this(parent, classNode, getGraphicChildren, Collections.emptyList(), binding);
	}

	@SafeVarargs
	public <PARENTNODE> Element(Element<PARENTNODE> parent, Class<N> classNode, Function<? super PARENTNODE, ObservableList<?>> getGraphicChildren, List<Binding<N, ?, ?>> metaBindings, Binding<N, ?, ?>... binding) {
		this.classNode = classNode;
		this.metaBindings = metaBindings;
		this.bindings.addAll(Arrays.asList(binding));
		this.getGraphicChildren = getGraphicChildren;
		if (parent != null)
			parent.<N> getChildren().add(this);
	}

	@SafeVarargs
	public final void addBoots(Boot<N>... boot) {
		this.boots.addAll(Arrays.asList(boot));
	}

	public List<Boot<N>> getBootList() {
		return boots;
	}

	public void addMetaBinding(Binding<N, ?, ?> metaBinding) {
		metaBindings.add(metaBinding);
	}

	@SafeVarargs
	public final void addBinding(Binding<N, ?, ?>... binding) {
		bindings.addAll(Arrays.asList(binding));
	}

	@SuppressWarnings("unchecked")
	public <PARENTNODE> ObservableList<N> getGraphicChildren(PARENTNODE graphicParent) {
		return ((Function<PARENTNODE, ObservableList<N>>) (Function<?, ?>) getGraphicChildren).apply(graphicParent);
	}

	public N apply(Object model) {
		N node = createNode();
		new ViewContext<>(new ModelContext(null, model), this, node, null);
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
