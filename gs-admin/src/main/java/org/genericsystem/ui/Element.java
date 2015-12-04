package org.genericsystem.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import javafx.collections.ObservableList;
import javafx.scene.layout.Pane;

public class Element<NODE> {
	public final Class<NODE> classNode;
	public List<? extends Binding<?, ?, ?>> metaBindings = new ArrayList<>();
	public List<Binding<?, ?, ?>> bindings = new ArrayList<>();

	private final List<Element<?>> children = new ArrayList<>();
	private final Function<?, ObservableList<?>> getGraphicChildren;

	private List<Boot> boots = new ArrayList<>();

	public Element(Element<?> parent, Class<NODE> classNode, Binding<?, ?, ?>... binding) {
		this(parent, classNode, Pane::getChildren, binding);
	}

	public <PARENTNODE> Element(Element<?> parent, Class<NODE> classNode, Function<PARENTNODE, ObservableList<?>> getGraphicChildren, Binding<?, ?, ?>... binding) {
		this(parent, classNode, getGraphicChildren, Collections.emptyList(), binding);
	}

	public <PARENTNODE> Element(Element<?> parent, Class<NODE> classNode, Function<PARENTNODE, ObservableList<?>> getGraphicChildren, List<? extends Binding<?, ?, ?>> metaBindings, Binding<?, ?, ?>... binding) {
		this.classNode = classNode;
		this.metaBindings = metaBindings;
		this.bindings.addAll(Arrays.asList(binding));
		this.getGraphicChildren = getGraphicChildren;
		if (parent != null)
			parent.<NODE> getChildren().add(this);
	}

	public void addBoots(Boot... boot) {
		this.boots.addAll(Arrays.asList(boot));
	}

	public List<Boot> getBootList() {
		return boots;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void addMetaBinding(Binding<?, ?, ?> metaBinding) {
		((List<Binding>) metaBindings).add(metaBinding);
	}

	public void addBinding(Binding<?, ?, ?>... binding) {
		bindings.addAll(Arrays.asList(binding));
	}

	@SuppressWarnings("unchecked")
	public <PARENTNODE> ObservableList<NODE> getGraphicChildren(PARENTNODE graphicParent) {
		return ((Function<PARENTNODE, ObservableList<NODE>>) (Function) getGraphicChildren).apply(graphicParent);
	}

	public NODE apply(Object model) {
		NODE node = createNode();
		new ViewContext<>(new ModelContext(null, model), this, node, null);
		return node;
	}

	NODE createNode() {
		try {
			return classNode.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	public <CHILDNODE> List<Element<CHILDNODE>> getChildren() {
		return (List) children;
	}

}
