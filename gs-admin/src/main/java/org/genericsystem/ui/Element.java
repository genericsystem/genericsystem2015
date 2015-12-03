package org.genericsystem.ui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

import javafx.collections.ObservableList;
import javafx.scene.layout.Pane;

public class Element {
	public final Class<?> classNode;
	public final List<? extends Binding<?>> metaBindings;
	public final Binding<?>[] bindings;
	private final List<Element> children = new ArrayList<>();
	private final Function<Object, ObservableList<Object>> getGraphicChildren;

	public <V extends Pane> Element(Element parent, Class<?> classNode, Binding<?>... binding) {
		this(parent, classNode, Pane::getChildren, binding);
	}

	public <V> Element(Element parent, Class<?> classNode, Function<V, ObservableList<?>> getGraphicChildren, Binding<?>... binding) {
		this(parent, classNode, getGraphicChildren, Collections.emptyList(), binding);
	}

	public <V> Element(Element parent, Class<?> classNode, Function<V, ObservableList<?>> getGraphicChildren, List<? extends Binding<?>> metaBindings, Binding<?>... binding) {
		this.classNode = classNode;
		this.metaBindings = metaBindings;
		this.bindings = binding;
		this.getGraphicChildren = (Function) getGraphicChildren;
		if (parent != null)
			parent.getChildren().add(this);
	}

	public ObservableList<Object> getGraphicChildren(Object graphicParent) {
		return getGraphicChildren.apply(graphicParent);
	}

	public ViewContext apply(Object model) {
		Pane node = (Pane) createNode();
		return new ViewContext(new ModelContext(null, model), this, node, null);
	}

	public Object createNode() {
		try {
			return classNode.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	public List<Element> getChildren() {
		return children;
	}

}
