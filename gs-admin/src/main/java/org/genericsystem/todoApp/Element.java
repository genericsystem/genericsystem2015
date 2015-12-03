package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import javafx.collections.ObservableList;
import javafx.scene.layout.Pane;

import org.genericsystem.todoApp.binding.Binding;

public class Element {
	public Class<?> classNode;
	public Binding<?>[] bindings;
	private List<Element> children = new ArrayList<>();
	private Function<Object, ObservableList<Object>> getGraphicChildren;

	public <V extends Pane> Element(Element parent, Class<?> classNode, Binding<?>... binding) {
		this(parent, classNode, Pane::getChildren, binding);
	}

	public <V> Element(Element parent, Class<?> classNode, Function<V, ObservableList<?>> getGraphicChildren, Binding<?>... binding) {
		this.classNode = classNode;
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
		return new ViewContext(new ModelContext(null, model), this, node, null).init();
	}

	Object createNode() {
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
