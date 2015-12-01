package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.Node;
import javafx.scene.layout.Pane;

import org.genericsystem.todoApp.binding.Binding;

public class Element {
	public Class<? extends Node> classNode;
	public Binding<?>[] bindings;
	private List<Element> children = new ArrayList<>();

	public Element(Element parent, Class<? extends Node> classNode, Binding<?>... binding) {
		this.classNode = classNode;
		this.bindings = binding;
		if (parent != null)
			parent.getChildren().add(this);
	}

	public ViewContext apply(Object model) {
		return new ViewContext(new ModelContext(null, model), this, createNode(), null).init();
	}

	private Node createNode() {
		try {
			return classNode.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	public List<Element> getChildren() {
		return children;
	}

	public Node createChildNode(Node parentNode) {
		Node childNode = createNode(classNode);
		if (parentNode instanceof Pane)
			((Pane) parentNode).getChildren().add(childNode);
		return childNode;

	}

	private Node createNode(Class<? extends Node> clazz) {
		try {
			return clazz.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}
}
