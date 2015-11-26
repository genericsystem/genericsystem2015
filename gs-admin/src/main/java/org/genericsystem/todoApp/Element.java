package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.Pane;
import org.genericsystem.todoApp.IModelContext.ModelContext;
import org.genericsystem.todoApp.binding.Binding;

public class Element {
	public Class<? extends Node> classNode;
	public StringProperty text = new SimpleStringProperty();
	public Binding[] binding;
	private List<Element> children = new ArrayList<>();

	public Element(Element parent, Class<? extends Node> classNode, String text, Binding... binding) {
		this.classNode = classNode;
		this.binding = binding;
		this.text.set(text);
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

	public void removeChildNode(Node parentNode, Node node) {
		if (parentNode instanceof Pane)
			((Pane) parentNode).getChildren().remove(node);
	}

	public Node createChildNode(Node parentNode) {
		Node childNode = createNode(classNode);
		if (parentNode instanceof Pane)
			((Pane) parentNode).getChildren().add(childNode);
		return childNode;

	}

	private Node createNode(Class<? extends Node> clazz) {
		Node childNode;
		try {
			childNode = clazz.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
		if (childNode instanceof Button)
			((Button) childNode).setText(text.get());
		return childNode;
	}
}
