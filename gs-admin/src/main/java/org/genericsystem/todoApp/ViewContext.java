package org.genericsystem.todoApp;

import java.util.Collections;
import java.util.List;
import javafx.beans.property.Property;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.Pane;
import org.genericsystem.todoApp.binding.Binding;
import org.genericsystem.todoApp.binding.BindingContext;

public class ViewContext {
	private final Element template;
	private final Node node;
	private final ModelContext modelContext;
	private final ViewContext parent;
	private boolean initChildren = true;

	public ViewContext(ModelContext modelContext, Element template, Node node, ViewContext parent) {
		this.template = template;
		this.node = node;
		this.modelContext = modelContext;
		this.parent = parent;
	}

	public ViewContext init() {
		BindingContext bindingContext = new BindingContext(modelContext, this);
		for (Binding<?> binding : template.bindings)
			binding.init(bindingContext);
		if (initChildren)
			initChildren();
		return this;
	}

	public List<Node> getChildrenNodes(Node parentNode) {
		if (parentNode instanceof Pane)
			return ((Pane) parentNode).getChildren();
		return Collections.emptyList();
	}

	public void removeChildNode(Node parentNode, Node node) {
		getChildrenNodes(parentNode).remove(node);
	}

	public void destroyChild(ViewContext childContext) {
		getChildrenNodes(node).remove(childContext.getNode());
	}

	public void bind(ModelContext modelContext) {
		new ViewContext(modelContext, template, node, this).initChildren();
	}

	private void initChildren() {
		for (Element childElement : template.getChildren()) {
			Node childNode = childElement.createChildNode(node);
			ViewContext childViewContext = new ViewContext(modelContext, childElement, childNode, this);
			modelContext.register(childViewContext);
			childViewContext.init();
		}
	}

	public Property<String> getTextProperty() {
		if (node instanceof Label)
			return ((Label) node).textProperty();
		if (node instanceof TextField)
			return ((TextField) node).textProperty();
		throw new IllegalStateException();
	}

	public void setOnAction(EventHandler<ActionEvent> handler) {
		if (node instanceof Button)
			((Button) getNode()).setOnAction(handler);
		else
			throw new IllegalStateException();
	}

	public void disableInitChildren() {
		this.initChildren = false;
	}

	public Element getTemplate() {
		return template;
	}

	public Node getNode() {
		return node;
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	public ViewContext getParent() {
		return parent;
	}
}
