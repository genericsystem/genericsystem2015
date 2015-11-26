package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;

import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.Pane;

import org.genericsystem.todoApp.binding.Binding;
import org.genericsystem.todoApp.binding.BindingContext;

public class ViewContext {
	private final Element template;
	private final Node node;
	private final IModelContext modelContext;
	private final ViewContext parent;
	private boolean initContent = true;
	private final List<ViewContext> children = new ArrayList<>();

	public ViewContext(IModelContext modelContext, Element template, Node node, ViewContext parent) {
		this.template = template;
		this.node = node;
		this.modelContext = modelContext;
		this.parent = parent;
	}

	public ViewContext init() {
		BindingContext bindingContext = new BindingContext(modelContext, this);
		for (Binding binding : template.binding)
			binding.init(bindingContext);
		if (initContent)
			initChildren();
		return this;
	}

	public void destroy() {
		if (node.getParent() instanceof Pane)
			((Pane) node.getParent()).getChildren().remove(node);
	}

	public void addChildren(ViewContext viewContext) {
		if (node instanceof Pane)
			((Pane) node).getChildren().add(viewContext.node);
		children.add(viewContext);
	}

	public void bind(IModelContext modelContext) {
		ViewContext wrapper = new ViewContext(modelContext, template, node, this);
		wrapper.initChildren();
	}

	private void initChildren() {
		template.getChildren().forEach(element -> {
			Node childNode = null;
			childNode = createNode(element.classNode);
			if (childNode instanceof Button)
				((Button) childNode).setText(element.text.get());
			ViewContext viewContextChild = new ViewContext(modelContext, (element), childNode, this);
			addChildren(viewContextChild);
			modelContext.registre(viewContextChild);
			viewContextChild.init();
		});
	}

	private Node createNode(Class<? extends Node> clazz) {
		try {
			return clazz.newInstance();
		} catch (InstantiationException | IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	public Element getTemplate() {
		return template;
	}

	public Node getNode() {
		return node;
	}

	public IModelContext getModelContext() {
		return modelContext;
	}

	public ViewContext getParent() {
		return parent;
	}

	public boolean isInitContent() {
		return initContent;
	}

	public void setInitContent(boolean initContent) {
		this.initContent = initContent;
	}

	public List<ViewContext> getChildren() {
		return children;
	}
}
