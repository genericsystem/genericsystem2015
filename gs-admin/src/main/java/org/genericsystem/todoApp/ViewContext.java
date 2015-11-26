package org.genericsystem.todoApp;

import javafx.scene.Node;
import org.genericsystem.todoApp.binding.Binding;
import org.genericsystem.todoApp.binding.BindingContext;

public class ViewContext {
	private final Element template;
	private final Node node;
	private final IModelContext modelContext;
	private final ViewContext parent;
	private boolean initContent = true;

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

	public void destroyChild(ViewContext childContext) {
		template.removeChildNode(node, childContext.getNode());
	}

	public void bind(IModelContext modelContext) {
		ViewContext wrapper = new ViewContext(modelContext, template, node, this);
		wrapper.initChildren();
	}

	private void initChildren() {
		for (Element childElement : template.getChildren()) {
			Node childNode = childElement.createChildNode(node);
			ViewContext childViewContext = new ViewContext(modelContext, childElement, childNode, this);
			modelContext.register(childViewContext);
			childViewContext.init();
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
}
