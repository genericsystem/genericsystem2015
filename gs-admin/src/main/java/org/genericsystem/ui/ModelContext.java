package org.genericsystem.ui;

import java.util.ArrayList;
import java.util.List;

public class ModelContext {

	private final ModelContext parent;
	private final Object model;
	private final List<ModelContext> children = new ArrayList<>();
	private final List<ViewContext> viewContexts = new ArrayList<>();

	void createSubContext(ViewContext viewContext, int index, Object model, Element childElement) {
		ModelContext childContext = new ModelContext(this, model);
		new ViewContext(childContext, childElement, childElement.classNode.isAssignableFrom(model.getClass()) ? model : childElement.createNode(), viewContext);
		children.add(index, childContext);
	}

	public ModelContext removeSubContext(int index) {
		ModelContext removed = children.remove(index);
		for (ViewContext viewContext : removed.viewContexts)
			viewContext.destroyChild();
		return removed;
	};

	public ModelContext(ModelContext parent, Object model) {
		this.parent = parent;
		this.model = model;
	}

	public Object getModel() {
		return this.model;
	}

	public ModelContext getParent() {
		return this.parent;
	}

	public List<ModelContext> getChildren() {
		return this.children;
	}

	public void register(ViewContext viewContext) {
		this.viewContexts.add(viewContext);
	}

	ModelContext get(int index) {
		return children.get(index);
	}

	int size() {
		return children.size();
	}

}
