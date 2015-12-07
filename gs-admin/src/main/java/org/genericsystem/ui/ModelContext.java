package org.genericsystem.ui;

import java.util.ArrayList;
import java.util.List;

public class ModelContext<M> {

	private final ModelContext<M> parent;
	private final M model;
	private final List<ModelContext<M>> children = new ArrayList<>();
	private final List<ViewContext<?>> viewContexts = new ArrayList<>();

	<SUBMODEL> void createSubContext(ViewContext<?> viewContext, int index, SUBMODEL model, Element<SUBMODEL> childElement) {
		ModelContext<M> childContext = new ModelContext<M>(this, (M) model);
		new ViewContext<>(childContext, childElement, childElement.classNode.isAssignableFrom(model.getClass()) ? model : childElement.createNode(), viewContext);
		children.add(index, childContext);
	}

	ModelContext<?> removeSubContext(int index) {
		ModelContext<?> removed = children.remove(index);
		for (ViewContext<?> viewContext : removed.viewContexts)
			viewContext.destroyChild();
		return removed;
	};

	ModelContext<M> get(int index) {
		return children.get(index);
	}

	int size() {
		return children.size();
	}

	public ModelContext(ModelContext<M> parent, M model) {
		this.parent = parent;
		this.model = model;
	}

	public M getModel() {
		return model;
	}

	public ModelContext<M> getParent() {
		return this.parent;
	}

	public List<ModelContext<M>> getChildren() {
		return this.children;
	}

	public void register(ViewContext<?> viewContext) {
		this.viewContexts.add(viewContext);
	}

}
