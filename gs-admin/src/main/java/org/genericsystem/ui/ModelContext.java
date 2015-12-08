package org.genericsystem.ui;

import java.util.ArrayList;
import java.util.List;

public class ModelContext {

	private final ModelContext parent;
	private final Object model;
	private List<ModelContext> children = new ArrayList<>();
	private final List<ViewContext<?>> viewContexts = new ArrayList<>();

	public ModelContext(ModelContext parent, Object model) {
		this.parent = parent;
		this.model = model;
	}

	@SuppressWarnings("unchecked")
	public <M> M getModel() {
		return (M) model;
	}

	public ModelContext getParent() {
		return this.parent;
	}

	public List<ModelContext> getChildren() {
		return this.children;
	}

	public void register(ViewContext<?> viewContext) {
		this.viewContexts.add(viewContext);
	}

	public List<ViewContext<?>> getViewContexts() {
		return viewContexts;
	}
}
