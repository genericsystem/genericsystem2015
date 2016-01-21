package org.genericsystem.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ModelContext {

	private final ModelContext parent;
	private final Object model;
	private Map<Element<?>, List<ModelContext>> children = new HashMap<Element<?>, List<ModelContext>>() {
		@Override
		public List<ModelContext> get(Object element) {
			List<ModelContext> list = super.get(element);
			if (list == null)
				put((Element<?>) element, list = new ArrayList<>());
			return list;
		}
	};
	private final List<ViewContext<?>> viewContexts = new ArrayList<>();

	public ModelContext(ModelContext parent, Object model) {
		this.parent = parent;
		this.model = model;
	}

	@Override
	public String toString() {
		return "ModelContext : " + model;
	}

	@SuppressWarnings("unchecked")
	public <M> M getModel() {
		return (M) model;
	}

	public ModelContext getParent() {
		return this.parent;
	}

	public List<ModelContext> getChildren(Element<?> childElement) {
		return children.get(childElement);
	}

	public void register(ViewContext<?> viewContext) {
		this.viewContexts.add(viewContext);
	}

	public List<ViewContext<?>> getViewContexts() {
		return viewContexts;
	}
}
