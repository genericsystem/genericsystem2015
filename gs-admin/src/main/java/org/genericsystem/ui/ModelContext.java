package org.genericsystem.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

public class ModelContext {

	private final ModelContext parent;
	private final Object model;
	private Map<Element<?>, List<ModelContext>> children = new HashMap<Element<?>, List<ModelContext>>() {
		private static final long serialVersionUID = -2758395427732908902L;

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

	public <T> Supplier<T> applyOnModel(Function<?, T> methodReference) {
		return () -> {
			ModelContext modelContext_ = this;
			String s = "/";
			while (modelContext_ != null) {
				s += modelContext_.getModel() + "/";
				try {
					return methodReference.apply(modelContext_.getModel());
				} catch (ClassCastException ignore) {
				}
				modelContext_ = modelContext_.getParent();
			}
			throw new IllegalStateException("Unable to resolve a method reference : " + methodReference + " on stack : " + s);
		};
	}

	public <MODEL> MODEL destroy() {
		for (ViewContext<?> viewContext : getViewContexts())
			viewContext.destroyChild();
		return getModel();
	}
}
