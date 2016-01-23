package org.genericsystem.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.value.ChangeListener;
import javafx.collections.ListChangeListener;

public class ModelContext {

	private final ModelContext parent;
	private final Object model;
	private final List<ViewContext<?>> viewContexts = new ArrayList<>();
	private Map<Element<?>, ModelContextList> children = new HashMap<Element<?>, ModelContextList>() {
		private static final long serialVersionUID = -2758395427732908902L;

		@Override
		public ModelContextList get(Object element) {
			ModelContextList list = super.get(element);
			if (list == null)
				put((Element<?>) element, list = new ModelContextList((Element<?>) element));
			return list;
		}
	};

	public class ModelContextList {

		private Element<?> childElement;
		private List<ModelContext> internal = new ArrayList<ModelContext>();

		public ModelContextList(Element<?> childElement) {
			this.childElement = childElement;
		}

		public <N> ModelContext insert(int index, Object model, ViewContext<N> viewContext) {
			ModelContext childModelContext = createChildContext(model);
			viewContext.createChildContext(childModelContext, childElement);
			internal.add(index, childModelContext);
			return childModelContext;
		};

		public void delete(int index) {
			internal.remove(index).destroy();
		};

		public <W> ListChangeListener<W> getListChangeListener(ViewContext<?> viewContext) {
			return change -> {
				while (change.next()) {
					if (change.wasPermutated()) {
						for (int i = change.getFrom(); i < change.getTo(); i++)
							delete(change.getFrom());
						int index = change.getFrom();
						for (W model : change.getList().subList(change.getFrom(), change.getTo()))
							insert(index++, model, viewContext);
					} else {
						if (change.wasRemoved())
							for (int i = 0; i < change.getRemovedSize(); i++)
								delete(change.getFrom());
						if (change.wasAdded()) {
							int index = change.getFrom();
							for (W model : change.getAddedSubList())
								insert(index++, model, viewContext);
						}
					}
				}
			};
		}

		public <W> ChangeListener<W> getChangeListener(ViewContext<?> viewContext) {
			return (o, oldModel, newModel) -> {
				if (oldModel == newModel)
					return;
				if (oldModel != null)
					delete(0);
				if (newModel != null)
					insert(0, newModel, viewContext);
			};
		}
	}

	private ModelContext(ModelContext parent, Object model) {
		this.parent = parent;
		this.model = model;
	}

	public ModelContext createChildContext(Object model) {
		return new ModelContext(this, model);
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

	public ModelContextList getChildren(Element<?> childElement) {
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

	public void destroy() {
		for (ViewContext<?> viewContext : getViewContexts())
			viewContext.destroyChild();
	}

	public static class RootModelContext extends ModelContext {

		public RootModelContext(Object model) {
			super(null, model);
		}
	}
}
