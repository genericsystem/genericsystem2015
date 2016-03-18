package org.genericsystem.distributed.ui;

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
	private final Model model;
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

	private ModelContext(ModelContext parent, Model model) {
		this.parent = parent;
		this.model = model;
	}

	public ModelContext createChildContext(Model model) {
		return new ModelContext(this, model);
	}

	@Override
	public String toString() {
		return "ModelContext : " + model;
	}

	@SuppressWarnings("unchecked")
	public <M extends Model> M getModel() {
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

	public <T> Supplier<T> applyOnModel(Function<Model, T> methodReference) {
		return () -> {
			ModelContext modelContext_ = this;
			String s = "/" + modelContext_.getModel() + "/";
			try {
				return methodReference.apply(modelContext_.getModel());
			} catch (ClassCastException ignore) {
				return methodReference.apply(modelContext_.getModel());
				// throw new IllegalStateException("Unable to resolve a method reference : " + methodReference + " on stack : " + s);
			}
		};
	}

	public void destroy() {
		for (ViewContext<?> viewContext : getViewContexts())
			viewContext.destroyChild();
	}

	public static class RootModelContext extends ModelContext {
		public RootModelContext(Model model) {
			super(null, model);
		}
	}

	public class ModelContextList {

		private Element<?> childElement;
		private List<ModelContext> internal = new ArrayList<>();

		public ModelContextList(Element<?> childElement) {
			this.childElement = childElement;
		}

		public <N> void insert(int index, Model model, ViewContext<N> viewContext) {
			ModelContext childModelContext = createChildContext(model);
			model.parent = getModel();// inject parent
			model.afterParentConstruct();
			viewContext.createChildContext(childModelContext, childElement);
			internal.add(index, childModelContext);
		};

		public void delete(int index) {
			System.out.println("delete : " + index);
			internal.remove(index).destroy();
		};

		public ListChangeListener<Model> getListChangeListener(ViewContext<?> viewContext) {
			return change -> {
				while (change.next()) {
					if (change.wasPermutated()) {
						for (int i = change.getFrom(); i < change.getTo(); i++)
							delete(change.getFrom());
						int index = change.getFrom();
						for (Model model : change.getList().subList(change.getFrom(), change.getTo()))
							insert(index++, model, viewContext);
					} else {
						if (change.wasRemoved())
							for (int i = 0; i < change.getRemovedSize(); i++)
								delete(change.getFrom());
						if (change.wasAdded()) {
							int index = change.getFrom();
							for (Model model : change.getAddedSubList())
								insert(index++, model, viewContext);
						}
					}
				}
			};
		}

		public ChangeListener<Model> getChangeListener(ViewContext<?> viewContext) {
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
}
