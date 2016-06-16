package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Nicolas Feybesse
 *
 */
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
		ViewContext<?> previous = viewContextsMap.put(viewContext.getElement(), viewContext);
		assert previous == null;
	}

	public List<ViewContext<?>> getViewContexts() {
		return viewContexts;
	}

	public void destroy() {
		for (ViewContext<?> viewContext : getViewContexts())
			viewContext.destroyChild();
	}

	public static class RootModelContext extends ModelContext {
		public RootModelContext(Model model) {
			super(null, model);
			model.modelContext = this;
		}
	}

	public class ModelContextList {

		private Element<?> childElement;

		private List<ModelContext> internal = new ArrayList<>();

		public ModelContextList(Element<?> childElement) {
			this.childElement = childElement;
		}

		public void insert(int index, Model model, ViewContext<?> viewContext) {
			ModelContext modelContextChild = createChildContext(model);
			model.parent = getModel();// inject parent
			model.modelContext = modelContextChild;
			model.element = childElement;
			model.afterParentConstruct();
			viewContext.createViewContextChild(index, modelContextChild, childElement);
			internal.add(index, modelContextChild);
		};

		public void delete(int index) {
			internal.remove(index).destroy();
		};
	}

	private final Map<Element<?>, ViewContext<?>> viewContextsMap = new LinkedHashMap<>();

	public ViewContext<?> getViewContext(Element<?> element) {
		return viewContextsMap.get(element);
	}
}
