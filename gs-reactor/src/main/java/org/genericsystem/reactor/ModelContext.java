package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

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
	}

	public List<ViewContext<?>> getViewContexts() {
		return viewContexts;
	}

	@Deprecated
	public <T> Supplier<T> applyOnModel(Function<Model, T> methodReference) {
		return () -> methodReference.apply(this.getModel());
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

		public void insert(int index, Model model, ViewContext<?> viewContext) {
			ModelContext childModelContext = createChildContext(model);
			model.parent = getModel();// inject parent
			model.element = childElement;
			model.afterParentConstruct();
			viewContext.createChildContext(index, childModelContext, childElement);
			internal.add(index, childModelContext);
		};

		public void delete(int index) {
			internal.remove(index).destroy();
		};
	}
}
