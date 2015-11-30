package org.genericsystem.todoApp;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;

public class ModelContext {

	private final ModelContext parent;
	private final Object model;
	private final List<ViewContext> viewContexts = new ArrayList<>();
	private final List<ModelContext> children = new AbstractList<ModelContext>() {

		private List<ModelContext> wrappedList = new ArrayList<>();

		@Override
		public ModelContext get(int index) {
			return wrappedList.get(index);
		}

		@Override
		public int size() {
			return wrappedList.size();
		}

		@Override
		public void add(int index, ModelContext element) {
			wrappedList.add(index, element);
		};

		@Override
		public ModelContext remove(int index) {
			ModelContext removed = wrappedList.remove(index);
			for (ViewContext viewContext : removed.viewContexts)
				viewContext.getParent().destroyChild(viewContext);
			return removed;
		};
	};

	public ModelContext(ModelContext parent, Object model) {
		this.parent = parent;
		this.model = model;
	}

	public ModelContext createChild(Object child, ViewContext viewContext) {
		ModelContext childContext = new ModelContext(this, child);
		viewContext.bind(childContext);
		return childContext;
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

}
