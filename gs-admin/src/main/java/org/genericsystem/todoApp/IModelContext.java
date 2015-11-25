package org.genericsystem.todoApp;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public interface IModelContext {

	public IModelContext createChild(Object childModel);

	void registre(IViewContext viewContext);

	public Object getModel();

	public void setModel(Object model);

	public IModelContext getParent();

	public ModelContext resolve(Method method);

	public ModelContext resolve(Field field);

	public List<IModelContext> getContextChildren();

	public void destroyChildrenContext(Object model);

	public void destroy();

	public static class ModelContext implements IModelContext {

		protected List<IViewContext> viewContexts = new ArrayList<>();
		protected List<IModelContext> modelContextChildren = new ArrayList<>();
		protected IModelContext parent;
		protected Object model;

		public ModelContext(IModelContext parent, Object model) {
			this.parent = parent;
			this.model = model;
		}

		@Override
		public void destroy() {
			viewContexts.forEach(viewContext -> {
				viewContext.destroy();
			});
		}

		@Override
		public ModelContext resolve(Method method) {
			if (method.getDeclaringClass().isAssignableFrom(this.model.getClass()))
				return this;
			else if (this.parent == null)
				throw new IllegalStateException("Unable to resolve method : " + method);
			else
				return parent.resolve(method);
		}

		@Override
		public ModelContext resolve(Field field) {
			if (field.getDeclaringClass().isAssignableFrom(this.model.getClass()))
				return this;
			else if (this.parent == null)
				throw new IllegalStateException("Unable to resolve field : " + field);
			else
				return parent.resolve(field);
		}

		@Override
		public Object getModel() {
			return this.model;
		}

		@Override
		public IModelContext getParent() {
			return this.parent;
		}

		@Override
		public void setModel(Object model) {
			this.model = model;
		}

		@Override
		public List<IModelContext> getContextChildren() {
			return this.modelContextChildren;
		}

		@Override
		public void destroyChildrenContext(Object model) {
			Iterator<IModelContext> iterator = modelContextChildren.iterator();
			while (iterator.hasNext()) {
				IModelContext child = iterator.next();
				if (child.getModel() == model) {
					child.destroy();
					iterator.remove();
				}
			}
		}

		@Override
		public void registre(IViewContext viewContext) {
			this.viewContexts.add(viewContext);
		}

		@Override
		public ModelContext createChild(Object child) {
			ModelContext childContext = new ModelContext(this, child);
			this.modelContextChildren.add(childContext);
			return childContext;
		}
	}
}
