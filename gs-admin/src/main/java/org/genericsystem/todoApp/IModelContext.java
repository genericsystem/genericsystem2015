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

	public AbstractModelContext resolve(Method method);

	public AbstractModelContext resolve(Field field);

	public List<IModelContext> getContextChildren();

	public void destroyChildrenContext(Object model);

	public void destroy();

	public static abstract class AbstractModelContext implements IModelContext {

		protected List<IViewContext> viewContexts = new ArrayList<IViewContext>();
		protected List<IModelContext> modelContextChildren = new ArrayList<IModelContext>();
		protected IModelContext parent;
		protected Object model;

		public AbstractModelContext(IModelContext parent, Object model) {
			super();
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
		public AbstractModelContext resolve(Method method) {
			if (method.getDeclaringClass().isAssignableFrom(this.model.getClass()))
				return this;
			else if (this.parent == null)
				throw new IllegalStateException("Unable to resolve method : " + method);
			else
				return parent.resolve(method);
		}

		@Override
		public AbstractModelContext resolve(Field field) {
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
	}

	public static class ModelContextImpl extends AbstractModelContext {
		public ModelContextImpl(IModelContext parent, Object model) {
			super(parent, model);
		}

		@Override
		public ModelContextImpl createChild(Object child) {
			ModelContextImpl childContext = new ModelContextImpl(this, child);
			this.modelContextChildren.add(childContext);
			return childContext;
		}

	}
}
