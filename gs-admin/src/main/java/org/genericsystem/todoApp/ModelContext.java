package org.genericsystem.todoApp;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ModelContext {

	protected List<ViewContext> viewContexts = new ArrayList<>();
	protected List<ModelContext> modelContextChildren = new ArrayList<>();
	protected ModelContext parent;
	protected Object model;

	public ModelContext(ModelContext parent, Object model) {
		this.parent = parent;
		this.model = model;
	}

	public void destroy() {
		for (ViewContext viewContext : viewContexts) {
			viewContext.getParent().destroyChild(viewContext);
		}
	}

	public ModelContext resolve(Class<?> clazz) {
		if (clazz.isAssignableFrom(model.getClass()))
			return this;
		else if (parent == null)
			throw new IllegalStateException("Unable to resolve method class : " + clazz);
		else
			return parent.resolve(clazz);
	}

	public ModelContext resolve(Method method) {
		if (method.getDeclaringClass().isAssignableFrom(model.getClass()))
			return this;
		else if (parent == null)
			throw new IllegalStateException("Unable to resolve method : " + method);
		else
			return parent.resolve(method);
	}

	public ModelContext resolve(Field field) {
		if (field.getDeclaringClass().isAssignableFrom(this.model.getClass()))
			return this;
		else if (parent != null)
			return parent.resolve(field);
		throw new IllegalStateException("Unable to resolve field : " + field);
	}

	public Object getModel() {
		return this.model;
	}

	public ModelContext getParent() {
		return this.parent;
	}

	public void setModel(Object model) {
		this.model = model;
	}

	public List<ModelContext> getContextChildren() {
		return this.modelContextChildren;
	}

	public void destroyChildrenContext(Object model) {
		Iterator<ModelContext> iterator = modelContextChildren.iterator();
		while (iterator.hasNext()) {
			ModelContext child = iterator.next();
			if (child.getModel() == model) {
				child.destroy();
				iterator.remove();
			}
		}
	}

	public void register(ViewContext viewContext) {
		this.viewContexts.add(viewContext);
	}

	public ModelContext createChild(Object child) {
		ModelContext childContext = new ModelContext(this, child);
		this.modelContextChildren.add(childContext);
		return childContext;
	}
}
