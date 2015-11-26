package org.genericsystem.todoApp.binding;

import org.genericsystem.todoApp.IModelContext;
import org.genericsystem.todoApp.ViewContext;

public class BindingContext {
	private final IModelContext modelContext;
	private final ViewContext viewContext;

	public BindingContext(IModelContext modelContext, ViewContext viewContext) {
		this.modelContext = modelContext;
		this.viewContext = viewContext;
	}

	public IModelContext getModelContext() {
		return modelContext;
	}

	public ViewContext getViewContext() {
		return viewContext;
	}
}
