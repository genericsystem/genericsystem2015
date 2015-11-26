package org.genericsystem.todoApp.binding;

import org.genericsystem.todoApp.ModelContext;
import org.genericsystem.todoApp.ViewContext;

public class BindingContext {
	private final ModelContext modelContext;
	private final ViewContext viewContext;

	public BindingContext(ModelContext modelContext, ViewContext viewContext) {
		this.modelContext = modelContext;
		this.viewContext = viewContext;
	}

	public ModelContext getModelContext() {
		return modelContext;
	}

	public ViewContext getViewContext() {
		return viewContext;
	}
}
