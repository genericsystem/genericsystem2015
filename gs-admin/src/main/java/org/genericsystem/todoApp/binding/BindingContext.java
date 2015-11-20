package org.genericsystem.todoApp.binding;

import org.genericsystem.todoApp.IModelContext;
import org.genericsystem.todoApp.IViewContext;

public class BindingContext {
	public IModelContext modelContext;
	public IViewContext viewContext;

	public BindingContext(IModelContext modelContext, IViewContext viewContext) {
		super();
		this.modelContext = modelContext;
		this.viewContext = viewContext;
	}
}
