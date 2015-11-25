package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.scene.Node;

import org.genericsystem.todoApp.IModelContext.ModelContextImpl;
import org.genericsystem.todoApp.IViewContext.AbstractViewContext;
import org.genericsystem.todoApp.IViewContext.ElementViewContext;
import org.genericsystem.todoApp.binding.Binding.AbstractBinding;

public interface IElement {
	public AbstractViewContext apply(Object model);

	public static class Element implements IElement {
		public Class<? extends Node> classNode;
		public StringProperty text = new SimpleStringProperty();
		public AbstractBinding[] binding;

		public List<IElement> content = new ArrayList<IElement>();

		public Element(Class<? extends Node> classNode, String text, List<IElement> content, AbstractBinding... binding) {
			super();
			this.classNode = classNode;
			this.binding = binding;
			this.content = content;
			this.text.set(text);
		}

		@Override
		public AbstractViewContext apply(Object model) {
			Node node = null;
			node = createNode();
			ModelContextImpl modelContext = new ModelContextImpl(null, model);
			ElementViewContext viewContext = new ElementViewContext(modelContext, this, node, null);
			viewContext.init();
			return viewContext;
		}

		private Node createNode() {
			try {
				return classNode.newInstance();
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}

	}
}
