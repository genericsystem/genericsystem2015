package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.scene.Node;
import org.genericsystem.todoApp.IModelContext.ModelContext;
import org.genericsystem.todoApp.IViewContext.ElementViewContext;
import org.genericsystem.todoApp.binding.Binding;

public interface IElement {

	public List<IElement> getChildren();

	public ElementViewContext apply(Object model);

	public static class Element implements IElement {
		public Class<? extends Node> classNode;
		public StringProperty text = new SimpleStringProperty();
		public Binding[] binding;
		private List<IElement> children = new ArrayList<>();

		public Element(Class<? extends Node> classNode, String text, Binding... binding) {
			super();
			this.classNode = classNode;
			this.binding = binding;
			this.text.set(text);
		}

		@Override
		public ElementViewContext apply(Object model) {
			ElementViewContext viewContext = new ElementViewContext(new ModelContext(null, model), this, createNode(), null);
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

		@Override
		public List<IElement> getChildren() {
			return children;
		}

	}

}
