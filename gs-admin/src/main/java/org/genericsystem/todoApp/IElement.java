package org.genericsystem.todoApp;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.scene.Node;

import org.genericsystem.todoApp.IModelContext.ModelContextImpl;
import org.genericsystem.todoApp.IViewContext.AbstractViewContext;
import org.genericsystem.todoApp.IViewContext.ElementViewContext;
import org.genericsystem.todoApp.binding.Binding.BindingImpl;

public interface IElement {
	public AbstractViewContext apply(Object model);

	public static abstract class AbstractElement implements IElement {
		public Class<? extends Node> classNode;
		public StringProperty text = new SimpleStringProperty();
		public BindingImpl binding;

		public List<IElement> content = new ArrayList<IElement>();

		public AbstractElement(Class<? extends Node> classNode, String text, BindingImpl binding, List<IElement> content) {
			super();
			this.classNode = classNode;
			this.binding = binding;
			this.content = content;
			this.text.set(text);
		}

		@Override
		public AbstractViewContext apply(Object model) {
			// System.out.println("apply ::: ");
			Node node = null;
			try {
				node = createNode();
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
			ModelContextImpl modelContext = new ModelContextImpl(null, model);
			ElementViewContext viewContext = new ElementViewContext(modelContext, this, node, null);
			viewContext.init();
			return viewContext;
		}

		private Node createNode() throws InstantiationException, IllegalAccessException {
			// System.out.println("AbstractElement::createNode(" + classNode + ")");
			return classNode.newInstance();
		}

	}

	public static class Element extends AbstractElement {
		public Element(Class<? extends Node> classNode, String text, BindingImpl binding, List<IElement> content) {
			super(classNode, text, binding, content);
			// System.out.println("Element::const");
		}
	}
}
