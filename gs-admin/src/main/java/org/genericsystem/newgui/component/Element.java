package org.genericsystem.newgui.component;

import javafx.scene.Node;
import javafx.scene.layout.VBox;

import org.genericsystem.newgui.context.IModelContext.ModelContextImpl;
import org.genericsystem.newgui.context.IViewContext;
import org.genericsystem.newgui.context.IViewContext.ElementViewContext;

public interface Element {

	public IViewContext apply(Object model);

	// public IComponent apply(IComponent parent, IContext context);

	// public List<Element> getChildren();

	public Node buildNode();

	public static abstract class AbstractElement implements Element {

		public AbstractElement(Element parent) {

		}

		// @Override
		// public List<Element> getChildren() {
		// return null;
		// }

		@Override
		public IViewContext apply(Object model) {
			Node node = buildNode();
			ModelContextImpl modelContext = new ModelContextImpl(model, null);
			ElementViewContext viewContext = new ElementViewContext(modelContext, node, this);
			viewContext.init();
			return viewContext;
		}
	}

	public static class VBoxMetaComponent extends AbstractElement {
		public VBoxMetaComponent(Element parent) {
			super(parent);
		}

		@Override
		public Node buildNode() {
			return new VBox(5);
		}

	}

}

// public static class LabelMetaComponent extends AbstractElement {
//
// public LabelMetaComponent(Element parent) {
// super(parent);
//
// }
//
// @Override
// public IComponent apply(IComponent parent, IContext context) {
// IComponent comp = buildComponent(parent, context);
// return comp;
// }
//
// @Override
// public IComponent buildComponent(IComponent parent, IContext context) {
// return new LabelComponent(parent, context);
// }
//
// }
//
// public static class TableViewMetaComponent extends AbstractElement {
//
// public TableViewMetaComponent(Element parent) {
// super(parent);
// }
//
// @Override
// public IComponent apply(IComponent parent, IContext context) {
// IComponent tableComponent = buildComponent(parent, context);
//
// applyChildren(tableComponent, context);
// return tableComponent;
// }
//
// @Override
// public IComponent buildComponent(IComponent parent, IContext context) {
// return new TableViewComponent(parent, context);
// }
// }
//
// public static class VBoxMetaComponent extends AbstractElement {
// public VBoxMetaComponent(Element parent) {
// super(parent);
// }
//
// @Override
// public IComponent apply(IComponent parent, IContext context) {
// IComponent thisComponent = buildComponent(parent, context);
// applyChildren(thisComponent, context);
// return thisComponent;
// }
//
// @Override
// public IComponent buildComponent(IComponent parent, IContext context) {
// VBoxComponent vb = new VBoxComponent(parent, context);
// System.out.println("VBoxMetaComponent::buildComponent ::" + ((RootContext) context).subContextObservableList.size());
// // ((RootContext) context).subContextObservableList.forEach(sub -> new LabelComponent(vb, sub));
// return vb;
// }
// }
//
// public static class ButtonMetaComponent extends AbstractElement {
// public ButtonMetaComponent(Element parent) {
// super(parent);
// }
//
// @Override
// public IComponent apply(IComponent parent, IContext context) {
// return buildComponent(parent, context);
// }
//
// @Override
// public IComponent buildComponent(IComponent parent, IContext context) {
// return new ButtonComponent(parent, context);
// }
// }

