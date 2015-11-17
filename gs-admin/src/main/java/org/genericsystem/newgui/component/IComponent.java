package org.genericsystem.newgui.component;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TableView;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

import org.genericsystem.newgui.context.IContext;
import org.genericsystem.newgui.metacontext.IMetaContext.RootMetaContext.RootContext;
import org.genericsystem.newgui.metacontext.IMetaContext.RootMetaContext.SubContext;

public interface IComponent {
	Node getNode();

	public ObservableList<IComponent> getChildren();

	public ObservableList<Node> getChildrenNodes();

	public IContext getContext();

	public static class LabelComponent extends AbstractComponent {

		public LabelComponent(IComponent parent, IContext context) {
			super(parent, context);
			if (context instanceof RootContext)
				((Label) getNode()).textProperty().bind(((RootContext) context).titleColumnProperty);
			else
				((Label) getNode()).textProperty().bind(((SubContext) context).labelTextProperty);
		}

		@Override
		public ObservableList<Node> getChildrenNodes() {
			return FXCollections.emptyObservableList();
		}

		@Override
		protected Node buildNode() {
			return new Label();
		}
	}

	public static class TableViewComponent<T extends AbstractComponent> extends AbstractComponent {

		public TableViewComponent(IComponent parent, IContext context) {
			super(parent, context);
		}

		@Override
		protected Node buildNode() {
			return new TableView<T>();
		}

		@Override
		public ObservableList<Node> getChildrenNodes() {
			return ((TableView) getNode()).getItems();
		}
	}

	public static class VBoxComponent extends AbstractComponent {

		public VBoxComponent(IComponent parent, IContext context) {
			super(parent, context);
		}

		@Override
		protected Node buildNode() {
			return new VBox();
		}

		@Override
		public ObservableList<Node> getChildrenNodes() {

			return ((Pane) getNode()).getChildren();
		}
	}

	public static class ButtonComponent extends AbstractComponent {

		public ButtonComponent(IComponent parent, IContext context) {
			super(parent, context);
		}

		@Override
		protected Node buildNode() {
			return new Button("button");
		}

		@Override
		public ObservableList<Node> getChildrenNodes() {
			return FXCollections.emptyObservableList();
		}
	}
}
