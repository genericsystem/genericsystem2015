package org.genericsystem.newgui.component;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;

import org.genericsystem.newgui.context.IModelContext;

public interface IComponent {
	Node getNode();

	public ObservableList<IComponent> getChildren();

	public ObservableList<Node> getChildrenNodes();

	public IModelContext getContext();

	public static abstract class AbstractComponent implements IComponent {

		private Node node;
		private ObservableList<IComponent> children = FXCollections.observableArrayList();
		private IModelContext context;

		abstract protected Node buildNode();

		public AbstractComponent(IComponent parent, IModelContext context) {
			this.context = context;
			node = buildNode();
			if (parent != null)
				parent.getChildrenNodes().add(node);
		}

		@Override
		public Node getNode() {
			return node;
		}

		@Override
		public ObservableList<IComponent> getChildren() {
			return this.children;
		}

		@Override
		public IModelContext getContext() {
			return this.context;
		}
	}

	// public static class LabelComponent extends AbstractComponent {
	//
	// public LabelComponent(IComponent parent, IModelContext context) {
	// super(parent, context);
	// if (context instanceof RootContext)
	// ((Label) getNode()).textProperty().bind(((RootContext) context).titleColumnProperty);
	// else
	// ((Label) getNode()).textProperty().bind(((SubContext) context).labelTextProperty);
	// }
	//
	// @Override
	// public ObservableList<Node> getChildrenNodes() {
	// return FXCollections.emptyObservableList();
	// }
	//
	// @Override
	// protected Node buildNode() {
	// return new Label();
	// }
	// }
	//
	// public static class TableViewComponent<T extends AbstractComponent> extends AbstractComponent {
	//
	// public TableViewComponent(IComponent parent, IModelContext context) {
	// super(parent, context);
	// RootContext rc = (RootContext) context;
	// TableColumn<SubContext, String> column = new TableColumn<SubContext, String>("subContext");
	// ((TableView) getNode()).getColumns().add(column);
	// ((TableView) getNode()).setItems(rc.subContextObservableList);
	// column.setCellValueFactory((g) -> new SimpleObjectProperty(g.getValue().labelTextProperty.getValue()));
	// }
	//
	// @Override
	// protected Node buildNode() {
	// return new TableView<T>();
	// }
	//
	// @Override
	// public ObservableList<Node> getChildrenNodes() {
	// return ((TableView) getNode()).getItems();
	// }
	// }
	//
	// public static class VBoxComponent extends AbstractComponent {
	//
	// public VBoxComponent(IComponent parent, IModelContext context) {
	// super(parent, context);
	// }
	//
	// @Override
	// protected Node buildNode() {
	// return new VBox();
	// }
	//
	// @Override
	// public ObservableList<Node> getChildrenNodes() {
	//
	// return ((Pane) getNode()).getChildren();
	// }
	// }
	//
	// public static class ButtonComponent extends AbstractComponent {
	//
	// public ButtonComponent(IComponent parent, IModelContext context) {
	// super(parent, context);
	// }
	//
	// @Override
	// protected Node buildNode() {
	// return new Button("button");
	// }
	//
	// @Override
	// public ObservableList<Node> getChildrenNodes() {
	// return FXCollections.emptyObservableList();
	// }
	// }
}
