package org.genericsystem.newgui.component;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TableView;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

public interface IComponent {
	Node getNode();

	public ObservableList<IComponent> getChildren();

	public ObservableList<Node> getChildrenNodes();

	public Node init();

	public static class TableViewComponent<T extends AbstractComponent> extends AbstractComponent {

		public TableViewComponent(IComponent parent) {
			super(parent);
			// TODO Auto-generated constructor stub
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

		public VBoxComponent(IComponent parent) {
			super(parent);
			// TODO Auto-generated constructor stub
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

		public ButtonComponent(IComponent parent) {
			super(parent);
			// TODO Auto-generated constructor stub
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
