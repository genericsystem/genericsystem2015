package org.genericsystem.newgui.component;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;

public abstract class AbstractComponent implements IComponent {

	private Node node;
	private ObservableList<IComponent> children = FXCollections.observableArrayList();

	abstract protected Node buildNode();

	public AbstractComponent(IComponent parent) {
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
	public Node init() {
		// getChildren().forEach(componentChild -> getChildrenNodes().add(componentChild.init()));
		return getNode();
	}
}
