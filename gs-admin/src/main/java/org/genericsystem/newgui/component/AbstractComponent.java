package org.genericsystem.newgui.component;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;

import org.genericsystem.newgui.context.IContext;

public abstract class AbstractComponent implements IComponent {

	private Node node;
	private ObservableList<IComponent> children = FXCollections.observableArrayList();
	private IContext context;

	abstract protected Node buildNode();

	public AbstractComponent(IComponent parent, IContext context) {
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
	public IContext getContext() {
		return this.context;
	}
}
