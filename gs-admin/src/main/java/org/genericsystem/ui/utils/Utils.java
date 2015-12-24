package org.genericsystem.ui.utils;

import java.util.function.Function;

import javafx.collections.ModifiableObservableListBase;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.Pane;

import org.genericsystem.ui.Element;

public class Utils {
	static public <PARENTNODE> Function<PARENTNODE, ObservableList<?>> getClassChildren(Element<PARENTNODE> parent) {
		Function<Pane, ObservableList<?>> paneChildren = Pane::getChildren;
		Function<Group, ObservableList<?>> groupChildren = Group::getChildren;
		Function<ScrollPane, ObservableList<?>> scrollChildren = scrollPane -> new ModifiableObservableListBase<Node>() {

			@Override
			public Node get(int index) {
				return scrollPane.getContent();
			}

			@Override
			public int size() {
				return scrollPane.getContent() == null ? 0 : 1;
			}

			@Override
			protected void doAdd(int index, Node element) {
				if (size() != 0)
					throw new IllegalStateException("Only one element is supported in a GSScrollPane !");
				scrollPane.setContent(element);
			}

			@Override
			protected Node doSet(int index, Node element) {
				Node result = doRemove(index);
				doAdd(index, element);
				return result;
			}

			@Override
			protected Node doRemove(int index) {
				Node result = scrollPane.getContent();
				scrollPane.setContent(null);
				return result;
			}
		};
		if (Pane.class.isAssignableFrom(parent.nodeClass))
			return (Function) paneChildren;
		if (Group.class.isAssignableFrom(parent.nodeClass))
			return (Function) groupChildren;
		if (ScrollPane.class.isAssignableFrom(parent.nodeClass))
			return (Function) scrollChildren;
		throw new IllegalStateException("Not a supported JavaFX container : " + parent.nodeClass);

	}
}
