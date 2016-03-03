package org.genericsystem.distributed.ui.utils;

import io.vertx.core.json.JsonObject;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import javafx.collections.ModifiableObservableListBase;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.Pane;

import org.genericsystem.distributed.ui.Element;
import org.genericsystem.distributed.ui.HtmlNode;

public class Utils {
	static public <PARENTNODE> Function<PARENTNODE, List<?>> getClassChildren(Element<PARENTNODE> parent) {
		Function<Pane, ObservableList<?>> paneChildren = Pane::getChildren;
		Function<Group, ObservableList<?>> groupChildren = Group::getChildren;

		Function<HtmlNode, List<HtmlNode>> nodeJsChildren = parentNodeJs -> new AbstractList<HtmlNode>() {
			private List<HtmlNode> childrenNode = new ArrayList<>();

			@Override
			public HtmlNode get(int index) {
				return childrenNode.get(index);
			}

			@Override
			public int size() {
				return childrenNode.size();
			}

			@Override
			public void add(int index, HtmlNode htmlNode) {
				JsonObject jsonObj = new JsonObject().put("msg_type", "A");
				htmlNode.fillJsonAdd(parentNodeJs, jsonObj);
				parent.sendMessage(jsonObj);
				childrenNode.add(htmlNode);
			}

			@Override
			public HtmlNode set(int index, HtmlNode element) {
				return childrenNode.set(index, (element));
			}

			@Override
			public HtmlNode remove(int index) {
				JsonObject jsonObj = new JsonObject().put("msg_type", "R");
				childrenNode.get(index).fillJsonRemove(jsonObj);
				parent.sendMessage(jsonObj);
				return childrenNode.remove(index);
			}
		};

		Function<ScrollPane, ObservableList<?>> scrollChildren = scrollPane -> new ModifiableObservableListBase<Node>() {

			@Override
			public Node get(int index) {
				assert size() == 1 && index == 0;
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
				if (size() == 0)
					throw new IllegalStateException();
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
		if (HtmlNode.class.isAssignableFrom(parent.nodeClass))
			return (Function) nodeJsChildren;
		throw new IllegalStateException("Not a supported JavaFX container : ");

	}
}
