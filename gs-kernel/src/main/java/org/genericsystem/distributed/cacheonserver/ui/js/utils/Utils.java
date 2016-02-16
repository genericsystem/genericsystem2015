package org.genericsystem.distributed.cacheonserver.ui.js.utils;

import io.vertx.core.json.JsonObject;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import javafx.collections.ModifiableObservableListBase;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.Pane;

import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.cacheonserver.ui.js.Element;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;

public class Utils {
	static public <PARENTNODE> Function<PARENTNODE, ObservableList<?>> getClassChildren(Element<PARENTNODE> parent) {
		Function<Pane, ObservableList<?>> paneChildren = Pane::getChildren;
		Function<Group, ObservableList<?>> groupChildren = Group::getChildren;
		Function<HtmlNode, List<?>> nodeJsChildren = parentNodeJs -> new ModifiableObservableListBase() {
			private List<HtmlNode> childrenNode = new ArrayList<>();

			@Override
			public Object get(int index) {
				return childrenNode.get(index);
			}

			@Override
			public int size() {
				return childrenNode.size();
			}

			@Override
			protected void doAdd(int index, Object element) {
				JsonObject jsonObj = new JsonObject().put("msg_type", "A");
				jsonObj.put("parentId", parentNodeJs.getId());
				jsonObj.put("nodeId", ((HtmlNode) element).getId());
				jsonObj.put("tagHtml", ((HtmlNode) element).getTag().get());
				jsonObj.put("textContent", ((HtmlNode) element).getText().get());
				GSBuffer bufferAdmin = new GSBuffer();
				bufferAdmin.appendString(jsonObj.encode());
				if (parent instanceof HtmlElement)
					((HtmlElement) parent).getWebSocket().write(bufferAdmin);

				childrenNode.add(((HtmlNode) element));
			}

			@Override
			protected Object doSet(int index, Object element) {
				return childrenNode.set(index, ((HtmlNode) element));
			}

			@Override
			protected Object doRemove(int index) {
				JsonObject jsonObj = new JsonObject().put("msg_type", "R");
				jsonObj.put("nodeId", (childrenNode.get(index)).getId());
				GSBuffer bufferAdmin = new GSBuffer();
				bufferAdmin.appendString(jsonObj.encode());
				if (parent instanceof HtmlElement)
					((HtmlElement) parent).getWebSocket().write(bufferAdmin);

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

		// if (parent != null) {
		if (Pane.class.isAssignableFrom(parent.nodeClass))
			return (Function) paneChildren;
		if (Group.class.isAssignableFrom(parent.nodeClass))
			return (Function) groupChildren;
		if (ScrollPane.class.isAssignableFrom(parent.nodeClass))
			return (Function) scrollChildren;
		if (HtmlNode.class.isAssignableFrom(parent.nodeClass))
			return (Function) nodeJsChildren;
		// }s
		throw new IllegalStateException("Not a supported JavaFX container : ");

	}
}
