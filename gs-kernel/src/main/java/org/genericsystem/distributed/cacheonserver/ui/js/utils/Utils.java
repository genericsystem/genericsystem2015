package org.genericsystem.distributed.cacheonserver.ui.js.utils;

import io.vertx.core.http.ServerWebSocket;

import java.util.function.Function;

import javafx.collections.ModifiableObservableListBase;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.Pane;

import org.genericsystem.distributed.GSBuffer;
import org.genericsystem.distributed.cacheonserver.ui.js.Element;
import org.genericsystem.distributed.cacheonserver.ui.js.NodeJs;

public class Utils {
	static public <PARENTNODE> Function<PARENTNODE, ObservableList<?>> getClassChildren(Element<PARENTNODE> parent, ServerWebSocket webSocket) {
		Function<Pane, ObservableList<?>> paneChildren = Pane::getChildren;
		Function<Group, ObservableList<?>> groupChildren = Group::getChildren;
		Function<NodeJs, ObservableList<?>> nodeJsChildren = parentNodeJs -> new ModifiableObservableListBase() {

			@Override
			public Object get(int index) {
				// TODO Auto-generated method stub
				System.out.println("ddddd");
				return null;
			}

			@Override
			public int size() {
				// TODO Auto-generated method stub
				System.out.println("ddddd");
				return 0;
			}

			@Override
			protected void doAdd(int index, Object element) {
				GSBuffer bufferAdmin = new GSBuffer();
				bufferAdmin.appendString(parentNodeJs.getId() + ((NodeJs) element).getId() + ((NodeJs) element).getTag());
				webSocket.writeBinaryMessage(bufferAdmin);
				
			}

			@Override
			protected Object doSet(int index, Object element) {
				// TODO Auto-generated method stub
				System.out.println("ddddd");
				return null;
			}

			@Override
			protected Object doRemove(int index) {
				// TODO Auto-generated method stub
				System.out.println("ddddd");
				return null;
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
		if (NodeJs.class.isAssignableFrom(parent.nodeClass))
			return (Function) nodeJsChildren;
		// }s
		throw new IllegalStateException("Not a supported JavaFX container : ");

	}
}
