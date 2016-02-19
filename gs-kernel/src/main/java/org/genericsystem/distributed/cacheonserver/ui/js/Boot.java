package org.genericsystem.distributed.cacheonserver.ui.js;

import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.collections.ObservableList;

public class Boot<NODE> {

	private final Consumer<NODE> consumer;

	private Boot(Consumer<NODE> consumer) {
		this.consumer = consumer;
	}

	public void init(NODE node) {
		consumer.accept(node);
	}

	public static <NODE, VALUE> Boot<NODE> setProperty(Function<NODE, Property<VALUE>> applyOnNode, VALUE value) {
		return new Boot<>(node -> applyOnNode.apply(node).setValue(value));
	}

	public static <NODE, VALUE> Boot<NODE> addProperty(Function<NODE, ObservableList<VALUE>> applyOnNode, VALUE value) {
		System.out.println(value);
		return new Boot<>(node -> applyOnNode.apply(node).add(value));
	}

	public static <NODE> Boot<NODE> apply(Consumer<NODE> applyOnNode) {
		return new Boot<>(object -> applyOnNode.accept(object));
	}
}
