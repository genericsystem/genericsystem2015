// package org.genericsystem.reactor;
//
// import java.util.Set;
// import java.util.function.Consumer;
// import java.util.function.Function;
//
// import javafx.beans.property.Property;
//
/// **
// * @author Nicolas Feybesse
// *
// * @param <NODE>
// */
// @Deprecated
// public class Boot<NODE> {
//
// private final Consumer<NODE> consumer;
//
// private Boot(Consumer<NODE> consumer) {
// this.consumer = consumer;
// }
//
// public void init(NODE node) {
// consumer.accept(node);
// }
//
// @Deprecated
// public static <NODE, VALUE> Boot<NODE> setProperty(Function<NODE, Property<VALUE>> applyOnNode, VALUE value) {
// return new Boot<>(node -> applyOnNode.apply(node).setValue(value));
// }
//
// @Deprecated
// public static <NODE, VALUE> Boot<NODE> addProperty(Function<NODE, Set<VALUE>> applyOnNode, VALUE value) {
// return new Boot<>(node -> applyOnNode.apply(node).add(value));
// }
//
// // public static <NODE, VALUE> Boot<NODE> addProperty(Function<NODE, ObservableMap<VALUE, VALUE>> applyOnNode, VALUE attr, VALUE value) {
// // return new Boot<>(node -> applyOnNode.apply(node).put(attr, value));
// // }
//
// @Deprecated
// public static <NODE> Boot<NODE> apply(Consumer<NODE> applyOnNode) {
// return new Boot<>(object -> applyOnNode.accept(object));
// }
// }
