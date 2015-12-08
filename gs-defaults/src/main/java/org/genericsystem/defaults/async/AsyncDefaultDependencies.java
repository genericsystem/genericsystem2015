//package org.genericsystem.defaults.async;
//
//import java.io.Serializable;
//import java.util.ArrayList;
//import java.util.Arrays;
//import java.util.Collections;
//import java.util.Iterator;
//import java.util.List;
//import java.util.Objects;
//import java.util.concurrent.CompletableFuture;
//import java.util.function.Predicate;
//import java.util.stream.Stream;
//
//import org.genericsystem.api.core.IVertex;
//import org.genericsystem.api.core.Snapshot;
//
//public interface AsyncDefaultDependencies<T extends AsyncDefaultVertex<T>> extends IVertex<T> {
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Boolean> isAsyncAlive() {
//		return getAsyncCurrentCache().isAsyncAlive((T) this);
//	}
//
//	default boolean isAsyncAncestorOf(T dependency) {// TODO promise ?
//		return equals(dependency) || (!dependency.isMeta() && isAncestorOf(dependency.getMeta())) || dependency.getSupers().stream().anyMatch(this::isAncestorOf) || dependency.getComponents().stream().filter(x -> x != null).anyMatch(this::isAncestorOf);
//	}
//
//	default AsyncDefaultContext<T> getAsyncCurrentCache() {
//		return (AsyncDefaultContext<T>) getRoot().getCurrentCache();
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<T> getAsyncInstance(Serializable value, T... components) {
//		CompletableFuture<Snapshot<T>> snapshotPromise = getAsyncInstances(value, components);
//		return snapshotPromise.thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<T> getAsyncInstance(T... components) {
//		CompletableFuture<Snapshot<T>> snapshotPromise = getAsyncInstances(components);
//		return snapshotPromise.thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	// user computeAsyncAndCheckOverridesAreReached
//	default CompletableFuture<T> getAsyncInstance(T override, Serializable value, T... components) {
//		CompletableFuture<Snapshot<T>> snapshotPromise = getAsyncInstances(override, value, components);
//		return snapshotPromise.thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	// user computeAsyncAndCheckOverridesAreReached
//	default CompletableFuture<T> getAsyncInstance(List<T> overrides, Serializable value, T... components) {
//		CompletableFuture<Snapshot<T>> snapshotPromise = getAsyncInstances(overrides, value, components);
//		return snapshotPromise.thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncInstances(Serializable value, T... components) {
//		return getAsyncInstances(components).thenApply(instances -> {
//			return instances.filter(valueFilter(value));
//		});
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncInstances() {
//		return getAsyncCurrentCache().getAsyncInstances((T) this);
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncInstances(T... components) {
//		return getAsyncInstances().thenApply(snapshot -> snapshot.filter(componentsFilter(components)));
//	}
//
//	@SuppressWarnings("unchecked")
//	// use computeAsyncAndCheckOverridesAreReached : unimplemented
//	default CompletableFuture<Snapshot<T>> getAsyncInstances(T override, Serializable value, T... components) {
//		return getAsyncInstances(Collections.singletonList(override), value, components);
//	}
//
//	@SuppressWarnings("unchecked")
//	// use computeAsyncAndCheckOverridesAreReached : unimplemented
//	default CompletableFuture<Snapshot<T>> getAsyncInstances(List<T> overrides, Serializable value, T... components) {
//		CompletableFuture<List<T>> supersPromise = getAsyncCurrentCache().computeAsyncAndCheckOverridesAreReached((T) this, overrides, value, Arrays.asList(components));
//		return supersPromise.thenCompose(supers -> getAsyncInstances(value, components).thenApply(instances -> instances.filter(overridesFilter(supers))));
//	}
//
//	static <T extends DefaultVertex<T>> Predicate<T> valueFilter(Serializable value) {
//		return attribute -> Objects.equals(attribute.getValue(), value);
//	}
//
//	static <T extends DefaultVertex<T>> Predicate<T> overridesFilter(List<T> overrides) {
//		return x -> overrides.isEmpty() ? x.getSupers().isEmpty() : filter(x.getSupers(), overrides).test(x);
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<T> getAsyncSubInstance(Serializable value, T... components) {
//		CompletableFuture<Snapshot<T>> subInstancePromise = getAsyncSubInstances(value, components);
//		return subInstancePromise.thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncSubInstances(Serializable value, T... components) {
//		CompletableFuture<Snapshot<T>> subInstancePromise = getAsyncSubInstances(components);
//		return subInstancePromise.thenApply(snapshot -> snapshot.filter(valueFilter(value)));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncSubInstances() {
//		return getAsyncSubInheritings().thenCompose(snapshot -> {
//			CompletableFuture<Stream<T>> internal = CompletableFuture.completedFuture(Stream.of());
//			for (Iterator<T> i = snapshot.iterator(); i.hasNext();) {
//				internal = internal.thenCompose(stream -> i.next().getAsyncInstances().thenApply(instances -> Stream.concat(stream, instances.stream())));
//			}
//			return internal.thenApply(stream -> (Snapshot<T>) stream);
//		});
//		// return () -> getSubInheritings().stream().flatMap(inheriting -> inheriting.getInstances().stream());
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<T> getAsyncSubInstance(T... components) {
//		CompletableFuture<Snapshot<T>> subInstancePromise = getAsyncSubInstances(components);
//		return subInstancePromise.thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncSubInstances(T... components) {
//		return getAsyncSubInstances().thenApply(snapshot -> snapshot.filter(componentsFilter(components)));
//	}
//
//	@SuppressWarnings("unchecked")
//	// use computeAsyncAndCheckOverridesAreReached : unimplemented
//	default CompletableFuture<T> getAsyncSubInstance(T override, Serializable value, T... components) {
//		return getAsyncSubInstances(override, value, components).thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	// use computeAsyncAndCheckOverridesAreReached : unimplemented
//	default CompletableFuture<Snapshot<T>> getAsyncSubInstances(T override, Serializable value, T... components) {
//		return getAsyncSubInstances(Collections.singletonList(override), value, components);
//	}
//
//	@SuppressWarnings("unchecked")
//	// use computeAsyncAndCheckOverridesAreReached : unimplemented
//	default CompletableFuture<T> getAsyncSubInstance(List<T> overrides, Serializable value, T... components) {
//		return getAsyncSubInstances(overrides, value, components).thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	// use computeAsyncAndCheckOverridesAreReached : unimplemented
//	default CompletableFuture<Snapshot<T>> getAsyncSubInstances(List<T> overrides, Serializable value, T... components) {
//		CompletableFuture<List<T>> supersPromise = getAsyncCurrentCache().computeAsyncAndCheckOverridesAreReached((T) this, overrides, value, Arrays.asList(components));
//		return supersPromise.thenApply(supers -> getSubInstances(value, components).filter(overridesFilter(supers)));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<T> getAsyncInheriting(Serializable value, T... components) {
//		return getAsyncInheritings(value, components).thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<T> getAsyncInheriting(T... components) {
//		return getAsyncInheritings(components).thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncInheritings(Serializable value, T... components) {
//		return getAsyncInheritings(components).thenApply(snapshot -> snapshot.filter(valueFilter(value)));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncInheritings() {
//		return getAsyncCurrentCache().getAsyncInheritings((T) this);
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncInheritings(T... components) {
//		return getAsyncInheritings().thenApply(snapshot -> snapshot.filter(componentsFilter(components)));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<T> getAsyncSubInheriting(Serializable value, T... components) {
//		return getAsyncSubInheritings(value, components).thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncSubInheritings(Serializable value, T... components) {
//		return getAsyncSubInheritings(components).thenApply(snapshot -> snapshot.filter(valueFilter(value)));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<T> getAsyncSubInheriting(T... components) {
//		return getAsyncSubInheritings(components).thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncSubInheritings(T... components) {
//		return getAsyncSubInheritings().thenApply(snapshot -> snapshot.filter(componentsFilter(components)));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncSubInheritings() {
//		assert false;
//		return getAsyncInheritings().thenCompose(snapshot -> {
//			CompletableFuture<Stream<T>> internal = CompletableFuture.completedFuture(Stream.of());
//			for (Iterator<T> i = snapshot.iterator(); i.hasNext();) {
//				internal = internal.thenCompose(stream -> i.next().getAsyncInstances().thenApply(instances -> Stream.concat(stream, instances.stream())));
//			}
//			return internal.thenApply(stream -> (Snapshot<T>) Stream.concat(Stream.of((T) this), stream));
//		});
//		// TODO no distinct...
//		// return () -> Stream.concat(Stream.of((T) this), getInheritings().stream().flatMap(inheriting -> inheriting.getSubInheritings().stream())).distinct();
//	}
//
//	default CompletableFuture<T> getAsyncComposite(Serializable value) {
//		return getAsyncComposites(value).thenApply(snapshot -> getAsyncNonAmbiguousResult(snapshot.stream()));
//	}
//
//	default CompletableFuture<Snapshot<T>> getAsyncComposites(Serializable value) {
//		return getAsyncComposites().thenApply(snapshot -> snapshot.filter(valueFilter(value)));
//	}
//
//	@SuppressWarnings("unchecked")
//	default CompletableFuture<Snapshot<T>> getAsyncComposites() {
//		return getAsyncCurrentCache().getAsyncComposites((T) this);
//	}
//
//	@SuppressWarnings("unchecked")
//	static <T extends AsyncDefaultVertex<T>> Predicate<T> componentsFilter(T... components) {
//		return x -> filter(x.getComponents(), Arrays.asList(components)).test(x);
//	}
//
//	static <T extends AsyncDefaultVertex<T>> Predicate<T> filter(List<T> ancestors, List<T> ancestorsReached) {
//		return attribute -> {
//			List<T> attributeAncestors = new ArrayList<>(ancestors);
//			for (T ancestorsReach : ancestorsReached) {
//				T matchedComponent = attributeAncestors.stream().filter(attributeAncestor -> attributeAncestor.equals(ancestorsReach)).findFirst().orElse(null);
//				if (matchedComponent != null)
//					attributeAncestors.remove(matchedComponent);
//				else
//					return false;
//			}
//			return true;
//		};
//	}
//
//	T getAsyncNonAmbiguousResult(Stream<T> stream);
// }
