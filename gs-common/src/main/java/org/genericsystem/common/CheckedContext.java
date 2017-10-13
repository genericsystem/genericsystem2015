package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.tools.Memoizer;
import org.genericsystem.defaults.DefaultContext;

import io.reactivex.Observable;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class CheckedContext implements DefaultContext<Generic> {

	private final Root root;
	private final Checker checker;

	private final Function<Generic, Snapshot<Generic>> getSubInheritingsM = Memoizer.memoize(generic -> new Snapshot<Generic>() {
		private Observable<Generic> adds = Observable.merge(generic.getInheritings().getAdds(),
				Observable.fromIterable(generic.getInheritings()).flatMap(g -> getSubInheritingsM.apply(g).getAdds()),
				generic.getInheritings().getAdds().flatMap(g -> getSubInheritingsM.apply(g).getAdds())).share();
		private Observable<Generic> removals = Observable.merge(generic.getInheritings().getRemovals(),
				Observable.fromIterable(generic.getInheritings()).flatMap(g -> getSubInheritingsM.apply(g).getRemovals()),
				generic.getInheritings().getAdds().flatMap(g -> getSubInheritingsM.apply(g).getRemovals())).share();

		@Override
		public Stream<Generic> unfilteredStream() {
			return Stream.concat(Stream.of(generic), generic.getInheritings().stream().flatMap(inheriting -> getSubInheritingsM.apply(inheriting).stream())).distinct();
		}

		@Override
		public Observable<Generic> getAdds() {
			return adds;
		}

		@Override
		public Observable<Generic> getRemovals() {
			return removals;
		}
	});

	private final Function<Generic, Snapshot<Generic>> getSubInstancesM = Memoizer.memoize(generic -> new Snapshot<Generic>() {
		private Observable<Generic> adds = Observable.merge(Observable.fromIterable(generic.getSubInheritings()).flatMap(g -> g.getInstances().getAdds()),
				generic.getSubInheritings().getAdds().flatMap(g -> g.getInstances().getAdds())).share();
		private Observable<Generic> removals = Observable.merge(Observable.fromIterable(generic.getSubInheritings()).flatMap(g -> g.getInstances().getRemovals()),
				generic.getSubInheritings().getAdds().flatMap(g -> g.getInstances().getRemovals())).share();

		@Override
		public Stream<Generic> unfilteredStream() {
			return generic.getSubInheritings().stream().flatMap(inheriting -> inheriting.getInstances().stream());
		}

		@Override
		public Observable<Generic> getAdds() {
			return adds;
		}

		@Override
		public Observable<Generic> getRemovals() {
			return removals;
		}
	});

	protected CheckedContext(Root root) {
		assert root != null;
		this.root = root;
		this.checker = buildChecker();
	}

	protected Checker buildChecker() {
		return new Checker(this);
	}

	protected Checker getChecker() {
		return checker;
	}

	@Override
	public Root getRoot() {
		return root;
	}

	protected void triggersMutation(Generic oldDependency, Generic newDependency) {
	}

	@Override
	public abstract Snapshot<Generic> getDependencies(Generic ancestor);

	@Override
	public Snapshot<Generic> getSubInstances(Generic vertex) {
		return getSubInstancesM.apply(vertex);
	}

	@Override
	public Snapshot<Generic> getSubInheritings(Generic vertex) {
		return getSubInheritingsM.apply(vertex);
	}

	public Generic get(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		meta = meta != null ? meta : (Generic) getRoot();
		meta = meta.adjustMeta(components);
		if (meta.getComponents().size() != components.size())
			return null;
		overrides = computeAndCheckOverridesAreReached(meta, overrides, value, components);
		return meta.getDirectInstance(overrides, value, components);
	}
}
