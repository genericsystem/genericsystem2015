package org.genericsystem.defaults.tools;

import java.util.function.Function;
import java.util.stream.Stream;

import org.genericsystem.api.core.IGeneric;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.tools.Memoizer;

import io.reactivex.Observable;

// Memoized functions for DefaultDependencies since it’s not possible to define them there.
public class MemoizedFunctions {

	@SuppressWarnings({ "rawtypes" })
	public static Function<IGeneric<?>, Snapshot<?>> getSubInheritingsM = Memoizer.memoize(generic -> new Snapshot() {
		private Observable<?> adds = Observable.merge(generic.getInheritings().getAdds(),
				Observable.fromIterable(generic.getInheritings()).flatMap(g -> getSubInheritingsM.apply(g).getAdds()),
				generic.getInheritings().getAdds().flatMap(g -> getSubInheritingsM.apply(g).getAdds())).share();
		private Observable<?> removals = Observable.merge(generic.getInheritings().getRemovals(),
				Observable.fromIterable(generic.getInheritings()).flatMap(g -> getSubInheritingsM.apply(g).getRemovals()),
				generic.getInheritings().getAdds().flatMap(g -> getSubInheritingsM.apply(g).getRemovals())).share();

		@Override
		public Stream<?> unfilteredStream() {
			return Stream.concat(Stream.of(generic), generic.getInheritings().stream().flatMap(inheriting -> getSubInheritingsM.apply(inheriting).stream())).distinct();
		}

		@Override
		public Observable<?> getAdds() {
			return adds;
		}

		@Override
		public Observable<?> getRemovals() {
			return removals;
		}
	});

	@SuppressWarnings("rawtypes")
	public static Function<IGeneric<?>, Snapshot<?>> getSubInstancesM = Memoizer.memoize(generic -> new Snapshot() {
		private Observable<?> adds = Observable.merge(Observable.fromIterable(generic.getSubInheritings()).flatMap(g -> g.getInstances().getAdds()),
				generic.getSubInheritings().getAdds().flatMap(g -> g.getInstances().getAdds())).share();
		private Observable<?> removals = Observable.merge(Observable.fromIterable(generic.getSubInheritings()).flatMap(g -> g.getInstances().getRemovals()),
				generic.getSubInheritings().getAdds().flatMap(g -> g.getInstances().getRemovals())).share();

		@Override
		public Stream<?> unfilteredStream() {
			return generic.getSubInheritings().stream().flatMap(inheriting -> inheriting.getInstances().stream());
		}

		@Override
		public Observable<?> getAdds() {
			return adds;
		}

		@Override
		public Observable<?> getRemovals() {
			return removals;
		}
	});
}
