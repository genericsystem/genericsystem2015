package org.genericsystem.common;

import io.reactivex.Observable;

import java.io.Serializable;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.FiltersBuilder;
import org.genericsystem.api.core.IndexFilter;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.tools.Memoizer;
import org.genericsystem.defaults.DefaultConfig.NonHeritableProperty;
import org.genericsystem.defaults.DefaultContext;
import org.genericsystem.defaults.tools.InheritanceComputer;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class CheckedContext implements DefaultContext<Generic> {

	private final Root root;
	private final Checker checker;

	private final Function<Generic, Snapshot<Generic>> getSubInheritingsM = Memoizer.<Generic, Snapshot<Generic>> memoize(generic -> new Snapshot<Generic>() {
		private Observable<Generic> adds = Observable.merge(generic.getInheritings().getAdds(), Observable.fromIterable(generic.getInheritings()).flatMap(g -> getSubInheritingsM.apply(g).getAdds()),
				generic.getInheritings().getAdds().flatMap(g -> getSubInheritingsM.apply(g).getAdds())).share();
		private Observable<Generic> removals = Observable.merge(generic.getInheritings().getRemovals(), Observable.fromIterable(generic.getInheritings()).flatMap(g -> getSubInheritingsM.apply(g).getRemovals()),
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

	private final Function<Generic, Snapshot<Generic>> getSubInstancesM = Memoizer.<Generic, Snapshot<Generic>> memoize(generic -> new Snapshot<Generic>() {
		private Observable<Generic> adds = Observable.merge(Observable.fromIterable(generic.getSubInheritings()).flatMap(g -> g.getInstances().getAdds()), generic.getSubInheritings().getAdds().flatMap(g -> g.getInstances().getAdds())).share();
		private Observable<Generic> removals = Observable.merge(Observable.fromIterable(generic.getSubInheritings()).flatMap(g -> g.getInstances().getRemovals()), generic.getSubInheritings().getAdds().flatMap(g -> g.getInstances().getRemovals())).share();

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

	private final Function<Generic, Function<Generic, Snapshot<Generic>>> getAttributesM = Memoizer.memoize(this::computeGetAttributes);

	private final Function<Generic, Function<Generic, Snapshot<Generic>>> getHoldersM = Memoizer.memoize(this::computeGetHolders);

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

	private Function<Generic, Snapshot<Generic>> computeGetAttributes(Generic generic) {
		return Memoizer.<Generic, Snapshot<Generic>> memoize(attribute -> {
			return new Snapshot<Generic>() {

				InheritanceComputer<Generic> inheritanceComputer = new InheritanceComputer<>(generic, attribute, ApiStatics.STRUCTURAL);

				@Override
				public Stream<Generic> unfilteredStream() {
					Generic nonHeritableProperty = generic.getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
					if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty) || attribute.isInheritanceEnabled())
						return inheritanceComputer.inheritanceStream();
					return generic.getComposites().filter(new IndexFilter(FiltersBuilder.IS_SPECIALIZATION_OF, attribute)).filter(new IndexFilter(FiltersBuilder.HAS_LEVEL, ApiStatics.STRUCTURAL)).stream();
				}

				@Override
				public Observable<Generic> getAdds() {
					return inheritanceComputer.getAdds();
				}

				@Override
				public Observable<Generic> getRemovals() {
					return inheritanceComputer.getRemovals();
				}
			};
		});
	}

	@Override
	public Snapshot<Generic> getAttributes(Generic generic, Generic attribute) {
		return getAttributesM.apply(generic).apply(attribute);
	}

	private Function<Generic, Snapshot<Generic>> computeGetHolders(Generic generic) {
		return Memoizer.<Generic, Snapshot<Generic>> memoize(attribute -> {
			return new Snapshot<Generic>() {

				InheritanceComputer<Generic> inheritanceComputer = new InheritanceComputer<>(generic, attribute, ApiStatics.CONCRETE);

				@Override
				public Stream<Generic> unfilteredStream() {
					Generic nonHeritableProperty = generic.getKey(NonHeritableProperty.class, ApiStatics.NO_POSITION);
					if (nonHeritableProperty == null || attribute.inheritsFrom(nonHeritableProperty) || attribute.isInheritanceEnabled())
						return inheritanceComputer.inheritanceStream();
					return generic.getComposites().filter(new IndexFilter(FiltersBuilder.IS_SPECIALIZATION_OF, attribute)).filter(new IndexFilter(FiltersBuilder.HAS_LEVEL, ApiStatics.CONCRETE)).stream();
				}

				@Override
				public Observable<Generic> getAdds() {
					return inheritanceComputer.getAdds();
				}

				@Override
				public Observable<Generic> getRemovals() {
					return inheritanceComputer.getRemovals();
				}
			};
		});
	}

	@Override
	public Snapshot<Generic> getHolders(Generic generic, Generic attribute) {
		return getHoldersM.apply(generic).apply(attribute);
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
