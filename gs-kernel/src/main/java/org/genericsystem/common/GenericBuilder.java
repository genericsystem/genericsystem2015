package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

import org.genericsystem.api.core.annotations.constraints.InstanceValueGenerator.ValueGenerator;
import org.genericsystem.api.core.exceptions.ExistsException;
import org.genericsystem.defaults.DefaultVertex;

public abstract class GenericBuilder<T extends DefaultVertex<T>> {
	protected final AbstractContext<T> context;
	final T meta;
	protected T adjustedMeta;
	final List<T> overrides;
	protected List<T> supers;
	protected final Serializable value;
	protected final List<T> components;
	protected T gettable;

	GenericBuilder(AbstractContext<T> context, T meta, List<T> overrides, Serializable value, List<T> components) {
		assert overrides != null;
		this.context = context;
		this.meta = meta != null ? meta : (T) context.getRoot();
		this.overrides = overrides;
		this.components = components;
		this.value = generateValue(value);
		check();
		adjust();
	}

	Serializable generateValue(Serializable value) {
		Class<? extends ValueGenerator> instanceValueGenerator = meta.getInstanceValueGenerator();
		if (instanceValueGenerator != null) {
			try {
				return instanceValueGenerator.newInstance().generateInstanceValue(meta, supers, value, components);
			} catch (InstantiationException | IllegalAccessException e) {
				context.discardWithException(e);
			}
		}
		return value;
	}

	private void check() {
		context.getChecker().checkBeforeBuild(meta, overrides, value, components);
	}

	protected boolean isMeta() {
		return Objects.equals(context.getRoot().getValue(), value) && components.stream().allMatch(context.getRoot()::equals);
	}

	void adjust() {
		adjustedMeta = meta.adjustMeta(components);
		if (!isMeta() && adjustedMeta.getComponents().size() != components.size())
			adjustedMeta = context.setMeta(components.size());
		supers = context.computeAndCheckOverridesAreReached(adjustedMeta, overrides, value, components);

	}

	T get() {
		if (gettable == null)
			gettable = adjustedMeta.getDirectInstance(supers, value, components);
		return gettable;
	}

	T getEquiv() {
		return adjustedMeta.getDirectEquivInstance(supers, value, components);
	}

	public T getOrBuild() {
		T instance = get();
		return instance == null ? build() : instance;
	}

	protected T build() {
		return gettable = context.buildAndPlug(null, isMeta() ? null : adjustedMeta, supers, value, components);
	}

	T add() {
		return context.getRestructurator().rebuildAll(null, () -> build(), context.computePotentialDependencies(adjustedMeta, supers, value, components));
	}

	T set(T update) {
		assert update != null;
		return context.getRestructurator().rebuildAll(update, () -> build(), context.computeDependencies(update));
	}

	T merge(T update) {
		assert update != null;
		return context.getRestructurator().rebuildAll(update, () -> getOrBuild(), context.computeDependencies(update));
	}

	// static class GetHandler extends GenericHandler {
	//
	// GetHandler(Context context, Generic gettable) {
	// super(context, gettable.getMeta(), gettable.getSupers(), gettable.getValue(), gettable.getComponents());
	// this.gettable = gettable;
	// this.adjustedMeta = gettable.getMeta();
	// this.supers = gettable.getSupers();
	// }
	//
	// GetHandler(Context context, Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
	// super(context, meta, overrides, value, components);
	// }
	//
	// Generic resolve() {
	// return get();
	// }
	//
	// @Override
	// Serializable generateValue(Serializable value) {
	// // TODO Auto-generated method stub
	// return null;
	// }
	// }

	public static class AddBuilder<T extends DefaultVertex<T>> extends GenericBuilder<T> {

		AddBuilder(AbstractContext<T> context, T meta, List<T> overrides, Serializable value, List<T> components) {
			super(context, meta, overrides, value, components);
		}

		public T resolve() {
			T generic = get();
			if (generic != null)
				context.discardWithException(new ExistsException("An equivalent instance already exists : " + generic.info()));
			return add();
		}
	}

	static class SetBuilder<T extends DefaultVertex<T>> extends GenericBuilder<T> {

		SetBuilder(AbstractContext<T> context, T meta, List<T> overrides, Serializable value, List<T> components) {
			super(context, meta, overrides, value, components);
		}

		T resolve() {
			T generic = get();
			if (generic != null)
				return generic;
			generic = getEquiv();
			return generic == null ? add() : set(generic);
		}
	}

	static class UpdateBuilder<T extends DefaultVertex<T>> extends GenericBuilder<T> {

		private final T update;

		UpdateBuilder(AbstractContext<T> context, T update, T meta, List<T> overrides, Serializable value, List<T> components) {
			super(context, meta, overrides, value, components);
			this.update = update;
		}

		T resolve() {
			T generic = get();
			if (generic != null)
				if (update != generic)
					context.discardWithException(new ExistsException("An equivalent instance already exists : " + generic.info()));
			return set(update);
		}
	}

	static class MergeBuilder<T extends DefaultVertex<T>> extends GenericBuilder<T> {

		private final T update;

		MergeBuilder(AbstractContext<T> context, T update, T meta, List<T> overrides, Serializable value, List<T> components) {
			super(context, meta, overrides, value, components);
			this.update = update;
		}

		T resolve() {
			return merge(update);
		}
	}

	public static class AtomicBuilder<T extends DefaultVertex<T>> extends GenericBuilder<T> {

		protected AtomicBuilder(AbstractContext<T> context, T meta, List<T> overrides, Serializable value, List<T> components) {
			super(context, meta, overrides, value, components);
		}

		public final T resolve() {
			return getOrBuild();
		}
	}

	protected static class SetSystemBuilder<T extends DefaultVertex<T>> extends AtomicBuilder<T> {

		private final Class<?> clazz;

		SetSystemBuilder(AbstractContext<T> context, Class<?> clazz, T meta, List<T> overrides, Serializable value, List<T> components) {
			super(context, meta, overrides, value, components);
			this.clazz = clazz;
		}

		@Override
		Serializable generateValue(Serializable value) {
			return value;
		}

		@Override
		protected T build() {
			return gettable = context.buildAndPlug(clazz, isMeta() ? null : adjustedMeta, supers, value, components);
		}
	}

}
