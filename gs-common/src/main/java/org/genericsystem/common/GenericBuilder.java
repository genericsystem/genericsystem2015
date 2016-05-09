package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

import org.genericsystem.api.core.annotations.constraints.InstanceValueGenerator.ValueGenerator;
import org.genericsystem.api.core.exceptions.ExistsException;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class GenericBuilder {
	protected final AbstractCache context;
	final Generic meta;
	protected Generic adjustedMeta;
	final List<Generic> overrides;
	protected List<Generic> supers;
	protected final Serializable value;
	protected final List<Generic> components;
	protected Generic gettable;

	GenericBuilder(AbstractCache context, Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		assert overrides != null;
		this.context = context;
		this.meta = meta != null ? meta : (Generic) context.getRoot();
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
		adjustedMeta = meta.adjustMeta(components);// do adjustmeta() on context
		if (!isMeta() && adjustedMeta.getComponents().size() != components.size())
			adjustedMeta = context.setMeta(components.size());
		supers = context.computeAndCheckOverridesAreReached(adjustedMeta, overrides, value, components);
	}

	Generic get() {
		if (gettable == null)
			gettable = adjustedMeta.getDirectInstance(supers, value, components);
		return gettable;
	}

	Generic getEquiv() {
		return adjustedMeta.getDirectEquivInstance(supers, value, components);
	}

	public Generic getOrBuild() {
		Generic instance = get();
		return instance == null ? build() : instance;
	}

	protected Generic build() {
		return gettable = context.buildAndPlug(null, isMeta() ? null : adjustedMeta, supers, value, components);
	}

	Generic add() {
		return context.getRestructurator().rebuildAll(null, () -> build(), context.computePotentialDependencies(adjustedMeta, supers, value, components));
	}

	Generic set(Generic update) {
		assert update != null;
		return context.getRestructurator().rebuildAll(update, () -> build(), context.computeDependencies(update));
	}

	Generic merge(Generic update) {
		assert update != null;
		return context.getRestructurator().rebuildAll(update, () -> getOrBuild(), context.computeDependencies(update));
	}

	public static class AddBuilder extends GenericBuilder {

		AddBuilder(AbstractCache context, Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
			super(context, meta, overrides, value, components);
		}

		public Generic resolve() {
			Generic generic = get();
			if (generic != null)
				context.discardWithException(new ExistsException("An equivalent instance already exists : " + generic.info()));
			return add();
		}
	}

	static class SetBuilder extends GenericBuilder {

		SetBuilder(AbstractCache context, Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
			super(context, meta, overrides, value, components);
		}

		Generic resolve() {
			Generic generic = get();
			if (generic != null)
				return generic;
			generic = getEquiv();
			return generic == null ? add() : set(generic);
		}
	}

	static class UpdateBuilder extends GenericBuilder {

		private final Generic update;

		UpdateBuilder(AbstractCache context, Generic update, Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
			super(context, meta, overrides, value, components);
			this.update = update;
		}

		Generic resolve() {
			Generic generic = get();
			if (generic != null)
				if (update != generic)
					context.discardWithException(new ExistsException("An equivalent instance already exists : " + generic.info()));
			return set(update);
		}
	}

	static class MergeBuilder extends GenericBuilder {

		private final Generic update;

		MergeBuilder(AbstractCache context, Generic update, Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
			super(context, meta, overrides, value, components);
			this.update = update;
		}

		Generic resolve() {
			return merge(update);
		}
	}

	public static class AtomicBuilder extends GenericBuilder {

		protected AtomicBuilder(AbstractCache context, Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
			super(context, meta, overrides, value, components);
		}

		public Generic resolve() {
			return getOrBuild();
		}
	}

	protected static class SetSystemBuilder extends AtomicBuilder {

		private final Class<?> clazz;

		SetSystemBuilder(AbstractCache context, Class<?> clazz, Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
			super(context, meta, overrides, value, components);
			this.clazz = clazz;
		}

		@Override
		Serializable generateValue(Serializable value) {
			return value;
		}

		@Override
		protected Generic build() {
			return gettable = context.buildAndPlug(clazz, isMeta() ? null : adjustedMeta, supers, value, components);
		}

		@Override
		public final Generic resolve() {
			Generic instance = get();
			return instance == null ? build() : instance;
		}
	}

}
