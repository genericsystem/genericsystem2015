package org.genericsystem.common;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.GenericBuilder.AddBuilder;
import org.genericsystem.common.GenericBuilder.MergeBuilder;
import org.genericsystem.common.GenericBuilder.SetBuilder;
import org.genericsystem.common.GenericBuilder.UpdateBuilder;
import org.genericsystem.defaults.DefaultContext;
import org.genericsystem.defaults.DefaultVertex;

public abstract class AbstractContext<T extends DefaultVertex<T>> implements DefaultContext<T> {

	private final AbstractRoot<T> root;
	private final Checker<T> checker;
	private final Restructurator<T> restructurator;

	protected AbstractContext(AbstractRoot<T> root) {
		assert root != null;
		this.root = root;
		this.checker = buildChecker();
		this.restructurator = buildRestructurator();
	}

	public abstract long getTs();

	protected Checker<T> buildChecker() {
		return new Checker<>(this);
	}

	protected Restructurator<T> buildRestructurator() {
		return new Restructurator<>(this);
	}

	protected Checker<T> getChecker() {
		return checker;
	}

	Restructurator<T> getRestructurator() {
		return restructurator;
	}

	@Override
	public AbstractRoot<T> getRoot() {
		return root;
	}

	protected T setMeta(int dim) {
		return new SetBuilder<>(this, null, Collections.emptyList(), getRoot().getValue(), Arrays.asList(rootComponents(dim))).resolve();
	}

	@Override
	public T setInstance(T meta, List<T> overrides, Serializable value, List<T> components) {
		return new SetBuilder<>(this, meta, overrides, value, components).resolve();
	}

	@Override
	public T addInstance(T meta, List<T> overrides, Serializable value, List<T> components) {
		return new AddBuilder<>(this, meta, overrides, value, components).resolve();
	}

	@Override
	public T update(T update, List<T> overrides, Serializable newValue, List<T> newComponents) {
		return new UpdateBuilder<>(this, update, update.getMeta(), overrides, newValue, newComponents).resolve();
	}

	@Override
	public T merge(T update, List<T> overrides, Serializable newValue, List<T> newComponents) {
		return new MergeBuilder<>(this, update, update.getMeta(), overrides, newValue, newComponents).resolve();
	}

	@Override
	public void forceRemove(T generic) {
		getRestructurator().rebuildAll(null, null, computeDependencies(generic));
	}

	@Override
	public void remove(T generic) {
		getRestructurator().rebuildAll(null, null, computeRemoveDependencies(generic));
	}

	@Override
	public void conserveRemove(T generic) {
		getRestructurator().rebuildAll(generic, () -> generic, computeDependencies(generic));
	}

	protected abstract T plug(T generic);

	protected abstract void unplug(T generic);

	protected void triggersMutation(T oldDependency, T newDependency) {
	}

	@Override
	public abstract Snapshot<T> getDependencies(T ancestor);

	T buildAndPlug(Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components) {
		return plug(getRoot().build(null, clazz, meta, supers, value, components));
	}

	@SuppressWarnings("unchecked")
	public final <U extends AbstractContext<T>> U start() {
		return (U) getRoot().start(this);
	}

	public final void stop() {
		getRoot().stop(this);
	}

	public void flush() {

	}

}
