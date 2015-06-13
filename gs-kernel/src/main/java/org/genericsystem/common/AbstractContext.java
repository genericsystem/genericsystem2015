package org.genericsystem.common;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.THandler.AddHandler;
import org.genericsystem.common.THandler.MergeHandler;
import org.genericsystem.common.THandler.SetHandler;
import org.genericsystem.common.THandler.UpdateHandler;
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
		return new SetHandler<>(this, null, Collections.emptyList(), getRoot().getValue(), Arrays.asList(rootComponents(dim))).resolve();
	}

	@Override
	public T setInstance(T meta, List<T> overrides, Serializable value, List<T> components) {
		return new SetHandler<>(this, meta, overrides, value, components).resolve();
	}

	@Override
	public T addInstance(T meta, List<T> overrides, Serializable value, List<T> components) {
		return new AddHandler<>(this, meta, overrides, value, components).resolve();
	}

	@Override
	public T update(T update, List<T> overrides, Serializable newValue, List<T> newComponents) {
		return new UpdateHandler<>(this, update, update.getMeta(), overrides, newValue, newComponents).resolve();
	}

	@Override
	public T merge(T update, List<T> overrides, Serializable newValue, List<T> newComponents) {
		return new MergeHandler<>(this, update, update.getMeta(), overrides, newValue, newComponents).resolve();
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
		return plug(build(null, clazz, meta, supers, value, components, getRoot().isInitialized() ? ApiStatics.USER_TS : ApiStatics.SYSTEM_TS));
	}

	protected T build(Long ts, Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components, long[] otherTs) {
		return getRoot().init(ts, clazz, meta, supers, value, components, otherTs);
	}

}
