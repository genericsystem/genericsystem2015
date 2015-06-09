package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultContext;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.kernel.GenericHandler.AddHandler;
import org.genericsystem.kernel.GenericHandler.MergeHandler;
import org.genericsystem.kernel.GenericHandler.SetHandler;
import org.genericsystem.kernel.GenericHandler.UpdateHandler;

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

	T[] rootComponents(int dim) {
		T[] components = getRoot().newTArray(dim);
		Arrays.fill(components, root);
		return components;
	}

	protected T getMeta(int dim) {
		T adjustedMeta = (root).adjustMeta(rootComponents(dim));
		return adjustedMeta != null && adjustedMeta.getComponents().size() == dim ? adjustedMeta : null;
	}

	T setMeta(int dim) {
		return new SetHandler<>(this, null, Collections.emptyList(), getRoot().getValue(), Arrays.asList(rootComponents(dim))).resolve();
	}

	@Override
	public T addInstance(T meta, List<T> overrides, Serializable value, List<T> components) {
		return new AddHandler<>(this, meta, overrides, value, components).resolve();
	}

	@Override
	public T setInstance(T meta, List<T> overrides, Serializable value, List<T> components) {
		return new SetHandler<>(this, meta, overrides, value, components).resolve();
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
		return buildAndPlug(null, clazz, meta, supers, value, components, new LifeManager(getRoot().isInitialized() ? LifeManager.USER_TS : LifeManager.SYSTEM_TS));
	}

	// archiver acces
	T buildAndPlug(Long ts, Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components, LifeManager lifeManager) {
		return plug(build(ts, clazz, meta, supers, value, components, lifeManager));
	}

	private T build(Long ts, Class<?> clazz, T meta, List<T> supers, Serializable value, List<T> components, LifeManager lifeManager) {
		return getRoot().init(ts, clazz, meta, supers, value, components, lifeManager);
	}

}
