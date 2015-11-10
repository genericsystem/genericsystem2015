package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultContext;

public abstract class CheckedContext implements DefaultContext<Generic> {

	private final AbstractRoot root;
	private final Checker checker;

	protected CheckedContext(AbstractRoot root) {
		assert root != null;
		this.root = root;
		this.checker = buildChecker();
	}

	// public abstract long getTs();

	protected Checker buildChecker() {
		return new Checker(this);
	}

	protected Checker getChecker() {
		return checker;
	}

	@Override
	public AbstractRoot getRoot() {
		return root;
	}

	protected void triggersMutation(Generic oldDependency, Generic newDependency) {
	}

	@Override
	public abstract Snapshot<Generic> getDependencies(Generic ancestor);

	public Generic get(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		meta = meta != null ? meta : (Generic) getRoot();
		meta = meta.adjustMeta(components);
		if (meta.getComponents().size() != components.size())
			return null;
		overrides = computeAndCheckOverridesAreReached(meta, overrides, value, components);
		return meta.getDirectInstance(overrides, value, components);
	}
}
