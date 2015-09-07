package org.genericsystem.common;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.defaults.DefaultContext;
import org.genericsystem.kernel.Generic;

public abstract class AbstractContext implements DefaultContext<Generic> {

	private final AbstractRoot root;
	private final Checker checker;

	protected AbstractContext(AbstractRoot root) {
		assert root != null;
		this.root = root;
		this.checker = buildChecker();
	}

	public abstract long getTs();

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

}
