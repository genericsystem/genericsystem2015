package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.CacheNoStartedException;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.defaults.DefaultCache;
import org.genericsystem.distributed.LightClientEngine;
import org.genericsystem.kernel.Statics;

public abstract class LightCache extends AbstractCache implements DefaultCache<Generic> {

	private IDifferential<Generic> transaction;
	protected Differential differential;
	private final ContextEventListener<Generic> listener;

	public void shiftTs() throws RollbackException {
		transaction = buildTransaction();
		listener.triggersRefreshEvent();
	}

	protected abstract IDifferential<Generic> buildTransaction();

	protected LightCache(LightClientEngine root) {
		this(root, new ContextEventListener<Generic>() {});
	}

	public IDifferential<Generic> getTransaction() {
		return transaction;
	}

	protected LightCache(LightClientEngine root, ContextEventListener<Generic> listener) {
		super(root);
		this.listener = listener;
		this.transaction = buildTransaction();
		initialize();
	}

	@Override
	public LightClientEngine getRoot() {
		return (LightClientEngine) super.getRoot();
	}

	@Override
	public long getTs() {
		return transaction.getTs();
	}

	@Override
	public Snapshot<Generic> getDependencies(Generic vertex) {
		return differential.getDependencies(vertex);
	}

	protected void initialize() {
		differential = new Differential(differential == null ? new TransactionDifferential() : differential.getSubCache());
	}

	@Override
	public void tryFlush() throws ConcurrencyControlException {
		if (!equals(getRoot().getCurrentCache()))
			discardWithException(new CacheNoStartedException("The Cache isn't started"));
		try {
			checkConstraints();
			doSynchronizedApplyInSubContext();
			initialize();
			listener.triggersFlushEvent();
		} catch (OptimisticLockConstraintViolationException exception) {
			discardWithException(exception);
		}
	}

	@Override
	public void flush() {
		// System.out.println("FLUSH");
		Throwable cause = null;
		for (int attempt = 0; attempt < Statics.ATTEMPTS; attempt++) {
			try {
				// System.out.println("TRYFLUSH");
				// TODO reactivate this
				// if (getEngine().pickNewTs() - getTs() >= timeOut)
				// throw new ConcurrencyControlException("The timestamp cache (" + getTs() + ") is bigger than the life time out : " + Statics.LIFE_TIMEOUT);
				tryFlush();
				return;
			} catch (ConcurrencyControlException e) {
				cause = e;
				try {
					Thread.sleep(Statics.ATTEMPT_SLEEP);
					shiftTs();
				} catch (InterruptedException ex) {
					discardWithException(ex);
				}
			}
		}
		discardWithException(cause);
	}

	protected void doSynchronizedApplyInSubContext() throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		Differential originalCacheElement = this.differential;
		if (this.differential.getSubCache() instanceof Differential)
			this.differential = (Differential) this.differential.getSubCache();
		try {
			synchronizedApply(originalCacheElement);
		} finally {
			this.differential = originalCacheElement;
		}
	}

	private void synchronizedApply(Differential cacheElement) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		synchronized (getRoot()) {
			cacheElement.apply();
		}
	}

	public void clear() {
		initialize();
		listener.triggersClearEvent();
		listener.triggersRefreshEvent();
	}

	public void mount() {
		differential = new Differential(differential);
	}

	public void unmount() {
		IDifferential<Generic> subCache = differential.getSubCache();
		differential = subCache instanceof Differential ? (Differential) subCache : new Differential(subCache);
		listener.triggersClearEvent();
		listener.triggersRefreshEvent();
	}

	@Override
	protected void triggersMutation(Generic oldDependency, Generic newDependency) {
		if (listener != null)
			listener.triggersMutationEvent(oldDependency, newDependency);
	}

	// Generic buildAndPlug(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components) {
	// return plug(getRoot().build(null, clazz, meta, supers, value, components));
	// }

	protected Generic plug(Generic generic) {
		assert generic.getBirthTs() == Long.MAX_VALUE || generic.getBirthTs() == 0L : generic.info() + generic.getBirthTs();
		differential.plug(generic);
		getChecker().checkAfterBuild(true, false, generic);
		return generic;
	}

	protected void unplug(Generic generic) {
		getChecker().checkAfterBuild(false, false, generic);
		differential.unplug(generic);
	}

	protected void checkConstraints() throws RollbackException {
		differential.checkConstraints(getChecker());
	}

	@Override
	public void discardWithException(Throwable exception) throws RollbackException {
		clear();
		throw new RollbackException(exception);
	}

	public int getCacheLevel() {
		return differential.getCacheLevel();
	}

	@Override
	public Generic setInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericById(
				getRoot().getServer().setInstance(meta.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));
	}

	@Override
	public Generic addInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericByVertex(
				getRoot().getServer().addInstance(meta.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));

	}

	@Override
	public Generic update(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericByVertex(
				getRoot().getServer().update(update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));

	}

	@Override
	public Generic merge(Generic update, List<Generic> overrides, Serializable value, List<Generic> components) {
		return getRoot().getGenericById(
				getRoot().getServer().merge(update.getTs(), overrides.stream().map(override -> override.getTs()).collect(Collectors.toList()), value, components.stream().map(component -> component.getTs()).collect(Collectors.toList())));

	}

	@Override
	public void forceRemove(Generic generic) {
		getRoot().getServer().forceRemove(generic.getTs());
		// getRestructurator().rebuildAll(null, null, computeDependencies(generic));
	}

	@Override
	public void remove(Generic generic) {
		getRoot().getServer().remove(generic.getTs());
		// getRestructurator().rebuildAll(null, null, computeRemoveDependencies(generic));
	}

	@Override
	public void conserveRemove(Generic generic) {
		getRoot().getServer().conserveRemove(generic.getTs());
		// getRestructurator().rebuildAll(generic, () -> generic, computeDependencies(generic));
	}

	private class TransactionDifferential implements IDifferential<Generic> {

		@Override
		public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
			transaction.apply(removes, adds);
		}

		@Override
		public Snapshot<Generic> getDependencies(Generic vertex) {
			return transaction.getDependencies(vertex);
		}

		@Override
		public long getTs() {
			return transaction.getTs();
		}
	}

	public static interface ContextEventListener<X> {

		default void triggersMutationEvent(X oldDependency, X newDependency) {}

		default void triggersRefreshEvent() {}

		default void triggersClearEvent() {}

		default void triggersFlushEvent() {}
	}

}
