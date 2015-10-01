package org.genericsystem.common;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.CacheNoStartedException;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.common.GenericBuilder.AddBuilder;
import org.genericsystem.common.GenericBuilder.MergeBuilder;
import org.genericsystem.common.GenericBuilder.SetBuilder;
import org.genericsystem.common.GenericBuilder.UpdateBuilder;
import org.genericsystem.defaults.DefaultCache;
import org.genericsystem.kernel.Statics;

public abstract class Cache extends AbstractCache implements DefaultCache<Generic> {

	private final Restructurator restructurator;
	private IDifferential<Generic> transaction;
	protected Differential differential;
	private final ContextEventListener<Generic> listener;
	private final long cacheId;

	public long shiftTs() throws RollbackException {
		transaction = buildTransaction();
		listener.triggersRefreshEvent();
		return getTs();
	}

	protected abstract IDifferential<Generic> buildTransaction();

	protected Cache(AbstractEngine root) {
		this(root, new ContextEventListener<Generic>() {
		});
	}

	protected Cache(AbstractEngine root, long cacheId) {
		this(root, new ContextEventListener<Generic>() {
		});
	}

	protected Cache(AbstractEngine root, ContextEventListener<Generic> listener) {
		this(root, root.pickNewTs(), listener);
	}

	protected Cache(AbstractEngine root, long cacheId, ContextEventListener<Generic> listener) {
		super(root);
		this.cacheId = cacheId;
		this.restructurator = buildRestructurator();
		this.listener = listener;
		this.transaction = buildTransaction();
		initialize();
	}

	Restructurator getRestructurator() {
		return restructurator;
	}

	public long getCacheId() {
		return cacheId;
	}

	public IDifferential<Generic> getTransaction() {
		return transaction;
	}

	@Override
	public long getTs() {
		return transaction.getTs();
	}

	@Override
	public Snapshot<Generic> getDependencies(Generic vertex) {
		return differential.getDependencies(vertex);
	}

	protected Restructurator buildRestructurator() {
		return new Restructurator(this);
	}

	// long getBirthTs(Generic generic) {
	// return differential.getBirthTs(generic);
	// }

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

	Generic buildAndPlug(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components) {
		return plug(getRoot().build(null, clazz, meta, supers, value, components));
	}

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
		return new SetBuilder(this, meta, overrides, value, components).resolve();
	}

	@Override
	public Generic addInstance(Generic meta, List<Generic> overrides, Serializable value, List<Generic> components) {
		return new AddBuilder(this, meta, overrides, value, components).resolve();
	}

	@Override
	public Generic update(Generic update, List<Generic> overrides, Serializable newValue, List<Generic> newComponents) {
		return new UpdateBuilder(this, update, update.getMeta(), overrides, newValue, newComponents).resolve();
	}

	@Override
	public Generic merge(Generic update, List<Generic> overrides, Serializable newValue, List<Generic> newComponents) {
		return new MergeBuilder(this, update, update.getMeta(), overrides, newValue, newComponents).resolve();
	}

	@Override
	public void forceRemove(Generic generic) {
		getRestructurator().rebuildAll(null, null, computeDependencies(generic));
	}

	@Override
	public void remove(Generic generic) {
		getRestructurator().rebuildAll(null, null, computeRemoveDependencies(generic));
	}

	@Override
	public void conserveRemove(Generic generic) {
		getRestructurator().rebuildAll(generic, () -> generic, computeDependencies(generic));
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

		default void triggersMutationEvent(X oldDependency, X newDependency) {
		}

		default void triggersRefreshEvent() {
		}

		default void triggersClearEvent() {
		}

		default void triggersFlushEvent() {
		}
	}

}
