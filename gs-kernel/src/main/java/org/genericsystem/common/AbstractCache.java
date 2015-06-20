package org.genericsystem.common;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.CacheNoStartedException;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.defaults.DefaultVertex;
import org.genericsystem.kernel.Statics;

public abstract class AbstractCache<T extends DefaultVertex<T>> extends AbstractContext<T> {

	protected ITransaction<T> transaction;
	protected Differential<T> differential;
	private final ContextEventListener<T> listener;

	public void shiftTs() throws RollbackException {
		transaction = buildTransaction(getRoot());
		listener.triggersRefreshEvent();
	}

	protected abstract ITransaction<T> buildTransaction(AbstractRoot<T> root);

	protected AbstractCache(AbstractRoot<T> root) {
		this(root, new ContextEventListener<T>() {
		});
	}

	protected AbstractCache(AbstractRoot<T> root, ContextEventListener<T> listener) {
		super(root);
		this.listener = listener;
		this.transaction = buildTransaction(root);
		initialize();
	}

	@Override
	public long getTs() {
		return transaction.getTs();
	}

	@Override
	public Snapshot<T> getDependencies(T vertex) {
		return differential.getDependencies(vertex);
	}

	// long getBirthTs(T generic) {
	// return differential.getBirthTs(generic);
	// }

	protected void initialize() {
		differential = new Differential<>(differential == null ? new TransactionDifferential() : differential.getSubCache());
	}

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
		Throwable cause = null;
		for (int attempt = 0; attempt < Statics.ATTEMPTS; attempt++) {
			try {
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
		Differential<T> originalCacheElement = this.differential;
		if (this.differential.getSubCache() instanceof Differential)
			this.differential = (Differential<T>) this.differential.getSubCache();
		try {
			synchronizedApply(originalCacheElement);
		} finally {
			this.differential = originalCacheElement;
		}
	}

	private void synchronizedApply(Differential<T> cacheElement) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
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
		differential = new Differential<>(differential);
	}

	public void unmount() {
		IDifferential<T> subCache = differential.getSubCache();
		differential = subCache instanceof Differential ? (Differential<T>) subCache : new Differential<>(subCache);
		listener.triggersClearEvent();
		listener.triggersRefreshEvent();
	}

	@Override
	protected void triggersMutation(T oldDependency, T newDependency) {
		if (listener != null)
			listener.triggersMutationEvent(oldDependency, newDependency);
	}

	@Override
	protected T plug(T generic) {
		assert generic.getBirthTs() == Long.MAX_VALUE || generic.getBirthTs() == 0L : generic.info() + generic.getBirthTs();
		differential.plug(generic);
		getChecker().checkAfterBuild(true, false, generic);
		return generic;
	}

	@Override
	protected void unplug(T generic) {
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

	private class TransactionDifferential implements IDifferential<T> {

		@Override
		public void apply(Iterable<T> removes, Iterable<T> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
			transaction.apply(removes, adds);
		}

		@Override
		public Snapshot<T> getDependencies(T vertex) {
			return transaction.getDependencies(vertex);
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
