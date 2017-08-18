package org.genericsystem.common;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Supplier;

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

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableIntegerValue;
import javafx.beans.value.ObservableLongValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class AbstractCache extends CheckedContext implements DefaultCache<Generic> {

	public Generic setMeta(int dim) {
		return setInstance(null, Collections.emptyList(), getRoot().getValue(), Arrays.asList(rootComponents(dim)));
	}

	public <U> U safeSupply(Supplier<U> safeExecution) {
		start();
		try {
			return safeExecution.get();
		} finally {
			stop();
		}
	}

	public <U> void safeConsum(Consumer<U> safeExecution) {
		start();
		try {
			safeExecution.accept(null);
		} finally {
			stop();
		}
	}

	@SuppressWarnings("unchecked")
	public final <U extends AbstractCache> U start() {
		return (U) getRoot().start(this);
	}

	public final void stop() {
		getRoot().stop(this);
	}

	private final Restructurator restructurator;
	protected final ObjectProperty<IDifferential<Generic>> transactionProperty;
	protected final ObjectProperty<Differential> differentialProperty = new SimpleObjectProperty<>();;
	private ObservableIntegerValue cacheLevel;
	private ObservableLongValue ts;
	private final ContextEventListener<Generic> listener;
	private Map<Generic, ObservableList<Generic>> dependenciesAsOservableListCacheMap = new HashMap<>();

	public Map<Generic, ObservableList<Generic>> getDependenciesAsOservableListCacheMap() {
		return dependenciesAsOservableListCacheMap;
	}

	@Override
	public long shiftTs() throws RollbackException {
		transactionProperty.set(buildTransaction());
		listener.triggersRefreshEvent();
		return getTs();
	}

	@Override
	public boolean contains(Generic generic) {
		return differentialProperty.getValue().getAdds().contains(generic);
	}

	protected abstract IDifferential<Generic> buildTransaction();

	public AbstractCache(Root root) {
		this(root, new ContextEventListener<Generic>() {
		});
	}

	public AbstractCache(Root root, ContextEventListener<Generic> listener) {
		super(root);
		this.restructurator = buildRestructurator();
		this.listener = listener;
		transactionProperty = new SimpleObjectProperty<>(buildTransaction());
		cacheLevel = Bindings.createIntegerBinding(() -> differentialProperty.get().getCacheLevel(), differentialProperty);
		ts = Bindings.createLongBinding(() -> transactionProperty.get().getTs(), transactionProperty);
		initialize();
	}

	protected Differential getDifferential() {
		return differentialProperty.get();
	}

	Restructurator getRestructurator() {
		return restructurator;
	}

	public ObservableValue<IDifferential<Generic>> getTransactionProperty() {
		return transactionProperty;
	}

	public IDifferential<Generic> getTransaction() {
		return transactionProperty.get();
	}

	public long getTs() {
		return getTransaction().getTs();
	}

	@Override
	public ObservableLongValue getTsObservableValue() {
		return ts;
	}

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {
		return getDifferential().getDependencies(generic);
	}

	protected Restructurator buildRestructurator() {
		return new Restructurator(this);
	}

	protected void initialize() {
		differentialProperty.set(buildDifferential(getDifferential() == null ? buildTransactionDifferential() : getDifferential().getSubDifferential()));
	}

	protected Differential buildDifferential(IDifferential<Generic> subCache) {
		return new Differential(subCache);
	}

	protected TransactionDifferential buildTransactionDifferential() {
		return new TransactionDifferential();
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
				// throw new ConcurrencyControlException("The timestamp cache (" + getTs() + ") is bigger than the life
				// time out : " + Statics.LIFE_TIMEOUT);

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

	protected void doSynchronizedApplyInSubContext()
			throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		Differential originalCacheElement = getDifferential();
		// if (getDifferential().getSubDifferential() instanceof Differential)
		// this.differentialProperty.set((Differential) getDifferential().getSubDifferential());
		// try {
		synchronizedApply(originalCacheElement);
		// } finally {
		// this.differentialProperty.set(originalCacheElement);
		// }
	}

	private void synchronizedApply(Differential cacheElement)
			throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		synchronized (getRoot()) {
			cacheElement.apply();
		}
	}

	@Override
	public void clear() {
		initialize();
		listener.triggersClearEvent();
		listener.triggersRefreshEvent();
	}

	@Override
	public void mount() {
		differentialProperty.set(buildDifferential(getDifferential()));
	}

	@Override
	public void unmount() {
		IDifferential<Generic> subCache = getDifferential().getSubDifferential();
		differentialProperty
				.set(subCache instanceof Differential ? (Differential) subCache : new Differential(subCache));
		listener.triggersClearEvent();
		listener.triggersRefreshEvent();
	}

	@Override
	protected void triggersMutation(Generic oldDependency, Generic newDependency) {
		if (listener != null)
			listener.triggersMutationEvent(oldDependency, newDependency);
	}

	public Generic buildAndPlug(Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components) {
		return plug(getRoot().build(null, clazz, meta, supers, value, components));
	}

	protected Generic plug(Generic generic) {
		assert generic.getBirthTs() == Long.MAX_VALUE || generic.getBirthTs() == 0L : generic.info()
				+ generic.getBirthTs();
		getDifferential().plug(generic);
		getChecker().checkAfterBuild(true, false, generic);
		return generic;
	}

	protected void unplug(Generic generic) {
		getChecker().checkAfterBuild(false, false, generic);
		getDifferential().unplug(generic);
	}

	protected void unplugAll(Collection<Generic> generics) {
		getDifferential().unplugAll(generics, getChecker());
	}

	protected void checkConstraints() throws RollbackException {
		getDifferential().checkConstraints(getChecker());
	}

	@Override
	public void discardWithException(Throwable exception) throws RollbackException {
		clear();
		throw new RollbackException(exception);
	}

	public int getCacheLevel() {
		return getDifferential().getCacheLevel();
	}

	@Override
	public ObservableIntegerValue getCacheLevelObservableValue() {
		return cacheLevel;
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

	protected class TransactionDifferential implements IDifferential<Generic> {

		@Override
		public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds)
				throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
			getTransaction().apply(removes, adds);
		}

		@Override
		public ObjectProperty<IDifferential<Generic>> getDifferentialProperty() {
			return (ObjectProperty) AbstractCache.this.differentialProperty;
		}

		@Override
		public Map<Generic, ObservableList<Generic>> getDependenciesAsOservableListCacheMap() {
			return AbstractCache.this.getDependenciesAsOservableListCacheMap();
		}

		@Override
		public Snapshot<Generic> getDependencies(Generic generic) {
			return getTransaction().getDependencies(generic);
		}

		@Override
		public long getTs() {
			return getTransaction().getTs();
		}

		@Override
		public final Observable getObservable(Generic generic) {
			return transactionProperty;
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
