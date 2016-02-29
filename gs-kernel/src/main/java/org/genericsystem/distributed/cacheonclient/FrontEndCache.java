package org.genericsystem.distributed.cacheonclient;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache;
import org.genericsystem.common.IDifferential;
import org.genericsystem.distributed.cacheonclient.utils.TransitiveInvalidator;

import com.sun.javafx.collections.ObservableListWrapper;

public class FrontEndCache extends HeavyCache {

	protected FrontEndCache(AbstractRoot root) {
		super(root);
	}

	protected FrontEndCache(AbstractRoot root, ContextEventListener<Generic> listener) {
		super(root, listener);
	}

	@Override
	protected AsyncDifferential buildDifferential(IDifferential<Generic> subCache) {
		return new AsyncDifferential((AsyncIDifferential) subCache);
	}

	@Override
	protected TransactionDifferential buildTransactionDifferential() {
		return new AsyncTransactionDifferential();
	}

	protected class AsyncTransactionDifferential extends TransactionDifferential implements AsyncIDifferential {
		@Override
		public Observable getInvalidator(Generic generic) {
			return TransitiveInvalidator.create(transactionProperty, () -> ((AsyncITransaction) transactionProperty.get()).getInvalidator(generic));
		}

		@Override
		public CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic) {
			return ((AsyncITransaction) transactionProperty.get()).getDependenciesPromise(generic);
		}
	}

	@Override
	protected FrontEndTransaction buildTransaction() {
		return new FrontEndTransaction((Engine) (getRoot()), getRoot().pickNewTs());
	}

	@Override
	public AsyncITransaction getTransaction() {
		return (AsyncITransaction) super.getTransaction();
	}

	public ObservableValue<IDifferential<Generic>> getObservableTransaction() {
		return super.getTransactionProperty();
	}

	@Override
	protected AsyncDifferential getDifferential() {
		return (AsyncDifferential) super.getDifferential();
	}

	private Observable getInvalidator(Generic generic) {
		return TransitiveInvalidator.create(differentialProperty, () -> ((AsyncDifferential) differentialProperty.get()).getInvalidator(generic));
	}

	private Map<Generic, ObservableList<Generic>> dependenciesAsOservableListCacheMap = new HashMap<>();

	@Override
	public ObservableList<Generic> getObservableDependencies(Generic generic) {
		ObservableList<Generic> result = dependenciesAsOservableListCacheMap.get(generic);
		if (result == null) {
			result = new ListBinding<Generic>() {
				private final Observable invalidator = getInvalidator(generic);
				{
					bind(invalidator);
				}

				@SuppressWarnings("restriction")
				@Override
				protected ObservableList<Generic> computeValue() {
					return new ObservableListWrapper<>(FrontEndCache.this.getDependencies(generic).toList());
				}
			};
			dependenciesAsOservableListCacheMap.put(generic, result);
		}
		return result;
	}

	//
	//
	// protected class AsyncTransactionDifferential extends TransactionDifferential implements AsyncIDifferential {
	// @Override
	// public ObservableValue<CompletableFuture<Snapshot<Generic>>>
	// getObervableDependenciesPromise(Generic generic) {
	//
	// return new ObjectBinding<CompletableFuture<Snapshot<Generic>>>() {
	//
	// private CompletableFuture<Snapshot<Generic>>
	// currentDependenciesPromise;
	//
	// {
	// currentDependenciesPromise =
	// getTransaction().getDependenciesPromise(generic);
	// bind(transactionProperty);
	// }
	//
	// @Override
	// protected CompletableFuture<Snapshot<Generic>> computeValue() {
	// return currentDependenciesPromise;
	// }
	//
	// @Override
	// protected void onInvalidating() {
	// currentDependenciesPromise =
	// getTransaction().getDependenciesPromise(generic);
	// invalidate();
	// }
	//
	// };
	// }
	// @Override
	// public ObservableSnapshot<Generic>
	// getDependenciesObservableSnapshot(Generic generic) {
	//
	// return new ObservableSnapshotBinding(generic,
	// getTransaction().getDependenciesObservableSnapshot(generic),
	// transactionProperty);
	// }
	//
	// private class ObservableSnapshotBinding extends SetBinding<Generic>
	// implements ObservableSnapshot<Generic> {
	//
	// private final ObjectBinding<ObservableSnapshot<Generic>> binding;
	//
	// ObservableSnapshotBinding(Generic generic,
	// ObservableSnapshot<Generic> dependencies,
	// ObjectProperty<IDifferential<Generic>> transactionProperty) {
	// bind(binding = new
	// GSObjectBinding<ObservableSnapshot<Generic>>(dependencies,
	// transactionProperty, false) {
	//
	// @Override
	// protected void onInvalidating() {
	// changeBindedObject(getTransaction().getDependenciesObservableSnapshot(generic));
	// }
	//
	// });
	// }
	//
	// @Override
	// protected ObservableSnapshot<Generic> computeValue() {
	// return binding.get();
	// }
	//
	// @Override
	// public ObservableSnapshot<Generic> getValue() {
	// return (ObservableSnapshot<Generic>) super.getValue();
	// }
	//
	// @Override
	// public Generic get(int index) {
	// return getValue().get(index);
	// }
	//
	// @Override
	// public int size() {
	// return getValue().size();
	// }
	// }
	// @Override
	// public CompletableFuture<Snapshot<Generic>> getDependenciesPromise(
	// Generic generic) {
	// return getTransaction().getDependenciesPromise(generic);
	// }
	// }

	// private Map<Generic, PromiseListBinding>
	// dependenciesPromiseAsOservableListCacheMap = new HashMap<Generic,
	// PromiseListBinding>();
	//
	// public CompletableFuture<ObservableList<Generic>>
	// getObervableDependenciesPromise(Generic generic) {
	// ObservableValue<CompletableFuture<Snapshot<Generic>>> dependenciesPromise
	// = getDifferential().getObervableDependenciesPromise(generic);
	// return dependenciesPromise.getValue().thenApply(snapshot -> {
	// PromiseListBinding observableList =
	// dependenciesPromiseAsOservableListCacheMap.get(generic);
	// if (observableList == null)
	// dependenciesPromiseAsOservableListCacheMap.put(generic, observableList =
	// new PromiseListBinding(generic, dependenciesPromise,
	// differentialProperty));
	// observableList.push(snapshot.toList());
	// return observableList;
	// });
	// }

	// private class PromiseListBinding extends ListBinding<Generic> {
	// private final
	// ObjectBinding<ObservableValue<CompletableFuture<Snapshot<Generic>>>>
	// binding;
	// private Collection<? extends Generic> elements = new ArrayList<>();
	// @SuppressWarnings("unused")
	// private
	// ChangeListener<ObservableValue<CompletableFuture<Snapshot<Generic>>>>
	// bindingListener;
	//
	// PromiseListBinding(Generic generic,
	// ObservableValue<CompletableFuture<Snapshot<Generic>>>
	// dependenciesPromise, ObjectProperty<Differential> differentialProperty) {
	//
	// this.binding = new
	// GSObjectBinding<ObservableValue<CompletableFuture<Snapshot<Generic>>>>(dependenciesPromise,
	// differentialProperty, true) {
	//
	// @Override
	// protected void onInvalidating() {
	// changeBindedObject(((AsyncIDifferential)
	// differentialProperty.get()).getObervableDependenciesPromise(generic));
	// }
	//
	// };
	//
	// binding.addListener(new WeakChangeListener<>(bindingListener =
	// ((observable, oldV, newV) -> {
	// newV.getValue().thenAccept(snapshot -> {
	// elements = snapshot.toList();
	// invalidate();
	// });
	// })));
	// }
	//
	// @Override
	// protected ObservableList<Generic> computeValue() {
	// return FXCollections.observableArrayList(elements);
	// }
	//
	// public boolean push(Collection<? extends Generic> elements) {
	// this.elements = elements;
	// invalidate();
	// return true;
	// };
	//
	// }

	// public ObservableList<Generic> getDependenciesObservableList(Generic
	// generic) {
	// ObservableList<Generic> dependenciesObservableList =
	// getDifferential().getDependenciesObservableSnapshot(generic).toObservableList();
	//
	// return new ObservableSnapshotListBinding(generic,
	// dependenciesObservableList, differentialProperty);
	// }

	// private class ObservableSnapshotListBinding extends ListBinding<Generic>
	// {
	// private final ObjectBinding<ObservableList<Generic>> binding;
	// @SuppressWarnings("unused")
	// private ChangeListener<ObservableList<Generic>> bindingListener;
	//
	// ObservableSnapshotListBinding(Generic generic, ObservableList<Generic>
	// dependenciesPromise, ObjectProperty<Differential> differentialProperty) {
	//
	// this.binding = new
	// GSObjectBinding<ObservableList<Generic>>(dependenciesPromise,
	// differentialProperty, true) {
	//
	// @Override
	// protected void onInvalidating() {
	// changeBindedObject(((AsyncIDifferential)
	// differentialProperty.get()).getDependenciesObservableSnapshot(generic).toObservableList());
	// }
	//
	// };
	//
	// binding.addListener(new WeakChangeListener<>(bindingListener =
	// ((observable, oldV, newV) -> {
	// invalidate();
	// })));
	// }
	//
	// @Override
	// protected ObservableList<Generic> computeValue() {
	// return binding.get();
	// }
	// }

	// public CompletableFuture<Snapshot<Generic>> getDependenciesPromise(Generic generic) {
	// return getDifferential().getDependenciesPromise(generic);
	// }
}
//
// public ObservableList<Generic> getObservableDependencies(Generic generic)
// {
// return new ListBinding<Generic>() {
// @SuppressWarnings("unused")
// private final InvalidationListener listener;
// private final Observable invalidator = getInvalidator(generic);
// private List<Generic> promisedList = new ArrayList<Generic>();
//
// {
// invalidator.addListener(new WeakInvalidationListener(listener = (o) -> {
// CocCache.this.getDependenciesPromise(generic).thenAccept(snapshot -> {
// promisedList = snapshot.toList();
// Platform.runLater(() -> invalidate());// resolve Not FX thread
// });
// }));
// }
//
// @Override
// protected ObservableList<Generic> computeValue() {
// return
// FXCollections.unmodifiableObservableList(FXCollections.observableList(promisedList));
// }
// };
// }