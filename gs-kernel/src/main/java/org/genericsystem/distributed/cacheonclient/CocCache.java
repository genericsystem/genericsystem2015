package org.genericsystem.distributed.cacheonclient;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache;
import org.genericsystem.common.IDifferential;
import org.genericsystem.distributed.cacheonclient.observables.ObservableSnapshot;

public class CocCache extends HeavyCache {

	private Map<Generic, ObservableList<Generic>> dependenciesAsOservableListCacheMap = new HashMap<Generic, ObservableList<Generic>>() {

		private static final long serialVersionUID = -6268341397267444297L;

		@Override
		public ObservableList<Generic> get(Object key) {
			ObservableList<Generic> result = super.get(key);
			if (result == null) {
				ObservableValue<List<Generic>> dependenciesAsObservableValue = getDifferential().getDependenciesObservableList((Generic) key);
				result = new ListBinding<Generic>() {
					{
						bind(dependenciesAsObservableValue);
					}

					@Override
					public void dispose() {
						unbind(dependenciesAsObservableValue);
					}

					@Override
					protected ObservableList<Generic> computeValue() {
						return FXCollections.observableArrayList(dependenciesAsObservableValue.getValue());
					}
				};
				ObservableList<Generic> assertResult = super.put((Generic) key, result);
				assert assertResult == null;
			}
			return result;

		}
	};

	protected CocCache(AbstractRoot root) {
		super(root);
	}

	protected CocCache(AbstractRoot root, ContextEventListener<Generic> listener) {
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
		public ObservableValue<List<Generic>> getDependenciesObservableList(Generic generic) {
			return getTransaction().getDependenciesObservableList(generic);
		}

		@Override
		public ObservableList<Generic> getWrappableDependencies(Generic generic) {
			return getTransaction().getWrappableDependencies(generic);
		}

		@Override
		public ObservableSnapshot<Generic> getDependenciesObservableSnapshot(Generic generic) {
			return getTransaction().getDependenciesObservableSnapshot(generic);
		}
	}

	@Override
	protected CocTransaction buildTransaction() {
		return new CocTransaction((CocClientEngine) (getRoot()), getRoot().pickNewTs());
	}

	@Override
	public CocTransaction getTransaction() {
		return (CocTransaction) super.getTransaction();
	}

	@Override
	protected AsyncDifferential getDifferential() {
		return (AsyncDifferential) super.getDifferential();
	}

	public ObservableList<Generic> getDependenciesObservableList(Generic generic) {
		return dependenciesAsOservableListCacheMap.get(generic);
	}

	public ObservableList<Generic> getWrappableDependenciesSnap(Generic generic) {
		return getDifferential().getDependenciesObservableSnapshot(generic).toObservableList();
	}

	public ObservableList<Generic> getWrappableDependencies(Generic generic) {
		return getDifferential().getWrappableDependencies(generic);
	}

	public ObservableList<Generic> getInstancesObservableList(Generic meta) {
		return getDependenciesObservableList(meta).filtered(generic -> meta.equals(generic.getMeta()));
	}
}