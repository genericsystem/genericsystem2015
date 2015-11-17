package org.genericsystem.distributed.cacheonclient;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.ObservableList;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.CheckedContext;
import org.genericsystem.common.Container;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Vertex;
import org.genericsystem.distributed.cacheonclient.CocClientEngine.ClientEngineHandler;

@SuppressWarnings("restriction")
public class CocTransaction extends CheckedContext implements AsyncIDifferential {

	private final long ts;

	protected CocTransaction(CocClientEngine engine, long ts) {
		super(engine);
		this.ts = ts;
	}

	protected CocTransaction(CocClientEngine engine) {
		this(engine, engine.pickNewTs());
	}

	@Override
	public long getTs() {
		return ts;
	}

	// @Override
	// protected Checker<Generic> buildChecker() {
	// return new Checker<Generic>(Transaction.this) {
	// @Override
	// public void checkAfterBuild(boolean isOnAdd, boolean isFlushTime, Generic
	// vertex) throws RollbackException {
	// checkSystemConstraintsAfterBuild(isOnAdd, isFlushTime, vertex);// Check
	// only system constraints on transactions
	// }
	// };
	// }

	@Override
	public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		assert adds.stream().allMatch(add -> add.getBirthTs() == Long.MAX_VALUE);
		getRoot().getServer().apply(getTs(), removes.stream().mapToLong(g -> g.getTs()).toArray(), adds.stream().map(g -> g.getVertex()).toArray(Vertex[]::new));
		removes.forEach(this::invalid);// Not efficient ! plug and unplug is better
		adds.forEach(this::invalid);// Not efficient !
		adds.forEach(this::giveBirth);
	}

	private void giveBirth(Generic generic) {
		assert Long.MAX_VALUE == generic.getBirthTs();
		((ClientEngineHandler) generic.getProxyHandler()).birthTs = getTs();
	}

	private void invalid(Generic generic) {
		generic.getComponents().forEach(component -> dependenciesMap.remove(component));
		generic.getSupers().forEach(superG -> dependenciesMap.remove(superG));
		dependenciesMap.remove(generic.getMeta());
		dependenciesMap.remove(generic);
	}

	@Override
	public CocClientEngine getRoot() {
		return (CocClientEngine) super.getRoot();
	}

	private Map<Generic, Snapshot<Generic>> dependenciesMap = new HashMap<>();

	@Override
	public Snapshot<Generic> getDependencies(Generic generic) {
		Snapshot<Generic> dependencies = dependenciesMap.get(generic);
		if (dependencies == null) {
			dependencies = new Container(Arrays.stream(getRoot().getServer().getDependencies(getTs(), generic.getTs())).map(vertex -> getRoot().getGenericByVertex(vertex)));
			Snapshot<Generic> result = dependenciesMap.put(generic, dependencies);
			assert result == null;
		}
		return dependencies;
	}

	public class CompletableObservableList extends SimpleObjectProperty<List<Generic>> implements ObservableValue<List<Generic>> {

		public CompletableObservableList(CompletableFuture<Object> promise) {
			super(new ArrayList<>());
			promise.thenAccept(elem -> {
				Vertex[] elements = (Vertex[]) elem;
				setValue(Arrays.stream(elements).map(vertex -> getRoot().getGenericByVertex(vertex)).collect(Collectors.toList()));
			});
		}
	}

	// public class CompletableObservableList2 extends ObservableListWrapper<Generic> {
	//
	// public CompletableObservableList2(CompletableFuture<Vertex[]> promise) {
	// super(new ArrayList<>());
	// promise.thenApply(elements -> setAll(Arrays.stream(elements).map(vertex -> getRoot().getGenericByVertex(vertex)).collect(Collectors.toList())));
	// }
	// }

	private Map<Generic, ObservableValue<List<Generic>>> dependenciesPromisesMap = new HashMap<>();

	@Override
	public ObservableValue<List<Generic>> getDependenciesObservableList(Generic generic) {
		ObservableValue<List<Generic>> dependencies = dependenciesPromisesMap.get(generic);
		if (dependencies == null) {
			dependencies = new CompletableObservableList(getRoot().getServer().getDependenciesPromise(getTs(), generic.getTs()));
			ObservableValue<List<Generic>> result = dependenciesPromisesMap.put(generic, dependencies);
			assert result == null;
		}
		return dependencies;
	}

	@Override
	public ObservableList<Generic> getWrappableDependencies(Generic generic) {
		return new AbstractWrappable<Generic>() {
			private ChangeListener<List<Generic>> listener = new WeakChangeListener<>((observableValue, oldValue, newValue) -> {
				beginChange();
				nextAdd(0, newValue.size());
				endChange();
			});
			{
				getDependenciesObservableList(generic).addListener(listener);
			}

			@Override
			public Generic get(int index) {
				return getDependenciesObservableList(generic).getValue().get(index);
			}

			@Override
			public int size() {
				return getDependenciesObservableList(generic).getValue().size();
			}
		};
	}
}
