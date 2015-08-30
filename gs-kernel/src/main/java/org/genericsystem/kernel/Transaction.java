package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.AbstractContext;
import org.genericsystem.common.IDependencies;
import org.genericsystem.common.IDifferential;
import org.genericsystem.common.Vertex;

public class Transaction extends AbstractContext<Generic> implements IDifferential<Generic> {

	private final long ts;

	public Transaction(Root root, long ts) {
		super(root);
		this.ts = ts;
	}

	public Transaction(Root root) {
		this(root, root.pickNewTs());
	}

	@Override
	public Root getRoot() {
		return (Root) super.getRoot();
	}

	@Override
	public long getTs() {
		return ts;
	}

	@Override
	protected Generic plug(Generic generic) {
		if (getRoot().isInitialized()) {
			generic.getProxyHandler().otherTs[0] = getTs();
			generic.getLifeManager().beginLife(getTs());
		}

		Set<Generic> set = new HashSet<>();
		if (!generic.isMeta())
			set.add(generic.getMeta());
		set.addAll(generic.getSupers());
		set.addAll(generic.getComponents());
		set.stream().forEach(ancestor -> ((IDependencies<Generic>) getDependencies(ancestor)).add(generic));
		getChecker().checkAfterBuild(true, false, generic);
		return generic;
	}

	private void kill(Generic generic) {
		getChecker().checkAfterBuild(false, false, generic);
		generic.getLifeManager().kill(getTs());
		getRoot().getGarbageCollector().add(generic);
	}

	@Override
	protected void unplug(Generic generic) {
		getChecker().checkAfterBuild(false, false, generic);
		Set<Generic> set = new HashSet<>();
		if (!generic.isMeta())
			set.add(generic.getMeta());
		set.addAll(generic.getSupers());
		set.addAll(generic.getComponents());
		set.stream().forEach(ancestor -> ((IDependencies<Generic>) getDependencies(ancestor)).remove(generic));
		generic.getLifeManager().kill(getTs());
		getRoot().getGarbageCollector().add(generic);
	}

	@Override
	public Snapshot<Generic> getDependencies(Generic ancestor) {
		assert ancestor != null;
		return new IDependencies<Generic>() {

			@Override
			public Stream<Generic> stream() {
				return ancestor.getProxyHandler().getDependencies().stream(getTs());
			}

			@Override
			public Generic get(Object o) {
				return ancestor.getProxyHandler().getDependencies().get((Generic) o, getTs());
			}

			@Override
			public void add(Generic add) {
				ancestor.getProxyHandler().getDependencies().add(add);
			}

			@Override
			public boolean remove(Generic remove) {
				return ancestor.getProxyHandler().getDependencies().remove(remove);
			}
		};
	}

	@Override
	public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		new LockedLifeManager().apply(removes, adds);
	}

	// archiver acces
	protected Generic buildAndPlug(Long ts, Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long[] otherTs) {
		return plug(build(ts, clazz, meta, supers, value, components, otherTs));

	}

	@Override
	protected Generic setMeta(int dim) {
		return super.setMeta(dim);
	}

	private class LockedLifeManager {

		private Set<LifeManager> lockedLifeManagers = new HashSet<>();

		private void apply(Iterable<Generic> removes, Iterable<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
			try {
				writeLockAllAndCheckMvcc(adds, removes);
				for (Generic remove : removes)
					kill(remove);
				for (Generic add : adds) {
					plug(add);
				}

			} finally {
				writeUnlockAll();
			}
		}

		private void writeLockAllAndCheckMvcc(Iterable<Generic> adds, Iterable<Generic> removes) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
			for (Generic remove : removes)
				writeLockAndCheckMvcc(remove);
			for (Generic add : adds)
				writeLockAndCheckMvccForAdd(add);
		}

		private void writeLockAndCheckMvccForAdd(Generic add) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
			writeLockAndCheckMvcc(add.getMeta());
			for (Generic superT : add.getSupers())
				writeLockAndCheckMvcc(superT);
			for (Generic component : add.getComponents())
				writeLockAndCheckMvcc(component);
			writeLockAndCheckMvcc(add);
		}

		private void writeLockAndCheckMvcc(Generic generic) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
			if (generic != null) {
				LifeManager manager = generic.getLifeManager();
				if (!lockedLifeManagers.contains(manager)) {
					manager.writeLock();
					lockedLifeManagers.add(manager);
					manager.checkMvcc(getTs());
				}
			}
		}

		private void writeUnlockAll() {
			for (LifeManager lifeManager : lockedLifeManagers)
				lifeManager.writeUnlock();
			lockedLifeManagers = new HashSet<>();
		}
	}

	public Snapshot<Long> getRemoteDependencies(long ts) {
		org.genericsystem.kernel.Generic serverGeneric = getRoot().getGenericById(ts);
		// What to do if serverGeneric not alive ???
		if (serverGeneric != null)
			return () -> getDependencies(serverGeneric).stream().map(serverDependency -> serverDependency.getTs());
		else
			return () -> Stream.empty();
	}

	public void remoteApply(long[] removeIds, Vertex[] addVertices) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		// Arrays.stream(removeIds).mapToObj(removeId ->
		// getRoot().getGenericById(removeId)).allMatch(g -> true);
		// assert Arrays.stream(addVertices).map(add ->
		// add.getTs()).distinct().count() == addVertices.length;
		assert Arrays.stream(addVertices).allMatch(addVertex -> getRoot().getGenericById(addVertex.getTs()) == null);
		Arrays.stream(addVertices).forEach(addVertex -> getRoot().build(addVertex));
		apply(() -> Arrays.stream(removeIds).mapToObj(removeId -> getRoot().getGenericById(removeId)), () -> Arrays.stream(addVertices).map(addVertex -> getRoot().getGenericById(addVertex.getTs())));
	}
}
