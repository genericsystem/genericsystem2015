package org.genericsystem.kernel;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;

public class Transaction extends AbstractContext<Generic> {

	private final long ts;

	public Transaction(Root root, long ts) {
		super(root);
		this.ts = ts;
	}

	protected Transaction(Root root) {
		this(root, root.pickNewTs());
	}

	@Override
	public Root getRoot() {
		return (Root) super.getRoot();
	}

	@Override
	public final long getTs() {
		return ts;
	}

	@Override
	protected final Generic plug(Generic generic) {
		if (getRoot().isInitialized())
			generic.getLifeManager().beginLife(getTs());

		Set<Generic> set = new HashSet<>();
		if (!generic.isMeta())
			set.add(generic.getMeta());
		set.addAll(generic.getSupers());
		set.addAll(generic.getComponents());
		set.stream().forEach(ancestor -> getDependencies(ancestor).add(generic));
		getChecker().checkAfterBuild(true, false, generic);
		return generic;

	}

	@Override
	protected void unplug(Generic generic) {
		getChecker().checkAfterBuild(false, false, generic);
		Set<Generic> set = new HashSet<>();
		if (!generic.isMeta())
			set.add(generic.getMeta());
		set.addAll(generic.getSupers());
		set.addAll(generic.getComponents());
		set.stream().forEach(ancestor -> getDependencies(ancestor).remove(generic));
		generic.getLifeManager().kill(getTs());
		getRoot().getGarbageCollector().add(generic);
	}

	public Generic getGenericFromTs(long ts) {
		Generic generic = getRoot().getGenericFromTs(ts);
		if (generic != null)
			getChecker().checkIsAlive(generic);
		return generic;
	}

	@Override
	public IDependencies<Generic> getDependencies(Generic ancestor) {
		assert ancestor != null;
		return new IDependencies<Generic>() {

			@Override
			public Stream<Generic> stream() {
				return getRoot().getDependencies(ancestor).stream(getTs());
			}

			@Override
			public Generic get(Object o) {
				return getRoot().getDependencies(ancestor).get((Generic) o, getTs());
			}

			@Override
			public void add(Generic add) {
				getRoot().getDependencies(ancestor).add(add);
			}

			@Override
			public boolean remove(Generic remove) {
				return getRoot().getDependencies(ancestor).remove(remove);
			}
		};
	}

	public void applyFromExternal(Snapshot<Long> removeIds, Snapshot<Vertex> addVertices) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		List<Generic> removes = removeIds.stream().map(removeId -> getRoot().getGenericFromTs(removeId)).collect(Collectors.toList());
		List<Generic> adds = addVertices.stream().map(addVertex -> getRoot().init((Class) null, addVertex.getTs(), addVertex.getMeta(), addVertex.getSupers(), addVertex.getValue(), addVertex.getComponents(), addVertex.getLifeManager()))
				.collect(Collectors.toList());
		new LockedLifeManager().apply(removes, adds);
	}

	private class LockedLifeManager {

		private Set<LifeManager> lockedLifeManagers = new HashSet<>();

		private void apply(Iterable<Generic> removes, Iterable<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
			try {
				writeLockAllAndCheckMvcc(adds, removes);
				for (Generic generic : removes)
					unplug(generic);
				for (Generic generic : adds)
					plug(generic);
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
}
