package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.genericsystem.api.core.IndexFilter;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.common.CheckedContext;
import org.genericsystem.common.Generic;
import org.genericsystem.common.IDependencies;
import org.genericsystem.common.IDifferential;
import org.genericsystem.kernel.AbstractServer.RootServerHandler;

import io.reactivex.Observable;

/**
 * @author Nicolas Feybesse
 *
 */
public class Transaction extends CheckedContext implements IDifferential<Generic> {

	private final long ts;

	public Transaction(AbstractServer root, long ts) {
		super(root);
		this.ts = ts;
	}

	public Transaction(AbstractServer root) {
		this(root, root.pickNewTs());
	}

	@Override
	public AbstractServer getRoot() {
		return (AbstractServer) super.getRoot();
	}

	@Override
	public long getTs() {
		return ts;
	}

	@Override
	public Observable<IDifferential<Generic>> getDifferentialObservable() {
		throw new UnsupportedOperationException();
	}

	protected Generic plug(Generic generic) {
		if (getRoot().isInitialized())
			((RootServerHandler) generic.getProxyHandler()).getLifeManager().beginLife(getTs());
		Set<Generic> set = new HashSet<>();
		if (!generic.isMeta())
			set.add(generic.getMeta());
		set.addAll(generic.getSupers());
		set.addAll(generic.getComponents());
		set.stream().forEach(ancestor -> getDependencies(ancestor).add(generic));
		set.stream().forEach(ancestor -> ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().signalAdd(generic));
		// getChecker().checkAfterBuild(true, false, generic);
		return generic;
	}

	private void kill(Generic generic) {
		// getChecker().checkAfterBuild(false, false, generic);
		((RootServerHandler) generic.getProxyHandler()).getLifeManager().kill(getTs());
		getRoot().getGarbageCollector().add(generic);
	}

	private void signalRemoval(Generic generic) {
		// getChecker().checkAfterBuild(false, false, generic);
		Set<Generic> set = new HashSet<>();
		if (!generic.isMeta())
			set.add(generic.getMeta());
		set.addAll(generic.getSupers());
		set.addAll(generic.getComponents());
		set.stream().forEach(ancestor -> ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().signalRemoval(generic));
	}

	@Override
	public IDependencies<Generic> getDependencies(Generic ancestor) {
		assert ancestor != null;
		return new IDependencies<Generic>() {

			@Override
			public Stream<Generic> unfilteredStream() {
				return ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().stream(getTs());
			}

			@Override
			public Observable<Generic> getAdds() {
				return ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().getAdds(getTs());
			}

			@Override
			public Observable<Generic> getRemovals() {
				return ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().getRemovals(getTs());
			}

			@Override
			public Snapshot<Generic> filter(List<IndexFilter> filters) {
				return ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().filter(filters, getTs());
			}

			@Override
			public Generic get(Object o) {
				return ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().get((Generic) o, getTs());
			}

			@Override
			public void add(Generic add) {
				((RootServerHandler) ancestor.getProxyHandler()).getDependencies().add(add);
			}

			@Override
			public boolean remove(Generic remove) {
				return ((RootServerHandler) ancestor.getProxyHandler()).getDependencies().remove(remove);
			}
		};
	}

	@Override
	public void apply(Snapshot<Generic> removes, Snapshot<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
		new LockedLifeManager().apply(removes, adds);
	}

	// archiver acces
	protected Generic buildAndPlug(Long ts, Class<?> clazz, Generic meta, List<Generic> supers, Serializable value, List<Generic> components, long[] otherTs) {
		return plug(getRoot().build(ts, clazz, meta, supers, value, components, otherTs));
	}

	private class LockedLifeManager {

		private Set<LifeManager> lockedLifeManagers = new HashSet<>();

		private void apply(Iterable<Generic> removes, Iterable<Generic> adds) throws ConcurrencyControlException, OptimisticLockConstraintViolationException {
			try {
				writeLockAllAndCheckMvcc(adds, removes);
				for (Generic remove : removes) {
					signalRemoval(remove);
					kill(remove);
				}
				for (Generic add : adds)
					plug(add);
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
				LifeManager manager = ((RootServerHandler) generic.getProxyHandler()).getLifeManager();
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

	@Override
	public Observable<Generic> getAdds(Generic generic) {
		return getDependencies(generic).getAdds();
	}

	@Override
	public Observable<Generic> getRemovals(Generic generic) {
		return getDependencies(generic).getRemovals();
	}
}
