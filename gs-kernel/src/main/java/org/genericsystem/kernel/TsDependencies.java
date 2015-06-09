package org.genericsystem.kernel;

import java.util.stream.Stream;

public interface TsDependencies<T> {

	Stream<T> stream(long ts);

	T get(T generic, long ts);

	void add(T add);

	public boolean remove(T remove);

	// public Iterator<T> iterator(long ts);

	// public abstract static class Dependencies implements IDependencies<Long> {
	//
	// private Long head = null;
	// private Long tail = null;
	//
	// private final ConcurrentHashMap<Long, Long> map = new ConcurrentHashMap<>();
	//
	// public abstract Long getAncestor();
	//
	// public abstract Root getRoot();
	//
	// public abstract LifeManager getLifeManager();
	//
	// @Override
	// public Stream<Long> stream(long ts) {
	// return StreamSupport.stream(Spliterators.spliteratorUnknownSize(new InternalIterator(ts), 0), false);
	// }
	//
	// @Override
	// public Long get(Long generic, long ts) {
	// Long result = map.get(generic);// this no lock read requires a concurrent hash map
	// if (result == null) {
	// LifeManager lifeManager = getLifeManager();
	// lifeManager.readLock();
	// try {
	// result = map.get(generic);
	// lifeManager.atomicAdjustLastReadTs(ts);
	// } finally {
	// lifeManager.readUnlock();
	// }
	// }
	// return result != null && getRoot().getGenericFromTs(result).getLifeManager().isAlive(ts) ? result : null;
	// }
	//
	// @Override
	// public void add(Long add) {
	// assert add != null;
	// // TODO active this
	// // assert !add.getRoot().isInitialized() || getLifeManager().isWriteLockedByCurrentThread();
	// if (head == null)
	// head = add;
	// else
	// getRoot().setNextDependency(tail, getAncestor(), add);
	// tail = add;
	// Long result = map.put(add, add);
	// assert result == null;
	// }
	//
	// @Override
	// public boolean remove(Long generic) {
	// assert generic != null : "generic is null";
	// assert head != null : "head is null";
	//
	// Long currentNode = head;
	//
	// Long currentContent = currentNode;
	// if (generic.equals(currentContent)) {
	// Long next = currentNode.getNextDependency(getAncestor());
	// head = next != null ? next : null;
	// return true;
	// }
	//
	// Long nextNode = currentNode.getNextDependency(getAncestor());
	// while (nextNode != null) {
	// Long nextLong = nextNode;
	// Long nextNextNode = nextNode.getNextDependency(getAncestor());
	// if (generic.equals(nextLong)) {
	// if (nextNextNode == null)
	// tail = currentNode;
	// currentNode.getRoot().setNextDependency(currentNode, getAncestor(), nextNextNode);
	// map.remove(generic);
	// return true;
	// }
	// currentNode = nextNode;
	// nextNode = nextNextNode;
	// }
	// return false;
	// }
	//
	// private class InternalIterator extends AbstractAwareIterator<Long> {
	//
	// private final long ts;
	//
	// private InternalIterator(long iterationTs) {
	// ts = iterationTs;
	// }
	//
	// @Override
	// protected void advance() {
	// for (;;) {
	// Long nextDependency = (next == null) ? head : next.getNextDependency(getAncestor());
	// if (nextDependency == null) {
	// LifeManager lifeManager = getLifeManager();
	// lifeManager.readLock();
	// try {
	// nextDependency = (next == null) ? head : next.getNextDependency(getAncestor());
	// if (nextDependency == null) {
	// next = null;
	// lifeManager.atomicAdjustLastReadTs(ts);
	// return;
	// }
	// } finally {
	// lifeManager.readUnlock();
	// }
	// }
	// next = nextDependency;
	// if (getRoot().getGenericFromTs(next).getLifeManager().isAlive(ts))
	// break;
	// }
	// }
	// }
	// }
}
