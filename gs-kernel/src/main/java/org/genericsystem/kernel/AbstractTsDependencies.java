package org.genericsystem.kernel;

import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.List;
import java.util.Spliterators;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.genericsystem.api.core.FiltersBuilder;
import org.genericsystem.api.core.IndexFilter;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.AbstractIterator;
import org.genericsystem.common.Generic;
import org.genericsystem.common.SoftValueHashMap;
import org.genericsystem.kernel.AbstractServer.RootServerHandler;

/**
 * @author Nicolas Feybesse
 *
 */
abstract class AbstractTsDependencies {
	private static interface Index {
		public boolean add(Generic generic);

		public boolean remove(Generic generic);

		public Stream<Generic> stream(long ts);

		public IndexFilter getFilter();
	}

	private class IndexImpl implements Index {
		private Node head = null;
		private Node tail = null;
		private final IndexFilter filter;

		IndexImpl(IndexFilter filter, long ts, Index parent) {
			this.filter = filter;
			if (parent != null)
				parent.stream(ts).forEach(generic -> {
					if (filter.test(generic))
						add(generic);
				});
		}

		@Override
		public boolean add(Generic generic) {
			assert generic != null;
			// assert getLifeManager().isWriteLockedByCurrentThread();
			if (filter.test(generic)) {
				Node newNode = new Node(generic);
				if (head == null)
					head = newNode;
				else
					tail.next = newNode;
				tail = newNode;
				Generic result = map.put(generic, generic);
				return true;
			}
			return false;
		}

		@Override
		public boolean remove(Generic generic) {
			assert generic != null : "generic is null";
			assert head != null : "head is null";

			Node currentNode = head;

			Generic currentContent = currentNode.content;
			if (generic.equals(currentContent)) {
				Node next = currentNode.next;
				head = next != null ? next : null;
				return true;
			}

			Node nextNode = currentNode.next;
			while (nextNode != null) {
				Generic nextGeneric = nextNode.content;
				Node nextNextNode = nextNode.next;
				if (generic.equals(nextGeneric)) {
					nextNode.content = null;
					if (nextNextNode == null)
						tail = currentNode;
					currentNode.next = nextNextNode;
					map.remove(generic);
					return true;
				}
				currentNode = nextNode;
				nextNode = nextNextNode;
			}
			return false;
		}

		@Override
		public Stream<Generic> stream(long ts) {
			return StreamSupport.stream(Spliterators.spliteratorUnknownSize(new InternalIterator(ts), 0), false);
		}

		@Override
		public IndexFilter getFilter() {
			return filter;
		}

		private class InternalIterator extends AbstractIterator<Node, Generic> {

			private final long ts;

			private InternalIterator(long iterationTs) {
				ts = iterationTs;
			}

			@Override
			protected void advance() {
				for (;;) {
					Node nextNode = (next == null) ? head : next.next;
					if (nextNode == null) {
						LifeManager lifeManager = getLifeManager();
						lifeManager.readLock();
						try {
							nextNode = (next == null) ? head : next.next;
							if (nextNode == null) {
								next = null;
								lifeManager.atomicAdjustLastReadTs(ts);
								return;
							}
						} finally {
							lifeManager.readUnlock();
						}
					}
					next = nextNode;
					Generic content = next.content;
					if (content != null && ((RootServerHandler) content.getProxyHandler()).getLifeManager().isAlive(ts))
						break;

				}
			}

			@Override
			protected Generic project() {
				return next.content;
			}
		}

	}

	private final ConcurrentHashMap<Generic, Generic> map = new ConcurrentHashMap<>();

	public abstract LifeManager getLifeManager();

	public Generic get(Generic generic, long ts) {
		Generic result = map.get(generic);// this no lock read requires a concurrent hash map
		if (result == null) {
			LifeManager lifeManager = getLifeManager();
			lifeManager.readLock();
			try {
				result = map.get(generic);
				lifeManager.atomicAdjustLastReadTs(ts);
			} finally {
				lifeManager.readUnlock();
			}
		}

		if (result != null && ((RootServerHandler) result.getProxyHandler()).getLifeManager().isAlive(ts))
			return result;
		return null;
	}

	public Snapshot<Generic> filter(List<IndexFilter> filters, long ts) {
		return new Snapshot<Generic>() {

			@Override
			public Stream<Generic> unfilteredStream() {
				return indexesTree.getIndex(filters, ts).stream(ts);
			}
		};
	}

	public Stream<Generic> stream(long ts) {
		return indexesTree.getIndex(new ArrayList<>(), ts).stream(ts);
	}

	private class IndexNode {
		private long ts;
		private Index index;
		private final SoftReference<IndexNode> parent;

		private SoftValueTsHashMap children = new SoftValueTsHashMap();

		private class SoftValueTsHashMap extends SoftValueHashMap<IndexFilter, IndexNode> {
			public synchronized IndexNode get(Object key, long ts) {
				return computeIfAbsent((IndexFilter) key, k -> new IndexNode(new IndexImpl(k, ts, index), ts, IndexNode.this));
			}
		}

		IndexNode(Index index, long ts, IndexNode parent) {
			this.index = index;
			this.ts = ts;
			this.parent = new SoftReference<>(parent);
		}

		Index getIndex(List<IndexFilter> filters, long ts) {
			if (ts < this.ts)
				index = new IndexImpl(index.getFilter(), ts, parent.get().index);
			if (filters.isEmpty())
				return index;
			return children.get(filters.get(0), ts).getIndex(filters.subList(1, filters.size()), ts);
		}

		public void add(Generic generic) {
			if (index.add(generic))
				children.values().forEach(childNode -> childNode.add(generic));
		}

		public boolean remove(Generic generic) {
			boolean result = index.remove(generic);
			if (result)
				children.values().forEach(childNode -> childNode.remove(generic));
			return result;
		}
	}

	private final IndexNode indexesTree = new IndexNode(new IndexImpl(new IndexFilter(FiltersBuilder.NO_FILTER), 0, null), 0, null);

	public void add(Generic generic) {
		indexesTree.add(generic);
	}

	public boolean remove(Generic generic) {
		return indexesTree.remove(generic);
	}

	private static class Node {
		Generic content;
		Node next;

		private Node(Generic content) {
			this.content = content;
		}
	}
}
