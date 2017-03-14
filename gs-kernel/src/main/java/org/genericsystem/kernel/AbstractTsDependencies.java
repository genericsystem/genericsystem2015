package org.genericsystem.kernel;

import java.util.Spliterators;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.genericsystem.api.core.Filters;
import org.genericsystem.api.core.Filters.IndexFilter;
import org.genericsystem.common.AbstractIterator;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.AbstractServer.RootServerHandler;

/**
 * @author Nicolas Feybesse
 *
 */
abstract class AbstractTsDependencies {
	private static interface Index {
		public void add(Generic generic);

		public boolean remove(Generic generic);

		public Stream<Generic> stream(long ts);

	}

	private class IndexImpl implements Index {
		private Node head = null;
		private Node tail = null;
		private final long ts;
		private final IndexFilter<Generic> filter;

		IndexImpl(IndexFilter<Generic> filter, long ts) {
			this.ts = ts;
			this.filter = filter;
			if (!Filters.NO_FILTER.equals(filter))
				indexs.get(Filters.NO_FILTER, ts).stream(ts).forEach(generic -> {
					if (filter.test(generic))
						add(generic);
				});
		}

		@Override
		public void add(Generic generic) {
			assert generic != null;
			// assert getLifeManager().isWriteLockedByCurrentThread();
			Node newNode = new Node(generic);
			if (head == null)
				head = newNode;
			else
				tail.next = newNode;
			tail = newNode;
			Generic result = map.put(generic, generic);
			// assert result == null;
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
			if (ts < this.ts)
				indexs.updateIndex(filter, ts);
			return StreamSupport.stream(Spliterators.spliteratorUnknownSize(new InternalIterator(ts), 0), false);
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

	public Stream<Generic> stream(long ts, IndexFilter<Generic> filter) {
		return indexs.get(filter, ts).stream(ts);

	}

	public Stream<Generic> stream(long ts) {
		return indexs.get(Filters.NO_FILTER, ts).stream(ts);
	}

	private final ExtendedMap indexs = new ExtendedMap() {

		private static final long serialVersionUID = 1067462646727512744L;

		{
			put((IndexFilter<Generic>) Filters.NO_FILTER, new IndexImpl((IndexFilter<Generic>) Filters.NO_FILTER, 0));
		}

	};

	private class ExtendedMap extends ConcurrentHashMap<IndexFilter<Generic>, Index> {

		private static final long serialVersionUID = 2969395358619269789L;

		public Index get(Object key, long ts) {
			return super.computeIfAbsent((IndexFilter<Generic>) key, k -> new IndexImpl(k, ts));
		};

		public Index updateIndex(IndexFilter<Generic> key, long ts) {
			return super.put(key, new IndexImpl(key, ts));
		}
	}

	public void add(Generic generic) {
		indexs.entrySet().forEach(entry -> {
			if (entry.getKey().test(generic)) {
				entry.getValue().add(generic);
			}
		});
	}

	public boolean remove(Generic generic) {
		boolean[] result = new boolean[] { false };
		indexs.entrySet().forEach(entry -> {
			if (entry.getKey().test(generic))
				result[0] = result[0] | entry.getValue().remove(generic);
		});
		return result[0];
	}

	private static class Node {
		Generic content;
		Node next;

		private Node(Generic content) {
			this.content = content;
		}
	}
}
