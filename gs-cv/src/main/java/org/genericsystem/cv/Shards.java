package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;

import org.opencv.core.Scalar;

public class Shards implements Iterable<Shard> {

	private final List<Shard> shards;

	public Shards() {
		shards = new ArrayList<Shard>();
	}

	public Shards(List<Shard> shardsList) {
		this.shards = shardsList;
	}

	public boolean isEmpty() {
		return shards.isEmpty();
	}

	public Shards removeIf(Predicate<Shard> filter) {
		shards.removeIf(filter);
		return this;
	}

	public int size() {
		return shards.size();
	}

	@Override
	public Iterator<Shard> iterator() {
		return shards.iterator();
	}

	public void draw(Img img, Scalar scalar, int thickness) {
		shards.forEach(adjusted -> adjusted.draw(img, scalar, thickness));
	}

	@Override
	public String toString() {
		return shards.toString();
	}
}
