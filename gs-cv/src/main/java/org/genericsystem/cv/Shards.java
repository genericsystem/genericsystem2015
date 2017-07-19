package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;

import org.opencv.core.Scalar;

public class Shards implements Iterable<Layout> {

	private final List<Layout> shards;

	public Shards() {
		shards = new ArrayList<Layout>();
	}

	public Shards(List<Layout> shardsList) {
		this.shards = shardsList;
	}

	public boolean isEmpty() {
		return shards.isEmpty();
	}

	public Shards removeIf(Predicate<Layout> filter) {
		shards.removeIf(filter);
		return this;
	}

	public int size() {
		return shards.size();
	}

	@Override
	public Iterator<Layout> iterator() {
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
