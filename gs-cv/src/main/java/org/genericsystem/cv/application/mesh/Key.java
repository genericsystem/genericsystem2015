package org.genericsystem.cv.application.mesh;

public class Key { // tableau multidimensionnel

	public int i, j; // i = numero de ligne, j = numero de colonne

	public Key(int i, int j) {
		this.i = i;
		this.j = j;
	}

	@Override
	public int hashCode() {
		return 31 * i + 7 * j;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Key))
			return false;
		Key other = (Key) obj;
		return i == other.i && j == other.j;
	}
}