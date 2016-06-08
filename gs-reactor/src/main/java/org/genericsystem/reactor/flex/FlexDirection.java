package org.genericsystem.reactor.flex;

public enum FlexDirection {
	COLUMN, ROW;

	@Override
	public String toString() {
		switch (this) {
		case COLUMN:
			return "column";
		case ROW:
			return "row";
		default:
			throw new IllegalStateException();
		}
	}
}
