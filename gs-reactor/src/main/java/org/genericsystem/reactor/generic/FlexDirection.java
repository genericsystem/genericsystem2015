package org.genericsystem.reactor.generic;

public enum FlexDirection {
	COLUMN, ROW;

	public FlexDirection reverse() {
		switch (this) {
		case COLUMN:
			return FlexDirection.ROW;
		case ROW:
			return FlexDirection.COLUMN;
		default:
			throw new IllegalStateException();
		}
	}

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
