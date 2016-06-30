package org.genericsystem.reactor.flex;

public enum FlexDirection {
	COLUMN, ROW;

	public static FlexDirection reverse(FlexDirection directionToReverse) {
		switch (directionToReverse) {
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
