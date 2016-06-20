package org.genericsystem.reactor.flex;

public enum FlexTag {
	HEADER, SECTION, FOOTER, ASIDE;

	@Override
	public String toString() {
		switch (this) {
		case HEADER:
			return "header";
		case SECTION:
			return "section";
		case FOOTER:
			return "footer";
		case ASIDE:
			return "aside";
		default:
			throw new IllegalStateException();
		}
	}
}