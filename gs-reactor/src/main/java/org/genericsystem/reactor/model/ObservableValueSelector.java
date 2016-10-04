package org.genericsystem.reactor.model;

import java.util.function.Function;
import java.util.function.Supplier;

import org.genericsystem.common.Generic;

public interface ObservableValueSelector extends Function<Generic[], Generic> {

	public static class STRICT_ATTRIBUTE_SELECTOR implements Supplier<ObservableValueSelector> {
		@Override
		public ObservableValueSelector get() {
			return gs -> gs[0].getComponents().size() < 2 ? gs[0] : null;
		}
	}

	public static class RELATION_SELECTOR implements Supplier<ObservableValueSelector> {
		@Override
		public ObservableValueSelector get() {
			return gs -> gs[0].getComponents().size() >= 2 ? gs[0] : null;
		}
	}

	public static class CHECK_BOX_DISPLAYER implements Supplier<ObservableValueSelector> {
		@Override
		public ObservableValueSelector get() {
			return gs -> gs[1].getComponents().size() == 1 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null;
		}
	}

	public static class CHECK_BOX_DISPLAYER_0 implements Supplier<ObservableValueSelector> {
		@Override
		public ObservableValueSelector get() {
			return gs -> gs[0].getComponents().size() == 1 && Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null;
		}
	}

	public static class LABEL_DISPLAYER implements Supplier<ObservableValueSelector> {
		@Override
		public ObservableValueSelector get() {
			return gs -> gs[1].getComponents().size() == 1 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null;
		}
	}

	public static class LABEL_DISPLAYER_0 implements Supplier<ObservableValueSelector> {
		@Override
		public ObservableValueSelector get() {
			return gs -> gs[0].getComponents().size() == 1 && !Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null;
		}
	}

	public static class DIRECT_RELATION_SELECTOR implements Supplier<ObservableValueSelector> {
		@Override
		public ObservableValueSelector get() {
			return gs -> gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null;
		}
	}

	public static class REVERSED_RELATION_SELECTOR implements Supplier<ObservableValueSelector> {
		@Override
		public ObservableValueSelector get() {
			return gs -> !gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) && !gs[0].getLinks(gs[2]).isEmpty() ? gs[0] : null;
		}
	}
}