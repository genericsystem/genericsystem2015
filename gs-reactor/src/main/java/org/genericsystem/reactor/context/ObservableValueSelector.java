package org.genericsystem.reactor.context;

import java.util.function.Function;

import org.genericsystem.common.Generic;

public interface ObservableValueSelector extends Function<Generic[], Generic> {

	public static class STRICT_ATTRIBUTE_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return gs[0].getComponents().size() < 2 ? gs[0] : null;
		}
	}

	public static class RELATION_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return gs[0].getComponents().size() >= 2 ? gs[0] : null;
		}
	}

	public static class STRICT_ATTRIBUTE_SELECTOR_OR_CHECK_BOX_DISPLAYER implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return gs[0];
			// return gs[1].getComponents().size() < 2 || Boolean.class.equals(gs[1].getInstanceValueClassConstraint())
			// ? gs[0] : null;
		}
	}

	public static class STRICT_ATTRIBUTE_SELECTOR_OR_CHECK_BOX_DISPLAYER_ATTRIBUTE implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return gs[0];
			// return gs[0].getComponents().size() < 2 || Boolean.class.equals(gs[0].getInstanceValueClassConstraint())
			// ? gs[0] : null;
		}
	}

	public static class CHECK_BOX_DISPLAYER implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return Boolean.class.equals(gs[1].getInstanceValueClassConstraint()) ? gs[0] : null;
		}
	}

	public static class CHECK_BOX_DISPLAYER_ATTRIBUTE implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return Boolean.class.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null;
		}
	}

	public static class LABEL_DISPLAYER implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return /* gs[1].getComponents().size() < 2 && */!Boolean.class
					.equals(gs[1].getInstanceValueClassConstraint()) ? gs[0] : null;
		}
	}

	public static class LABEL_DISPLAYER_ATTRIBUTE implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return /* gs[0].getComponents().size() < 2 && */!Boolean.class
					.equals(gs[0].getInstanceValueClassConstraint()) ? gs[0] : null;
		}
	}

	public static class LABEL_DISPLAYER_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return (!Boolean.class.equals(gs[0].getInstanceValueClassConstraint())
					&& gs[0].getInstanceValueGenerator() == null) ? gs[0] : null;
		}
	}

	public static class PASSWORD_INSTANCE_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return gs[1].isHidden() ? gs[0] : null;
		}
	}

	public static class NON_PASSWORD_INSTANCE_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return !gs[1].isHidden() ? gs[0] : null;
		}
	}

	public static class PASSWORD_ATTRIBUTE_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return gs[0].isHidden() ? gs[0] : null;
		}
	}

	public static class NON_PASSWORD_ATTRIBUTE_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return !gs[0].isHidden() ? gs[0] : null;
		}
	}

	public static class DIRECT_RELATION_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0])) ? gs[0] : null;
		}
	}

	public static class REVERSED_RELATION_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return !gs[1].isReferentialIntegrityEnabled(gs[1].getComponents().indexOf(gs[0]))
					&& !gs[0].getLinks(gs[2]).isEmpty() ? gs[0] : null;
		}
	}

	public static class MULTICHECKBOX_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			Generic component = gs[0].getComponents().stream().filter(c -> gs[2].inheritsFrom(c)).findFirst().get();
			return !gs[0].isHidden() && gs[0].getComponents().size() == 2
					&& !gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(component)) ? gs[0] : null;
		}
	}

	public static class NON_MULTICHECKBOX_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			Generic component = gs[0].getComponents().stream().filter(c -> gs[2].inheritsFrom(c)).findFirst().get();
			return !gs[0].isHidden() && (gs[0].getComponents().size() != 2
					|| gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(component))) ? gs[0] : null;
		}
	}

	public static class MULTICHECKBOX_SELECTOR_RELATION implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			Generic component = gs[0].getComponents().stream().filter(c -> gs[1].inheritsFrom(c)).findFirst().get();
			return !gs[0].isHidden() && gs[0].getComponents().size() == 2
					&& !gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(component)) ? gs[0] : null;
		}
	}

	public static class NON_MULTICHECKBOX_SELECTOR_RELATION implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			Generic component = gs[0].getComponents().stream().filter(c -> gs[1].inheritsFrom(c)).findFirst().get();
			return !gs[0].isHidden() && (gs[0].getComponents().size() != 2
					|| gs[0].isSingularConstraintEnabled(gs[0].getComponents().indexOf(component))) ? gs[0] : null;
		}
	}

	public static class TYPE_SELECTOR implements ObservableValueSelector {
		@Override
		public Generic apply(Generic[] gs) {
			return gs[1];
		}
	}
}