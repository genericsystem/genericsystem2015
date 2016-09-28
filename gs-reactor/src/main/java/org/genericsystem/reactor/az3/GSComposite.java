package org.genericsystem.reactor.az3;

import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.ReactorDependencies.ChildReactorDependencies;
import org.genericsystem.reactor.annotations.Styles.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.az3.GSComposite.GSFooterComponent;
import org.genericsystem.reactor.az3.GSComposite.GSFooterComponent.GSFooterComponentLabel;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent.GSHeaderComponentLabel;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;

@Flex("1 1 0%")
@Overflow("hidden")
@ReactorDependencies({ GSComposite.GSContentComponent.class })
@ChildFlexDirection(decorate = GSContentComponent.class, value = FlexDirection.COLUMN)
@ChildFlexDirection(decorate = GSHeaderComponent.class, value = FlexDirection.COLUMN)
@ChildFlexDirection(decorate = GSFooterComponent.class, value = FlexDirection.COLUMN)
@ChildReactorDependencies(decorate = GSContentComponent.class, value = GSContentComponent.GSContentComponentLabel.class)
@ChildReactorDependencies(decorate = GSHeaderComponent.class, value = GSHeaderComponentLabel.class)
@ChildReactorDependencies(decorate = GSFooterComponent.class, value = GSFooterComponentLabel.class)
public abstract class GSComposite extends GSCompositeDiv {

	@Flex("1 1 0%")
	@Overflow("hidden")
	public static class GSContentComponent extends GSCompositeDiv {
		public static class GSContentComponentLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	public static class GSHeaderComponent extends GSCompositeDiv {
		public static class GSHeaderComponentLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	public static class GSFooterComponent extends GSCompositeDiv {
		public static class GSFooterComponentLabel extends GSLabelDisplayer {

		}
	}
}