package org.genericsystem.reactor.az3;

import org.genericsystem.reactor.annotations.ForEach.ParentForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.ReactorDependencies.ChildReactorDependencies;
import org.genericsystem.reactor.annotations.Styles.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.ParentFlexDirection;
import org.genericsystem.reactor.az3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.az3.GSComposite.GSFooterComponent;
import org.genericsystem.reactor.az3.GSComposite.GSFooterComponent.GSFooterComponentLabel;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent.GSHeaderComponentLabel;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;

@Flex("1 1 0%")
@Overflow("hidden")
@ReactorDependencies({ GSComposite.GSContentComponent.class })
@ChildFlexDirection("column")
@ChildReactorDependencies(decorate = GSContentComponent.class, value = GSContentComponent.GSContentComponentLabel.class)
@ChildReactorDependencies(decorate = GSHeaderComponent.class, value = GSHeaderComponentLabel.class)
@ChildReactorDependencies(decorate = GSFooterComponent.class, value = GSFooterComponentLabel.class)
public abstract class GSComposite extends GSCompositeDiv {

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ParentForEach
	@ParentFlexDirection
	public static class GSContentComponent extends GSCompositeDiv {
		public static class GSContentComponentLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ParentFlexDirection
	public static class GSHeaderComponent extends GSCompositeDiv {
		public static class GSHeaderComponentLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ParentFlexDirection
	public static class GSFooterComponent extends GSCompositeDiv {
		public static class GSFooterComponentLabel extends GSLabelDisplayer {

		}
	}
}