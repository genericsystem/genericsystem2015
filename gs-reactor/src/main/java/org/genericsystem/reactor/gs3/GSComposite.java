package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.annotations.ForEach.ParentForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.ReactorDependencies.ChildReactorDependencies;
import org.genericsystem.reactor.annotations.Styles.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.ParentFlexDirection;
import org.genericsystem.reactor.gs3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.gs3.GSComposite.GSFooterComponent.GSFooterComponentLabel;
import org.genericsystem.reactor.gs3.GSComposite.GSHeaderComponent.GSHeaderComponentLabel;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;

@Flex("1 1 0%")
@Overflow("hidden")
@ReactorDependencies({ GSComposite.GSContentComponent.class })
@ChildFlexDirection("column")
@ChildReactorDependencies(GSContentComponent.GSContentComponentLabel.class)
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
	@ReactorDependencies(GSHeaderComponentLabel.class)
	public static class GSHeaderComponent extends GSCompositeDiv {
		public static class GSHeaderComponentLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ParentFlexDirection
	@ReactorDependencies(GSFooterComponentLabel.class)
	public static class GSFooterComponent extends GSCompositeDiv {
		public static class GSFooterComponentLabel extends GSLabelDisplayer {

		}
	}
}