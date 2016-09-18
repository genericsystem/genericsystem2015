package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.annotations.ForEach.ParentForEach;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Style.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Style.Flex;
import org.genericsystem.reactor.annotations.Style.Overflow;
import org.genericsystem.reactor.annotations.Style.ParentFlexDirection;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;

@Flex("1 1 0%")
@Overflow("hidden")
@ReactorDependencies({ GSComposite.GSContentComponent.class })
@ChildFlexDirection("column")
public abstract class GSComposite extends CompositeTagImpl {

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ParentForEach
	@ParentFlexDirection
	@ReactorDependencies(GSContentComponent.GSContentComponentLabel.class)
	public static class GSContentComponent extends CompositeTagImpl {
		public static class GSContentComponentLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ParentFlexDirection
	@ReactorDependencies(GSHeaderComponent.GSHeaderComponentLabel.class)
	public static class GSHeaderComponent extends CompositeTagImpl {
		public static class GSHeaderComponentLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ParentFlexDirection
	@ReactorDependencies(GSFooterComponent.GSFooterComponentLabel.class)
	public static class GSFooterComponent extends CompositeTagImpl {
		public static class GSFooterComponentLabel extends GSLabelDisplayer {

		}
	}
}