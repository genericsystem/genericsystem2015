package org.genericsystem.reactor.az3;

import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az3.GSComposite.GSContentComponent;
import org.genericsystem.reactor.az3.GSComposite.GSFooterComponent;
import org.genericsystem.reactor.az3.GSComposite.GSFooterComponent.GSFooterComponentLabel;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent;
import org.genericsystem.reactor.az3.GSComposite.GSHeaderComponent.GSHeaderComponentLabel;
import org.genericsystem.reactor.az3.GSComposite.GSTable.GSContentRow;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;

@Flex("1 1 0%")
@Overflow("hidden")
@ReactorDependencies({ GSComposite.GSContentComponent.class })
@FlexDirectionStyle(decorate = GSContentComponent.class, value = FlexDirection.COLUMN)
@FlexDirectionStyle(decorate = GSHeaderComponent.class, value = FlexDirection.COLUMN)
@FlexDirectionStyle(decorate = GSFooterComponent.class, value = FlexDirection.COLUMN)
@ReactorDependencies(decorate = GSContentComponent.class, value = GSContentComponent.GSContentComponentLabel.class)
@ReactorDependencies(decorate = GSHeaderComponent.class, value = GSHeaderComponentLabel.class)
@ReactorDependencies(decorate = GSFooterComponent.class, value = GSFooterComponentLabel.class)
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

	@ReactorDependencies(GSContentRow.class)
	public static abstract class GSTable extends GSCompositeDiv {

		@Flex("1 1 0%")
		@Overflow("hidden")
		public static class GSHeaderRow extends GSComposite {

		}

		@Flex("1 1 0%")
		@Overflow("hidden")
		public static class GSContentRow extends GSComposite {

		}

		@Flex("1 1 0%")
		@Overflow("hidden")
		public static class GSFooterRow extends GSComposite {

		}
	}
}