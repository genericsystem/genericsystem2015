package org.genericsystem.reactor.az3;

import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.annotations.Styles.ReverseFlexDirection;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az3.GSComposite.Content;
import org.genericsystem.reactor.az3.GSComposite.Footer;
import org.genericsystem.reactor.az3.GSComposite.Footer.FooterLabel;
import org.genericsystem.reactor.az3.GSComposite.GSTable.ContentRow;
import org.genericsystem.reactor.az3.GSComposite.Header;
import org.genericsystem.reactor.az3.GSComposite.Header.HeaderLabel;
import org.genericsystem.reactor.gstag.HtmlLabel.GSLabelDisplayer;

@Flex("1 1 0%")
@Overflow("hidden")
@ReactorDependencies({ GSComposite.Content.class })
@FlexDirectionStyle(path = Content.class, value = FlexDirection.COLUMN)
@FlexDirectionStyle(path = Header.class, value = FlexDirection.COLUMN)
@FlexDirectionStyle(path = Footer.class, value = FlexDirection.COLUMN)
@ReactorDependencies(path = Content.class, value = Content.ContentLabel.class)
@ReactorDependencies(path = Header.class, value = HeaderLabel.class)
@ReactorDependencies(path = Footer.class, value = FooterLabel.class)
public abstract class GSComposite extends GSCompositeDiv {

	@Flex("1 1 0%")
	@Overflow("hidden")
	public static class Content extends GSCompositeDiv {
		public static class ContentLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	public static class Header extends GSCompositeDiv {
		public static class HeaderLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	public static class Footer extends GSCompositeDiv {
		public static class FooterLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	@ReactorDependencies(ContentRow.class)
	public static abstract class GSTable extends GSCompositeDiv {

		@ReverseFlexDirection
		public static class HeaderRow extends GSComposite {

		}

		@ReverseFlexDirection
		public static class ContentRow extends GSComposite {

		}

		@ReverseFlexDirection
		public static class FooterRow extends GSComposite {

		}
	}
}