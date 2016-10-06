package org.genericsystem.reactor.gscomponents2;

import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Styles.Flex;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.Overflow;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents2.GSComposite.Content;
import org.genericsystem.reactor.gscomponents2.GSComposite.Footer;
import org.genericsystem.reactor.gscomponents2.GSComposite.Header;
import org.genericsystem.reactor.gscomponents2.GSComposite.Footer.FooterLabel;
import org.genericsystem.reactor.gscomponents2.GSComposite.Header.HeaderLabel;

import org.genericsystem.reactor.htmltag.HtmlLabel.GSLabelDisplayer;

@Flex("1 1 0%")
@Overflow("hidden")
@ReactorDependencies({ GSComposite.Content.class })
@FlexDirectionStyle(path = Content.class, value = FlexDirection.COLUMN)
@FlexDirectionStyle(path = Header.class, value = FlexDirection.COLUMN)
@FlexDirectionStyle(path = Footer.class, value = FlexDirection.COLUMN)
@ReactorDependencies(path = Content.class, value = Content.ContentLabel.class)
@ReactorDependencies(path = Header.class, value = HeaderLabel.class)
@ReactorDependencies(path = Footer.class, value = FooterLabel.class)
public abstract class GSComposite extends GSDiv {

	@Flex("1 1 0%")
	@Overflow("hidden")
	public static class Content extends GSDiv {
		public static class ContentLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	public static class Header extends GSDiv {
		public static class HeaderLabel extends GSLabelDisplayer {

		}
	}

	@Flex("1 1 0%")
	@Overflow("hidden")
	public static class Footer extends GSDiv {
		public static class FooterLabel extends GSLabelDisplayer {

		}
	}
}