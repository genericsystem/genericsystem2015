package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.annotations.TagName;

public class HtmlTag {

	@TagName(TagName.BUTTON)
	public static class HtmlButton extends TagImpl {

	}

	@TagName(TagName.DATALIST)
	public static class HtmlDatalist extends TagImpl {

	}

	@TagName(TagName.DIV)
	public static class HtmlDiv extends TagImpl {

	}

	@TagName(TagName.H1)
	public static class HtmlH1 extends TagImpl {

	}

	@TagName(TagName.H2)
	public static class HtmlH2 extends TagImpl {

	}

	@TagName(TagName.H4)
	public static class HtmlH4 extends TagImpl {

	}

	@TagName(TagName.A)
	public static class HtmlHyperLink extends TagImpl {

	}

	@TagName(TagName.IMG)
	public static class HtmlImg extends TagImpl {

	}

	@TagName(TagName.INPUT)
	public static class HtmlInputText extends TagImpl {

	}

	@TagName(TagName.LABEL)
	public static class HtmlLabel extends TagImpl {

		public static class GSLabelDisplayer extends HtmlLabel {

			public GSLabelDisplayer() {
				bindText();
			}
		}
	}

	@TagName(TagName.LI)
	public static class HtmlLi extends TagImpl {

	}

	@TagName(TagName.OPTION)
	public static class HtmlOption extends TagImpl {

	}

	@TagName(TagName.P)
	public static class HtmlP extends TagImpl {

	}

	@TagName(TagName.SPAN)
	public static class HtmlSpan extends TagImpl {

	}

	@TagName(TagName.UL)
	public static class HtmlUl extends TagImpl {

	}
}
