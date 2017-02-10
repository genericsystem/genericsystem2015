package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.annotations.TagName;

public class HtmlTag {

	@TagName(TagName.BUTTON)
	public static class HtmlButton extends TagImpl {

	}

	@TagName(value = TagName.INPUT, type = TagName.CHECKBOX)
	public static class HtmlCheckBox extends TagImpl {

	}

	@TagName(TagName.DATALIST)
	public static class HtmlDatalist extends TagImpl {

	}

	@TagName(TagName.DIV)
	public static class HtmlDiv extends TagImpl {

	}

	@TagName(TagName.FOOTER)
	public static class HtmlFooter extends TagImpl {

	}

	@TagName(TagName.H1)
	public static class HtmlH1 extends TagImpl {

	}

	@TagName(TagName.H2)
	public static class HtmlH2 extends TagImpl {

	}

	@TagName(TagName.H3)
	public static class HtmlH3 extends TagImpl {

	}

	@TagName(TagName.H4)
	public static class HtmlH4 extends TagImpl {

	}

	@TagName(TagName.H5)
	public static class HtmlH5 extends TagImpl {

	}

	@TagName(TagName.H6)
	public static class HtmlH6 extends TagImpl {

	}

	@TagName(TagName.HEADER)
	public static class HtmlHeader extends TagImpl {

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

	@TagName(value = TagName.INPUT, type = TagName.RADIO)
	public static class HtmlRadio extends TagImpl {

	}

	@TagName(TagName.SECTION)
	public static class HtmlSection extends TagImpl {

	}

	@TagName(TagName.SELECT)
	public static class HtmlSelect extends TagImpl {

	}

	@TagName(TagName.SPAN)
	public static class HtmlSpan extends TagImpl {

	}

	@TagName(TagName.STRONG)
	public static class HtmlStrong extends TagImpl {

	}

	@TagName(TagName.UL)
	public static class HtmlUl extends TagImpl {

	}
}
