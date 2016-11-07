package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;

@Style(name = "justify-content", value = "space-around")
@Style(name = "padding", value = "10px")
@Style(path = { FlexDiv.class }, pos = 2, name = "flex", value = "1")
@FlexDirectionStyle(FlexDirection.ROW)
@Children({ Logo.class, AppTitleDiv.class, FlexDiv.class })
public class AppHeader extends FlexDiv {
	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(name = "flex", value = "1 1 0%")
	@Style(name = "align-items", value = "center")
	@Children(HtmlImg.class)
	@Attribute(path = HtmlImg.class, name = "src", value = "logoTransp.png")
	@Attribute(path = HtmlImg.class, name = "alt", value = "logo")
	@Style(path = HtmlImg.class, name = "height", value = "auto")
	@Style(path = HtmlImg.class, name = "width", value = "150px")
	public static class Logo extends FlexDiv {
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(name = "justify-content", value = "center")
	@Style(name = "flex", value = "3")
	@Style(name = "align-items", value = "center")
	@Style(name = "color", value = "White")
	@Style(name = "text-shadow", value = "1px 1px 2px black, 0 0 25px blue, 0 0 5px darkblue")
	@Children(HtmlH1.class)
	@SetText(path = HtmlH1.class, value = "Reactor Live Demo")
	public static class AppTitleDiv extends FlexDiv {
	}
}
