package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;

@FlexDirectionStyle(FlexDirection.ROW)
@Style(path = TagImpl.class, name = "flex-wrap", value = "wrap")
@Style(path = TagImpl.class, name = "margin", value = "10px")
@Style(path = TagImpl.class, name = "padding", value = "10px")
@Style(path = TagImpl.class, name = "border-radius", value = "10px")
@Style(path = TagImpl.class, name = "background-color", value = "white")
@StyleClass(path = TagImpl.class, value = "screenResponsive")
@Style(path = TagImpl.class, name = "max-width", value = "100%")
public class Responsive extends FlexDiv {
}
