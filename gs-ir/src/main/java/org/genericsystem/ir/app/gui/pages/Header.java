package org.genericsystem.ir.app.gui.pages;

import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;

@Children({ Logo.class, AppTitleDiv.class, HomePageUserLogin.class })
@Style(name = "background-color", value = "#4283de")
@Style(path = Logo.class, name = "flex", value = "1")
@Style(path = AppTitleDiv.class, name = "flex", value = "4")
@Style(path = HomePageUserLogin.class, name = "flex", value = "1")
public class Header extends AppHeader {

}
