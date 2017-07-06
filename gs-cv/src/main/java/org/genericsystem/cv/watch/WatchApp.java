package org.genericsystem.cv.watch;

import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;

@DependsOnModel({ Doc.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ImgFilter.class })
@Style(name = "background-color", value = "#ffffff")
@Children({ AppHeader.class })
@Style(path = AppHeader.class, name = "background-color", value = "#00afeb")
@Children(path = AppHeader.class, value = { Logo.class, AppTitleDiv.class })
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "GS-Watch interface")
public class WatchApp {

	private static final String gsPath = "/gs-cv_model";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, SetRealValues2.class, gsPath);
	}
}
