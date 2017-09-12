package org.genericsystem.ir.app;

import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocFilename;
import org.genericsystem.cv.model.Doc.DocTimestamp;
import org.genericsystem.cv.model.Doc.RefreshTimestamp;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.LevDistance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTimestamp;
import org.genericsystem.ir.OcrEngineHolderVerticle;
import org.genericsystem.ir.app.gui.pages.FiltersStatisticsPage;
import org.genericsystem.ir.app.gui.pages.HomePage;
import org.genericsystem.ir.app.gui.utils.PageSwitcher;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.genericsystem.security.model.Role;
import org.genericsystem.security.model.User;
import org.genericsystem.security.model.UserRole;

/**
 * This application can be used to deploy the verticles of gs-watch to automatically process the documents sent to a specific email address.
 * <p>
 * The main view shows a list of documents for each document's class. Each record can be visualized, and supervised (provided that it was OCR'r first).
 * 
 * @author Pierrik Lassalas
 */
@DependsOnModel({ Role.class, User.class, UserRole.class, Doc.class, RefreshTimestamp.class, DocTimestamp.class, DocFilename.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ZoneTimestamp.class, ImgFilter.class, LevDistance.class,
		MeanLevenshtein.class, Score.class })
@Children({ HomePage.class, FiltersStatisticsPage.class })
public class WatchApp extends RootTagImpl {

	private static final String gsPath = "/gs-cv_model3";

	@Override
	public void init() {
		createNewInitializedProperty(PageSwitcher.PAGE, c -> PageSwitcher.HOME_PAGE);
	}

	public static void main(String[] mainArgs) {
		ApplicationServer server = ApplicationServer.startSimpleGenericApp(mainArgs, WatchApp.class, gsPath);
		Root root = server.getRoots().get(System.getenv("HOME") + "/genericsystem/" + gsPath);
		deployVerticles(root);
	}

	private static void deployVerticles(Root root) {
		OcrEngineHolderVerticle ocrEngineHolderVerticle = new OcrEngineHolderVerticle(root);
		ocrEngineHolderVerticle.doDeploy();
	}
}
