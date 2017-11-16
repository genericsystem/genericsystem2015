package org.genericsystem.ir.app;

import org.genericsystem.common.Root;
import org.genericsystem.cv.newmodel.SimpleModel.ConsolidatedType;
import org.genericsystem.cv.newmodel.SimpleModel.DocClassType;
import org.genericsystem.cv.newmodel.SimpleModel.DocType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgDocRel;
import org.genericsystem.cv.newmodel.SimpleModel.ImgPathType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgRefreshTimestampType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgTimestampType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgType;
import org.genericsystem.cv.newmodel.SimpleModel.LayoutType;
import org.genericsystem.cv.newmodel.SimpleModel.SupervisedType;
import org.genericsystem.cv.newmodel.SimpleModel.ZoneNumType;
import org.genericsystem.cv.newmodel.SimpleModel.ZoneType;
import org.genericsystem.ir.OcrEngineHolderVerticle;
import org.genericsystem.ir.app.gui.pages.ClassifierPage;
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
 */
@DependsOnModel({ Role.class, User.class, UserRole.class, DocClassType.class, LayoutType.class, ImgDocRel.class, DocType.class, ImgType.class, ZoneType.class, ZoneNumType.class, ConsolidatedType.class, ImgPathType.class, ImgTimestampType.class,
		ImgRefreshTimestampType.class, SupervisedType.class })
@Children({ HomePage.class, ClassifierPage.class })
public class WatchApp extends RootTagImpl {

	// private static final String gsPath = "/gs-cv_model3";
	private static final String gsPath = "/gs-cv-newmodel";

	@Override
	public void init() {
		createNewInitializedProperty(PageSwitcher.PAGE, c -> PageSwitcher.HOME_PAGE);
	}

	public static void main(String[] mainArgs) {
		ApplicationServer server = ApplicationServer.startSimpleGenericApp(mainArgs, WatchApp.class, gsPath);
		Root root = server.getRoots().get(System.getenv("HOME") + "/genericsystem/" + gsPath);
		// deployVerticles(root);
	}

	private static void deployVerticles(Root root) {
		OcrEngineHolderVerticle ocrEngineHolderVerticle = new OcrEngineHolderVerticle(root);
		ocrEngineHolderVerticle.doDeploy();
	}
}
