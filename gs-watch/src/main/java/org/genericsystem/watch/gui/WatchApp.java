package org.genericsystem.watch.gui;

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
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

@DependsOnModel({ Doc.class, RefreshTimestamp.class, DocTimestamp.class, DocFilename.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ZoneTimestamp.class, ImgFilter.class, LevDistance.class, MeanLevenshtein.class, Score.class })
@Children({ HomePage.class, VisualizeFiltersStatistics.class })
public class WatchApp extends RootTagImpl {

	private static final String gsPath = "/gs-cv_model3";

	public static void main(String[] mainArgs) {
		ApplicationServer server = ApplicationServer.startSimpleGenericApp(mainArgs, WatchApp.class, gsPath);
		// Root root = server.getRoots().get(System.getenv("HOME") + "/genericsystem/" + gsPath);
		// VerticleDeployerFromWatchApp deployer = new VerticleDeployerFromWatchApp(root);
		// deployer.doDeploy();
	}

	@Override
	public void init() {
		createNewInitializedProperty(PageSwitcher.PAGE, c -> PageSwitcher.HOME_PAGE);
	}

}
