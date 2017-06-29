package org.genericsystem.cv.comparator;

import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledHorizontalInstancesTable;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstanceStepEditor;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

/**
 * The SetRealValues class can be used to provide accurate values for the text
 * fields (zones).
 * 
 * These real values are stored in GS, and used by {@link ComputeTrainedScores}
 * to compute the scores for each zone/filter pairs.
 * 
 * @author middleware
 *
 */
@DependsOnModel({ Doc.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ImgFilter.class, Score.class,
		MeanLevenshtein.class })
@Children({ TitledInstancesTable.class })
@DirectSelect(path = TitledInstancesTable.class, value = Score.class)
public class VisualizeFiltersStatistics extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, VisualizeFiltersStatistics.class, "/gs-cv_model2");
	}

}