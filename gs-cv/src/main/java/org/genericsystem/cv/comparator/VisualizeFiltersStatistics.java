package org.genericsystem.cv.comparator;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.VisualizeFiltersStatistics.DocumentDiv;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.ObservableListExtractor.COMPONENTS;
import org.genericsystem.reactor.context.ObservableListExtractor.HOLDERS;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.InputTextEditorWithConversion;
import org.genericsystem.reactor.gscomponents.InstancesTable;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

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
@Children({ DocumentDiv.class })
public class VisualizeFiltersStatistics extends RootTagImpl {

	private static final String docClass = "id-fr-front";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, VisualizeFiltersStatistics.class, "/gs-cv_model");
	}

	@ForEach(DOC_CLASS_SELECTOR.class)
	@Children({ ZoneLabelDiv.class })
	public static class DocumentDiv extends HtmlDiv {

	}

	@ForEach(ZONE_SELECTOR.class)
	@Children({ ZoneLabel.class, TextDiv.class })
	public static class ZoneLabelDiv extends HtmlDiv {

	}

	// Define the zone label
	@BindText(ZONE_LABEL.class)
	public static class ZoneLabel extends HtmlLabel {

	}
	
	@Children({ TableResults.class })
	public static class TextDiv extends HtmlDiv {

	}
	
	@ForEach(COMPONENTS.class)
//	@DirectSelect(path = TitledInstancesTable.class,  value = { Score.class })
	public static class TableResults extends HtmlDiv{
		
	}
	
	public static class DOC_CLASS_SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			Generic currentDocClass = root.find(DocClass.class).getInstance(docClass);
			System.out.println("Current doc class : " + currentDocClass);
			Generic currentZone = root.find(ZoneGeneric.class);
			Snapshot<Generic> zoneInstances = currentZone.getInstances(currentDocClass);
			return zoneInstances.toObservableList();
		}
	}
	
	public static class ZONE_SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			System.out.println("Current zone : " + generics[0]);
			Root root = generics[0].getRoot();
			Generic score = root.find(Score.class);
			Snapshot<Generic> scores = score.getInstances().filter(z -> ((ScoreInstance) z).getZone() == generics[0]);
			return scores.toObservableList();
		}
	}
	
	public static class SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Generic currentDoc = generics[0];
			Root root = currentDoc.getBaseComponent().getRoot();
			Score score = root.find(Score.class);
			Snapshot<ScoreInstance> scoreInstances = (Snapshot) score.getInstances();
			return (ObservableList) scoreInstances.toObservableList();
		}
	}


	public static class ZONE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return new SimpleStringProperty("Zone " + context.getGenerics()[0].getValue());
		}
	}

}