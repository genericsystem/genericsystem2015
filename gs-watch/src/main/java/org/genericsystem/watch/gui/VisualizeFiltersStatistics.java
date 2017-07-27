package org.genericsystem.watch.gui;

import java.util.Arrays;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.ComputeTrainedScores;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.genericsystem.watch.VerticleDeployerFromWatchApp;
import org.genericsystem.watch.gui.PageSwitcher.FILTERS_STATISTICS;
import org.genericsystem.watch.gui.VisualizeFiltersStatistics.DocClassStatisticsDiv;

import io.vertx.core.Verticle;
import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * This class provides a gs-reactor application to visualize and update the {@link Score} and {@link MeanLevenshtein} distances for each filters.
 * 
 * @author Pierrik Lassalas
 */
// TODO: redesign the interface (smaller, add foreach loops)
@Switch(FILTERS_STATISTICS.class)
@DependsOnModel({ Doc.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ImgFilter.class, Score.class, MeanLevenshtein.class })
@Style(name = "background-color", value = "#ffffff")
@Children({ AppHeader.class, DocClassStatisticsDiv.class })
@Style(path = AppHeader.class, name = "background-color", value = "#00afeb")
@Children(path = AppHeader.class, value = { Logo.class, AppTitleDiv.class })
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "Global OCR accuracy per zone")
public class VisualizeFiltersStatistics extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, VisualizeFiltersStatistics.class, "/gs-cv_model3");
	}

	@Override
	public void init() {
		createNewInitializedProperty(PageSwitcher.PAGE, c -> PageSwitcher.FILTERS_STATISTICS);
	}

	@Children({ FlexDiv.class, FlexDiv.class })
	@Children(path = FlexDiv.class, pos = 1, value = StatisticsTable.class)
	@Children(path = FlexDiv.class, pos = 0, value = { FlexDiv.class, FlexDiv.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 1 }, value = ButtonDiv.class)
	@BindText(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 })
	@FlexDirectionStyle(path = FlexDiv.class, pos = 0, value = FlexDirection.ROW)
	@Style(path = FlexDiv.class, pos = 0, name = "justify-content", value = "center")
	@Style(path = FlexDiv.class, pos = 0, name = "align-items", value = "center")
	@Style(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, name = "flex", value = "1 1 auto")
	@Style(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 1 }, name = "flex", value = "0 1 auto")
	@StyleClass(path = FlexDiv.class, pos = 0, value = "doc-class-title")
	@Style(name = "width", value = "90%")
	@Style(name = "margin", value = "auto")
	@ForEach(DOC_CLASS_SELECTOR.class)
	// @DirectSelect(path = { FlexDiv.class, StatisticsTable.class }, pos = { 1, 0 }, value = Score.class)
	// @Switch(path = { FlexDiv.class, InstancesTable.class }, pos = { 1, 0 }, value = SCORE_SWITCHER.class)
	public static class DocClassStatisticsDiv extends FlexDiv {

	}

	@Children({ ComputeStatsButton.class, ComputeStatsStrictButton.class })
	@FlexDirectionStyle(FlexDirection.ROW)
	public static class ButtonDiv extends FlexDiv {

	}

	@SetText("Compute statistics")
	@BindAction({ COMPUTE_STATS.class })
	public static class ComputeStatsButton extends HtmlButton {

	}

	@SetText("Compute statistics (strict mode)")
	@BindAction({ COMPUTE_STATS.class })
	public static class ComputeStatsStrictButton extends HtmlButton {

	}

	public static class DOC_CLASS_SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			DocClass docClass = root.find(DocClass.class);
			Snapshot<Generic> docClassInstances = docClass.getInstances();
			if (null == docClassInstances)
				return FXCollections.emptyObservableList();
			return docClassInstances.toObservableList();
		}
	}

	public static class SCORE_SWITCHER implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return Bindings.createBooleanBinding(() -> {
				Score score = (Score) context.getGeneric();
				DocClassInstance docClassInstance = (DocClassInstance) context.getGenerics()[1];
				// System.out.println("score: " + score.info());
				// System.out.println("docClass: " + docClassInstance.info());
				score.getInstances().forEach(g -> System.out.println(((ScoreInstance) g).getZone().getDocClass())); // XXX why null?
				return true;
			});
		}
	}

	public static class COMPUTE_STATS implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Computing scores...");
			computeStatistics(context, tag, false);
		}
	}

	public static class COMPUTE_STATS_STRICT implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Computing scores (using strict mode)...");
			computeStatistics(context, tag, true);
		}
	}

	public static void computeStatistics(Context context, Tag tag, boolean useStrict) {
		DocClassInstance docClassInstance = (DocClassInstance) context.getGeneric();
		Root root = docClassInstance.getRoot();
		Arrays.asList(context.getGenerics()).forEach(g -> System.out.println(g.info()));
		Verticle worker = new WorkerVerticle() {
			@Override
			public void start() throws Exception {
				ComputeTrainedScores.compute(root, docClassInstance.getValue().toString(), useStrict);
				System.out.println("Done computing scores!");
			}
		};
		VerticleDeployerFromWatchApp.deployWorkerVerticle(worker, "Failed to execute the task");
	}

}