package org.genericsystem.cv.comparator;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.SetRealValues.DocumentDiv;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.InputTextEditorWithConversion;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

/**
 * The SetRealValues class can be used to provide accurate values for the text
 * fields (zones).
 * 
 * These real values are stored in GS, and used by {@link ComputeScores} to
 * compute the scores for each zone/filter pairs.
 * 
 * @author middleware
 *
 */
@DependsOnModel({ Doc.class, DocClass.class, ZoneGeneric.class, ZoneText.class })
@Children({ DocumentDiv.class })
public class SetRealValues extends RootTagImpl {

	private static final String docClass = "id-fr-front";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, SetRealValues.class, "/gs-cv_model");
	}

	/*
	 * For each document saved in GS, create image + textdiv elements
	 */
	@ForEach(DOC_CLASS_SELECTOR.class)
	@Children({ Image.class, TextDiv.class })
	public static class DocumentDiv extends HtmlDiv {

	}

	// Map the image source to the filename stored in GS
	public static class Image extends HtmlImg {
		@Override
		public void init() {
			bindAttribute("src", "imgadr",
					context -> new SimpleStringProperty((String) context.getGeneric().getValue()));
		}
	}

	// Define the textdiv
	@Children({ ZoneLabelInput.class, Validate.class })
	public static class TextDiv extends HtmlDiv {

	}

	/*
	 * For each zone, create label + inputText
	 */
	@Children({ ZoneLabel.class, ZoneInput.class })
	@ForEach(SELECTOR.class)
	public static class ZoneLabelInput extends HtmlDiv {

	}

	// Define the zone label
	@BindText(ZONE_LABEL.class)
	public static class ZoneLabel extends HtmlLabel {

	}

	// Define the inputText
	@BindText
	public static class ZoneInput extends InputTextEditorWithConversion {

	}

	@SetText("Validate")
	@BindAction(value = SAVE.class)
	public static class Validate extends HtmlButton {

	}

	public static class SELECTOR implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Generic currentDoc = generics[0];
			Root root = currentDoc.getRoot();
			System.out.println("Document : " + currentDoc);
			Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class))
					.filter(zt -> "reality".equals(((ZoneTextInstance) zt).getImgFilter().getValue()));
			return (ObservableList) zoneTextInstances.toObservableList()
					.sorted((g1, g2) -> Integer.compare(g1.getZoneNum(), g2.getZoneNum()));
		}
	}

	public static class DOC_CLASS_SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			Generic currentDocClass = root.find(DocClass.class).getInstance(docClass);
			System.out.println("Current doc class : " + currentDocClass);
			Snapshot<Generic> docInstances = currentDocClass.getHolders(root.find(Doc.class));
			return docInstances.toObservableList();
		}
	}

	public static class SAVE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Saving text for class " + docClass);
			context.getGeneric().getRoot().getCurrentCache().flush();
		}
	}

	public static class ZONE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return new SimpleStringProperty("Zone " + ((ZoneTextInstance) context.getGenerics()[0]).getZone());
		}
	}

}