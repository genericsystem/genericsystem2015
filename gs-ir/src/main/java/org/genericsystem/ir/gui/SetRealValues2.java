package org.genericsystem.ir.gui;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.ComputeTrainedScores;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.ir.gui.SetRealValues2.DocumentDiv;
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
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ContextAction.CANCEL;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.InputTextEditorWithConversion;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

/**
 * The SetRealValues class can be used to provide accurate values for the text fields (zones).
 * 
 * These real values are stored in GS, and used by {@link ComputeTrainedScores} to compute the scores for each zone/filter pairs.
 * 
 * @author Pierrik Lassalas
 *
 */
@Deprecated
@DependsOnModel({ Doc.class, DocClass.class, ZoneGeneric.class, ZoneText.class })
@Style(name = "background-color", value = "#ffffff")
@Children({ AppHeader.class, DocumentDiv.class })
@Style(path = AppHeader.class, name = "background-color", value = "#00afeb")
@Children(path = AppHeader.class, value = { Logo.class, AppTitleDiv.class })
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "OCR results comparator")
public class SetRealValues2 extends RootTagImpl {

	private static final String docClass = "id-fr-front";
	private static final String gsPath = "/gs-cv_model";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, SetRealValues2.class, gsPath);
	}

	// For each document in the doc class, create a div with an image and the
	// input text
	@ForEach(DOC_CLASS_SELECTOR.class)
	@Children({ Image.class, TextDiv.class })
	@StyleClass("document-div")
	public static class DocumentDiv extends HtmlDiv {

	}

	// Map the image source to the filename stored in GS
	public static class Image extends HtmlImg {
		@Override
		public void init() {
			bindAttribute("src", "imgadr", context -> new SimpleStringProperty((String) context.getGeneric().getValue()));
		}
	}

	// Define the textdiv
	@Children({ ZoneLabelInput.class, Validate.class, Cancel.class })
	public static class TextDiv extends HtmlDiv {

	}

	// For each zone, create label + inputText + print the results for all the
	// filters
	@Children({ ZoneLabel.class, ZoneInput.class, FiltersDiv.class })
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

	// For each filter, create a row with the filtername and the results of the
	// ocr
	@FlexDirectionStyle(FlexDirection.ROW)
	@StyleClass("ocr-row")
	@Children({ FiltersList.class, FiltersTextList.class })
	@ForEach(OCR_SELECTOR.class)
	public static class FiltersDiv extends FlexDiv {

	}

	// Print the filtername
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "flex", value = "1")
	@BindText
	@StyleClass({ "ocr", "ocr-label" })
	public static class FiltersList extends FlexDiv {

	}

	// Print the ocr text for the corresponding filter
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "flex", value = "4")
	@BindText(OCR_LABEL.class)
	@StyleClass({ "ocr", "ocr-text" })
	public static class FiltersTextList extends FlexDiv {

	}

	// Create a validate button to persist the changes
	@SetText("Save")
	@BindAction(value = SAVE.class)
	public static class Validate extends HtmlButton {

	}

	// Create a cancel button to cancel the changes
	@SetText("Cancel")
	@BindAction(value = CANCEL.class)
	public static class Cancel extends HtmlButton {

	}

	public static class SELECTOR implements ObservableListExtractor {
		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Generic currentDoc = generics[0];
			Root root = currentDoc.getRoot();
			System.out.println("Document : " + currentDoc);
			Snapshot<ZoneTextInstance> zoneTextInstances = (Snapshot) currentDoc.getHolders(root.find(ZoneText.class));
			return (ObservableList) zoneTextInstances.toObservableList().filtered(zt -> "reality".equals(zt.getImgFilter().getValue())).sorted((g1, g2) -> Integer.compare(g1.getZoneNum(), g2.getZoneNum()));
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

	public static class OCR_SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			ZoneTextInstance zti = (ZoneTextInstance) generics[0];
			System.out.println("zti : " + zti.getZoneNum() + " " + zti.getImgFilter() + " " + zti.getDoc());
			Snapshot<Generic> filters = root.find(ImgFilter.class).getInstances();
			return filters.toObservableList();
		}
	}

	public static class OCR_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ImgFilterInstance ifi = (ImgFilterInstance) context.getGenerics()[0];
			ZoneTextInstance zti = (ZoneTextInstance) context.getGenerics()[1];
			DocInstance doc = zti.getDoc();
			ZoneText zt = (ZoneText) ifi.getRoot().find(ZoneText.class);
			ZoneTextInstance text = zt.getZoneText(doc, zti.getZone(), ifi);
			return new SimpleStringProperty(text.getValue().toString());
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