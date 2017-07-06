package org.genericsystem.cv.watch;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.cv.watch.EditDocumentZones.TextDiv;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.InheritStyle;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ContextAction.CANCEL;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.InputTextEditorWithConversion;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

@Children(path = FlexDiv.class, value = { HtmlHyperLink.class, TextDiv.class })
@InheritStyle("background-color")
public class EditDocumentZones extends ModalWithDisplay {

	@FlexDirectionStyle(FlexDirection.ROW)
//	@Children({ ImageDiv.class, EditDiv.class })
	@Children({ FlexDiv.class, FlexDiv.class })
	@Children(path = FlexDiv.class, pos = 0, value = { Image.class, Validate.class, Cancel.class })
	@FlexDirectionStyle(path = FlexDiv.class, pos = 0, value = FlexDirection.COLUMN)
	@Children(path = FlexDiv.class, pos = 1, value = ZoneTextDiv.class)
	@FlexDirectionStyle(path = FlexDiv.class, pos = 1, value = FlexDirection.COLUMN)
	public static class TextDiv extends FlexDiv {

	}
	
	@SetText("Save")
	@BindAction(value = SAVE.class)
	public static class Validate extends HtmlButton {
		// Persists the changes
	}

	@SetText("Cancel")
	@BindAction(value = CANCEL.class)
	public static class Cancel extends HtmlButton {
		// Cancel the changes
	}
	
	@Style(name = "margin", value = "0.5em")
	@Style(name = "flex", value = "0 0 auto")
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	public static class Image extends HtmlImg {
		@Override
		public void init() {
			bindAttribute("src", "imgadr",
					context -> new SimpleStringProperty((String) context.getGeneric().getValue()));
		}
	}

	// For each zone, create label + inputText + print the results for all the
	// filters
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Children({ ZoneLabelInput.class, FiltersDiv.class })
	@ForEach(SELECTOR.class)
	public static class ZoneTextDiv extends FlexDiv {

	}
	
	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ ZoneLabel.class, ZoneInput.class })
	public static class ZoneLabelInput extends FlexDiv {

	}

	// Define the zone label
	@BindText(ZONE_LABEL.class)
	public static class ZoneLabel extends HtmlLabel {

	}

	// Define the inputText
	@BindText
	@StyleClass("glowing-border")
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
			System.out.println("Saving text for class " + context.getGenerics().toString());
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
