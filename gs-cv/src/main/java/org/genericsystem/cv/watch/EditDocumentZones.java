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
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.InheritStyle;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ContextAction.CANCEL;
import org.genericsystem.reactor.context.ObservableContextSelector.SELECTION_SELECTOR;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.InputTextEditorWithConversion;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.reactor.gscomponents.TagImpl;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

@Children(path = FlexDiv.class, value = { HtmlHyperLink.class, TextDiv.class })
@InheritStyle("background-color")
public class EditDocumentZones extends ModalWithDisplay {

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ FlexDiv.class, FlexDiv.class })
	@Children(path = FlexDiv.class, pos = 0, value = { Image.class, ZoneTextDiv.class })
	@Children(path = FlexDiv.class, pos = 1, value = { Validate.class, Cancel.class })
	@FlexDirectionStyle(path = FlexDiv.class, value = FlexDirection.COLUMN)
	@FlexDirectionStyle(path = { FlexDiv.class, FlexDiv.class }, pos = { -1, -1 }, value = FlexDirection.ROW)
	@Style(path = { FlexDiv.class, TagImpl.class }, pos = { -1, -1 }, name = "justify-content", value = "center")
	@Style(path = { FlexDiv.class, TagImpl.class }, pos = { -1, -1 }, name = "align-items", value = "center")
	@SelectContext(path = FlexDiv.class, pos = 0, value = SELECTION_SELECTOR.class)
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

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Children({ ZoneLabelInput.class, ZonesDetails.class })
	@ForEach(ZONE_SELECTOR.class)
	public static class ZoneTextDiv extends FlexDiv {
		// For each zone, create a div with label + inputText
		// and create a div for the results for all filters
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ ZoneLabel.class, ZoneInput.class })
	public static class ZoneLabelInput extends FlexDiv {

	}

	@BindText(ZONE_LABEL.class)
	@BindAction(MODAL_DISPLAY_FLEX_CUSTOM.class)
	@Attribute(name = "name", value = "zone")
	public static class ZoneLabel extends HtmlHyperLink {
		// Define the zone label
	}

	@BindText
	@StyleClass("glowing-border")
	public static class ZoneInput extends InputTextEditorWithConversion {
		// Define the inputText
	}

	public static class ZONE_SELECTOR implements ObservableListExtractor {
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
	
	public static class MODAL_DISPLAY_FLEX_CUSTOM implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getParent().getParent().find(ModalWithDisplay.class).getDisplayProperty(context).setValue("flex");
		}
	}
}
