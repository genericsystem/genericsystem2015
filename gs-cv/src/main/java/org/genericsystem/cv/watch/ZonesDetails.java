package org.genericsystem.cv.watch;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.cv.watch.ZonesDetails.FiltersDiv;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindSelection;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.Modal;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

//@Children(FlexDiv.class)
@Children(path = FlexDiv.class, value = { HtmlHyperLink.class, FiltersDiv.class })
@StyleClass(path = { FlexDiv.class, FiltersDiv.class }, value = "filter-results")
@Style(path = FlexDiv.class, name = "display", value = "block")
@Style(path = FlexDiv.class, name = "padding", value = "1.5em")
public class ZonesDetails extends ModalWithDisplay {
	
	
	@FlexDirectionStyle(FlexDirection.ROW)
	@StyleClass("ocr-row")
	@Children({ FilterNames.class, FiltersOcrText.class })
	@ForEach(OCR_SELECTOR.class)
	public static class FiltersDiv extends FlexDiv {
		// For each filter, create a row with the filtername and the results of
		// the ocr
	}
	
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@BindText
	@StyleClass({ "ocr", "ocr-label" })
	public static class FilterNames extends FlexDiv {
		// Print the filtername
	}

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@BindText(OCR_TEXT.class)
	@StyleClass({ "ocr", "ocr-text" })
	public static class FiltersOcrText extends FlexDiv {
		// Print the ocr text for the corresponding filter
	}
	
	public static class OCR_SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			ZoneTextInstance zti = (ZoneTextInstance) generics[0];
			Snapshot<Generic> filters = root.find(ImgFilter.class).getInstances();
			return filters.toObservableList();
		}
	}

	public static class OCR_TEXT implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			ImgFilterInstance ifi = (ImgFilterInstance) context.getGenerics()[0];
			ZoneTextInstance zti = (ZoneTextInstance) context.getGenerics()[1];
			DocInstance doc = zti.getDoc();
			ZoneText zt = (ZoneText) ifi.getRoot().find(ZoneText.class);
			ZoneTextInstance text = zt.getZoneText(doc, zti.getZone(), ifi);
			return new SimpleStringProperty(text == null ? null : text.getValue().toString());
		}
	}

}
