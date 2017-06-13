package org.genericsystem.cv;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.SetRealValues.Image;
import org.genericsystem.cv.SetRealValues.TextDiv;
import org.genericsystem.cv.SetRealValues.Validate;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute;
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

@DependsOnModel({ Doc.class, DocClass.class, ZoneGeneric.class, ZoneText.class })
@Children({ Image.class, TextDiv.class, Validate.class })
public class SetRealValues extends RootTagImpl {

	private static final String imageName = "image2-0-5666029511635993302.png";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, SetRealValues.class, "/gs-cv-model");
	}

	@Attribute(name = "src", value = imageName)
	public static class Image extends HtmlImg {

	}

	@Children(ZoneLabelInput.class)
	public static class TextDiv extends HtmlDiv {

	}

	@Children({ ZoneLabel.class, ZoneInput.class })
	@ForEach(SELECTOR.class)
	public static class ZoneLabelInput extends HtmlDiv {

	}

	@BindText(ZONE_LABEL.class)
	public static class ZoneLabel extends HtmlLabel {

	}

	@BindText
	public static class ZoneInput extends InputTextEditorWithConversion {

	}

	public static class SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			Generic currentDoc = root.find(Doc.class).getInstance(imageName);
			System.out.println("Current document : " + currentDoc);
			ZoneText zoneText = root.find(ZoneText.class);
			Snapshot<Generic> zoneTextInstances = currentDoc.getHolders(zoneText)
					.filter(zt -> "reality".equals(((ZoneTextInstance) zt).getImgFilter().getValue()));
			System.out.println("Nb zones : " + zoneTextInstances.size());
			return zoneTextInstances.toObservableList();
		}
	}

	@SetText("Validate")
	@BindAction(value = SAVE.class)
	public static class Validate extends HtmlButton {

	}

	public static class SAVE implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			System.out.println("Saving text for document " + imageName);
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