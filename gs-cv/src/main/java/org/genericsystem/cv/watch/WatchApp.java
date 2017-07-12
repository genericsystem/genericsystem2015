package org.genericsystem.cv.watch;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.watch.WatchApp.DocumentsList;
import org.genericsystem.cv.watch.WatchApp.HeaderRow;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH2;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH4;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.Monitor;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

@DependsOnModel({ Doc.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ImgFilter.class })
@Children({ EditDocumentZones.class, AppHeader.class, FlexDiv.class/*, Monitor.class*/ })
@Children(path = FlexDiv.class, pos = 2, value = { HeaderRow.class, DocumentsList.class })
@Children(path = AppHeader.class, value = { Logo.class, AppTitleDiv.class })
@Style(path = AppHeader.class, name = "background-color", value = "#00afeb")
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "GS-Watch interface")
public class WatchApp extends RootTagImpl {

	private static final String gsPath = "/gs-cv_model3";
	private static final String docClass = "id-fr-front";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, WatchApp.class, gsPath);
	}
	
	public WatchApp() {
		addPrefixBinding(context -> getAdminModeProperty(context).setValue(true));
	}

	@Children({ HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class })
	@FlexDirectionStyle(FlexDirection.ROW)
	@Attribute(path = HtmlLabel.class, name = "name", value = "title")
	@Style(name = "margin", value = "0.5em")
	@Style(path = HtmlLabel.class, name = "justify-content", value = "center")
	@Style(path = HtmlLabel.class, name = "justify-content", value = "center")
	@Style(path = HtmlLabel.class, name = "align-items", value = "center")
	@Style(path = HtmlLabel.class, name = "flex", value = "1")
	@Style(path = HtmlLabel.class, pos = 0, name = "flex", value = "3")
	@Style(path = HtmlLabel.class, name = "text-align", value = "center")
	@Style(path = HtmlLabel.class, name = "font-weight", value = "bold")
	@SetText(path = HtmlLabel.class, value = { "Document name", "De-zoned", "OCR'd", "Supervised", "Edit" })
	public static class HeaderRow extends FlexDiv {
		
	}
	
	@ForEach(DOC_CLASS_SELECTOR.class)
	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ DocumentName.class, FlexDiv.class, FlexDiv.class, FlexDiv.class, DocumentEditButtonDiv.class })
	@Children(path = FlexDiv.class, pos = 1, value = { CheckedImage.class, FailedImage.class })
	@Children(path = FlexDiv.class, pos = 2, value = { CheckedImage.class, FailedImage.class })
	@Children(path = FlexDiv.class, pos = 3, value = { CheckedImage.class, FailedImage.class })
	@Switch(path = { FlexDiv.class, CheckedImage.class }, pos = { 1, 0 }, value = DOC_DEZONED.class)
	@Switch(path = { FlexDiv.class, FailedImage.class }, pos = { 1, 0 }, value = DOC_NOT_DEZONED.class)
	@Switch(path = { FlexDiv.class, CheckedImage.class }, pos = { 2, 0 }, value = DOC_OCRD.class)
	@Switch(path = { FlexDiv.class, FailedImage.class }, pos = { 2, 0 }, value = DOC_NOT_OCRD.class)
	@Switch(path = { FlexDiv.class, CheckedImage.class }, pos = { 3, 0 }, value = DOC_SUPERVISED.class)
	@Switch(path = { FlexDiv.class, FailedImage.class }, pos = { 3, 0 }, value = DOC_NOT_SUPERVISED.class)
	@StyleClass(path = { FlexDiv.class, HtmlImg.class }, value = "img")
	@Style(name = "margin", value = "0.5em")
	@Style(path = FlexDiv.class, name = "flex", value = "1")
	@Style(path = FlexDiv.class, name = "justify-content", value = "center")
	@Style(path = FlexDiv.class, name = "align-items", value = "center")
	public static class DocumentsList extends FlexDiv {

	}
	
	@StyleClass("img")
	@Attribute(name = "src", value = "checked.png")
	public static class CheckedImage extends HtmlImg {
		
	}
	
	@StyleClass("img")
	@Attribute(name = "src", value = "failed.png")
	public static class FailedImage extends HtmlImg {
		
	}

	@BindText
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "3")
	public static class DocumentName extends FlexDiv {

	}

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "1 0 auto")
	@Children(DocumentEditButton.class)
	public static class DocumentEditButtonDiv extends FlexDiv {
		
	}
	
	@Children(HtmlHyperLink.class)
	@Children(path = HtmlHyperLink.class, value = HtmlImg.class)
	@Attribute(path = { HtmlHyperLink.class, HtmlImg.class }, pos = { 0, 0 }, name = "src", value = "edit.png")
	@StyleClass(path = { HtmlHyperLink.class, HtmlImg.class }, pos = { -1, 0 }, value = "img")
	@BindAction(SET_SELECTION.class)
	public static class DocumentEditButton extends FlexDiv {
		
	}

	public static class DOC_CLASS_SELECTOR implements ObservableListExtractor {
		@Override
		public ObservableList<Generic> apply(Generic[] generics) {
			Root root = generics[0].getRoot();
			Generic currentDocClass = root.find(DocClass.class).getInstance(docClass);
			System.out.println("Current doc class : " + currentDocClass.info());
			Snapshot<Generic> docInstances = currentDocClass.getHolders(root.find(Doc.class));
			return docInstances.toObservableList();
		}
	}
	
	public static class DOC_DEZONED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isClassZoneFilePresent(context);
		}
	}
	
	public static class DOC_NOT_DEZONED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isClassZoneFilePresent(context).not();
		}
	}
	
	public static class DOC_OCRD implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocOcrd(context);
		}
	}
	
	public static class DOC_NOT_OCRD implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocOcrd(context).not();
		}
	}
	
	public static class DOC_SUPERVISED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocSupervised(context);
		}
	}
	
	public static class DOC_NOT_SUPERVISED implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			return isDocSupervised(context).not();
		}
	}
	
	public static SimpleBooleanProperty isClassZoneFilePresent (Context context) {
		DocInstance currentDoc = (DocInstance) context.getGeneric();
		DocClassInstance docClassInstance = currentDoc.getDocClass();
		File file = new File(System.getProperty("user.dir") + "/../gs-cv/classes/" + docClassInstance.getValue().toString() + "/zones/zones.json");
		return new SimpleBooleanProperty(file.exists());
	}
	
	public static SimpleBooleanProperty isDocOcrd (Context context) {
		// TODO: implement
		return new SimpleBooleanProperty(true);
	}
	
	public static SimpleBooleanProperty isDocSupervised (Context context) {
		// TODO: implement
		return new SimpleBooleanProperty(true);
	}

}
