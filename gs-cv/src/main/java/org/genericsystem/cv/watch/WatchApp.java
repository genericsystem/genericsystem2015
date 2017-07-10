package org.genericsystem.cv.watch;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
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
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.context.ObservableListExtractor;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH2;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.Modal.ModalWithDisplay;
import org.genericsystem.reactor.gscomponents.Monitor;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import javafx.collections.ObservableList;

@DependsOnModel({ Doc.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ImgFilter.class })
@Children({ EditDocumentZones.class, AppHeader.class, FlexDiv.class, Monitor.class })
@Children(path = FlexDiv.class, pos = 2, value = { HeaderRow.class, DocumentsList.class })
@Children(path = AppHeader.class, value = { Logo.class, AppTitleDiv.class })
@Style(path = AppHeader.class, name = "background-color", value = "#00afeb")
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "GS-Watch interface")
public class WatchApp extends RootTagImpl {

	private static final String gsPath = "/gs-cv_model";
	private static final String docClass = "id-fr-front";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, WatchApp.class, gsPath);
	}
	
	public WatchApp() {
		addPrefixBinding(context -> getAdminModeProperty(context).setValue(true));
	}
	
	@Children({ FlexDiv.class, FlexDiv.class })
	@Children(path = FlexDiv.class, value = HtmlH2.class)
	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(path = FlexDiv.class, name = "justify-content", value = "center")
	@Style(path = FlexDiv.class, name = "align-items", value = "center")
	@Style(path = FlexDiv.class, name = "flex", value = "1 0 auto")
	@Style(path = { FlexDiv.class, HtmlH2.class }, name = "justify-content", value = "center")
	@Style(path = { FlexDiv.class, HtmlH2.class }, name = "align-items", value = "center")
	@Style(path = { FlexDiv.class, HtmlH2.class }, name = "flex", value = "1 0 auto")
	@SetText(path = { FlexDiv.class, HtmlH2.class }, pos = { 0, -1 }, value = "Nom")
	@SetText(path = { FlexDiv.class, HtmlH2.class }, pos = { 1, -1 }, value = "Options")
	public static class HeaderRow extends FlexDiv {
		
	}

	@ForEach(DOC_CLASS_SELECTOR.class)
	@FlexDirectionStyle(FlexDirection.ROW)
	@Style(name = "margin", value = "0.5em")
	@Children({ DocumentName.class, DocumentEditButtonDiv.class })
	public static class DocumentsList extends FlexDiv {

	}

	@BindText
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "1 0 auto")
	public static class DocumentName extends FlexDiv {

	}

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "1 0 auto")
	@Children({ /*EditDocumentZones.class,*/ DocumentEditButton.class })
	public static class DocumentEditButtonDiv extends FlexDiv {
		
	}
	
	@Children(HtmlHyperLink.class)
	@Children(path = HtmlHyperLink.class, value = HtmlImg.class)
	@Attribute(path = { HtmlHyperLink.class, HtmlImg.class }, pos = { 0, 0 }, name = "src", value = "edit.png")
	@StyleClass(path = { HtmlHyperLink.class, HtmlImg.class }, pos = { -1, 0 }, value = "img")
	@BindAction(SET_SELECTION.class)
	public static class DocumentEditButton extends DocumentEditButtonDiv {
		
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

}
