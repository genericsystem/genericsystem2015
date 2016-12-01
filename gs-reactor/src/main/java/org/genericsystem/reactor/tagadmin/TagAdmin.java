package org.genericsystem.reactor.tagadmin;

import org.genericsystem.common.Root;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.AnnotationContent;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.TagAnnotationGeneric;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;
import org.genericsystem.reactor.gscomponents.Monitor;
import org.genericsystem.reactor.gscomponents.Responsive;
import org.genericsystem.reactor.tagadmin.TagAdmin.TagScript;

/**
 * @author Nicolas Feybesse
 *
 */
@DependsOnModel({ GTagType.class, GTag.class })
@RunScript(TagScript.class)
@Style(name = "background-color", value = "#00afeb")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, Monitor.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, TitledInstancesTable.class, TitledInstancesTable.class })
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "App administration")
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { GTagType.class, TagAnnotationGeneric.class, AnnotationContent.class })
public class TagAdmin extends ExtendedRootTag {
	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, TagAdmin.class, "tags");
	}

	public TagAdmin(Root engine) {
		super(engine);
		addPrefixBinding(context -> getAdminModeProperty(context).setValue(true));
	}

	public static class TagScript implements Script {

		@Override
		public void run(Root engine) {
			engine.getCurrentCache().flush();
		}
	}
}
