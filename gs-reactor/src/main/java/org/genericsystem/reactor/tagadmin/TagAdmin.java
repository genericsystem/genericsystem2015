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
import org.genericsystem.reactor.extended.ExtendedRootTag;
import org.genericsystem.reactor.extended.ExtendedRootTag.GTag;
import org.genericsystem.reactor.extended.ExtendedRootTag.TagType;
import org.genericsystem.reactor.extended.ExtendedRootTag.TagType.TagAnnotationAttribute;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitledInstancesTable;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;
import org.genericsystem.reactor.gscomponents.Monitor.MonitorLogin;
import org.genericsystem.reactor.gscomponents.Responsive;
import org.genericsystem.reactor.tagadmin.TagAdmin.TagScript;
import org.genericsystem.security.model.User;

/**
 * @author Nicolas Feybesse
 *
 */
@DependsOnModel({ TagType.class, GTag.class, User.class })
@RunScript(TagScript.class)
@Style(name = "background-color", value = "#00afeb")
@Children({ ModalEditor.class, AppHeader.class, Responsive.class, MonitorLogin.class })
@Children(path = Responsive.class, value = { TitledInstancesTable.class, TitledInstancesTable.class, TitledInstancesTable.class, SwitchDivContainer.class })
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "App administration")
@DirectSelect(path = { Responsive.class, TitledInstancesTable.class }, value = { TagType.class, TagAnnotationAttribute.class, User.class })
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
