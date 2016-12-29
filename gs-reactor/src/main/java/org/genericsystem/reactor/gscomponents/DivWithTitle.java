package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.DivWithTitle.TitleDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH2;
import org.genericsystem.reactor.gscomponents.InstanceEditor.HorizontalInstanceEditor;
import org.genericsystem.reactor.gscomponents.InstanceStepEditor.HorizontalInstanceStepEditor;
import org.genericsystem.reactor.gscomponents.InstancesTable.HorizontalInstancesTable;

//Apple
//@StyleClass("divWithTitle")
@Style(name = "flex", value = "1")
@Style(name = "margin", value = "4px")
@FlexDirectionStyle(FlexDirection.COLUMN)
@Children(TitleDiv.class)
public class DivWithTitle extends FlexDiv {

	@Style(name = "background-color", value = "#EA4500")
	@Style(name = "color", value = "White")
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Children(HtmlH2.class)
	@BindText(path = HtmlH2.class)
	public static class TitleDiv extends FlexDiv {
	}

	@Children({ TitleDiv.class, InstancesTable.class })
	@SetStringExtractor(path = { TitleDiv.class, HtmlH2.class }, value = StringExtractor.MANAGEMENT.class)
	public static class TitledInstancesTable extends DivWithTitle {
	}

	@Children({ TitleDiv.class, HorizontalInstancesTable.class })
	public static class TitledHorizontalInstancesTable extends TitledInstancesTable {
	}

	@Children({ TitleDiv.class, InstanceEditor.class })
	@SetStringExtractor(path = { TitleDiv.class, HtmlH2.class }, value = StringExtractor.TYPE_INSTANCE_EXTRACTOR.class)
	public static class TitledInstanceEditor extends DivWithTitle implements SelectionDefaults {
	}

	@Children({ TitleDiv.class, HorizontalInstanceEditor.class })
	public static class TitledHorizontalInstanceEditor extends TitledInstanceEditor {
	}

	@Children({ TitleDiv.class, InstanceStepEditor.class })
	public static class TitledInstanceStepEditor extends TitledInstanceEditor {
	}

	@Children({ TitleDiv.class, HorizontalInstanceStepEditor.class })
	public static class TitledHorizontalInstanceStepEditor extends TitledInstanceStepEditor {
	}
}
