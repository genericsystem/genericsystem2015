package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlH2;

import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.GSTitleDiv;
import org.genericsystem.reactor.gscomponents3.InstanceEditor.HorizontalInstanceEditor;
import org.genericsystem.reactor.gscomponents3.InstanceStepEditor.HorizontalInstanceStepEditor;
import org.genericsystem.reactor.gscomponents3.InstancesTable.HorizontalInstancesTable;
import org.genericsystem.reactor.model.StringExtractor;

@Style(name = "flex", value = "1")
@Style(name = "margin", value = "4px")
@FlexDirectionStyle(FlexDirection.COLUMN)
@Children(GSTitleDiv.class)
public class DivWithTitle extends GSDiv {

	@Style(name = "background-color", value = "#EA4500")
	@Style(name = "margin-right", value = "1px")
	@Style(name = "margin-bottom", value = "1px")
	@Style(name = "color", value = "White")
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Children(HtmlH2.class)
	@BindText(path = HtmlH2.class)
	public static class GSTitleDiv extends GSDiv {
	}

	@Children({ GSTitleDiv.class, InstancesTable.class })
	@SetStringExtractor(path = GSTitleDiv.class, value = StringExtractor.MANAGEMENT.class)
	public static class TitledInstancesTable extends DivWithTitle {
	}

	@Children({ GSTitleDiv.class, HorizontalInstancesTable.class })
	public static class TitledHorizontalInstancesTable extends TitledInstancesTable {
	}

	@Children({ GSTitleDiv.class, InstanceEditor.class })
	@SetStringExtractor(path = GSTitleDiv.class, value = StringExtractor.TYPE_INSTANCE_EXTRACTOR.class)
	public static class TitledInstanceEditor extends DivWithTitle implements SelectionDefaults {
	}

	@Children({ GSTitleDiv.class, HorizontalInstanceEditor.class })
	public static class TitledHorizontalInstanceEditor extends TitledInstanceEditor {
	}

	@Children({ GSTitleDiv.class, InstanceStepEditor.class })
	public static class TitledInstanceStepEditor extends TitledInstanceEditor {
	}

	@Children({ GSTitleDiv.class, HorizontalInstanceStepEditor.class })
	public static class TitledHorizontalInstanceStepEditor extends TitledInstanceStepEditor {
	}
}
