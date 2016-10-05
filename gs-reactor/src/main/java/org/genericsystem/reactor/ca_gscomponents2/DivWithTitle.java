package org.genericsystem.reactor.ca_gscomponents2;

import org.genericsystem.reactor.aa_modelproperties.SelectionDefaults;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.SetStringExtractor;
import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Color;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.ba_htmltag.HtmlH2;
import org.genericsystem.reactor.ca_gscomponents.FlexDirection;
import org.genericsystem.reactor.ca_gscomponents.GSDiv;
import org.genericsystem.reactor.ca_gscomponents2.DivWithTitle.GSTitleDiv;
import org.genericsystem.reactor.ca_gscomponents2.DivWithTitle.GSTitleDiv.TitleContent;
import org.genericsystem.reactor.model.StringExtractor;

@Style(name = "margin", value = "4px")
@FlexDirectionStyle(FlexDirection.COLUMN)
@ReactorDependencies(GSTitleDiv.class)
public class DivWithTitle extends GSDiv {

	@BackgroundColor("#EA4500")
	@MarginRight("1px")
	@MarginBottom("1px")
	@Color("White")
	@JustifyContent("center")
	@AlignItems("center")
	@ReactorDependencies(TitleContent.class)
	public static class GSTitleDiv extends GSDiv {
		public static class TitleContent extends HtmlH2 {
			@Override
			public void init() {
				bindText();
			}
		}
	}

	@ReactorDependencies({ GSTitleDiv.class, InstancesTable.class })
	@SetStringExtractor(path = GSTitleDiv.class, value = StringExtractor.MANAGEMENT.class)
	public static class TitledInstancesTable extends DivWithTitle {

	}

	@ReactorDependencies({ GSTitleDiv.class, InstanceEditor.class })
	@SetStringExtractor(path = GSTitleDiv.class, value = StringExtractor.TYPE_INSTANCE_EXTRACTOR.class)
	public static class TitledInstanceEditor extends DivWithTitle implements SelectionDefaults {

	}
}
