package org.genericsystem.reactor.az3;

import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Styles.AlignItems;
import org.genericsystem.reactor.annotations.Styles.BackgroundColor;
import org.genericsystem.reactor.annotations.Styles.Color;
import org.genericsystem.reactor.annotations.Styles.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Styles.JustifyContent;
import org.genericsystem.reactor.annotations.Styles.MarginBottom;
import org.genericsystem.reactor.annotations.Styles.MarginRight;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.az.FlexDirection;
import org.genericsystem.reactor.az.GSDiv;
import org.genericsystem.reactor.az3.Table.InstancesTable;
import org.genericsystem.reactor.az3.TitledDiv.GSTitleDiv;
import org.genericsystem.reactor.az3.TitledDiv.GSTitleDiv.TitleContent;
import org.genericsystem.reactor.gstag.HtmlH2;
import org.genericsystem.reactor.model.StringExtractor;

@FlexDirectionStyle(FlexDirection.COLUMN)
@ReactorDependencies(GSTitleDiv.class)
public class TitledDiv extends GSDiv {

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
				setStringExtractor(StringExtractor.MANAGEMENT);
				bindText();
			}
		}
	}

	@Style(name = "margin", value = "4px")
	@ReactorDependencies({ GSTitleDiv.class, InstancesTable.class })
	public static class TitledInstancesTable extends TitledDiv {

	}

}
