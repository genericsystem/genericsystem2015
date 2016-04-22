package org.genericsystem.distributed.cacheonserver.ui.exemple;

import io.vertx.core.http.ServerWebSocket;
import javafx.collections.FXCollections;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Car;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.CarColor;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Power;
import org.genericsystem.distributed.cacheonserver.ui.list.TypeListHtml;
import org.genericsystem.distributed.cacheonserver.ui.list.TypeListHtml.TitleTypeListHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.title.TitleTypeTableHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.title.insertable.InsertTitleTypeTableHtml;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.distributed.ui.components.HtmlDiv;
import org.genericsystem.kernel.Engine;

public class AppHtml extends HtmlApp {

	public AppHtml(Engine engine, ServerWebSocket webSocket) {
		super(new AppModel(engine, engine.find(Car.class), FXCollections.observableArrayList(engine.find(Power.class), engine.find(CarColor.class))), webSocket);
		Generic car = engine.find(Car.class);
		Generic power = engine.find(Power.class);
		car.setInstance("Audi S4").setHolder(power, 333);
		car.setInstance("BMW M3").setHolder(power, 450);
		car.setInstance("Ferrari F40").setHolder(power, 478);
		car.setInstance("Mini Cooper").setHolder(power, 175);
		car.setInstance("Audi A4 3.0 TDI").setHolder(power, 233);
		car.setInstance("Peugeot 106 GTI").setHolder(power, 120);
		car.setInstance("Peugeot 206 S16").setHolder(power, 136);
	}

	@Override
	protected void initChildren() {
		HtmlDiv div = new HtmlDiv(this).addStyleClass("gsapp");
		{
			new AppHeaderHtml(div);
			new TypeListHtml(div).select(AppModel::getTypeListModel);
			new TitleTypeListHtml(div).select(AppModel::getTitleTypeListModel);
			new TypeTableHtml(div).select(AppModel::getTypeTableModel);
			new TitleTypeTableHtml(div).select(AppModel::getTitleTypeTableModel);
			new InsertTitleTypeTableHtml(div).select(AppModel::getInsertableTitleTypeTableModel);
			new InsertTitleTypeTableHtml(div).select(AppModel::getColorsInsertableTitleTypeTableModel);
			new AppFooterHtml(div);
		}
	}
}
