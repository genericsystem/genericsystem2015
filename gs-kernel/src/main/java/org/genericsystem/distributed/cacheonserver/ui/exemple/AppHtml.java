package org.genericsystem.distributed.cacheonserver.ui.exemple;

import io.vertx.core.http.ServerWebSocket;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Car;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.CarColor;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Color;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Power;
import org.genericsystem.distributed.cacheonserver.ui.list.TypeSectionHtml.TitleTypeListHtml;
import org.genericsystem.distributed.cacheonserver.ui.list.TypeSelectHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableHtml;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.distributed.ui.components.HtmlDiv;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;
import org.genericsystem.kernel.Engine;

public class AppHtml extends HtmlApp<AppModel> {

	private final ObservableList<Generic> attributes;

	public AppHtml(Engine engine, ServerWebSocket webSocket) {
		super(new AppModel(engine, engine.find(Car.class)), webSocket);
		attributes = FXCollections.observableArrayList(engine.find(Power.class), engine.find(CarColor.class));
		Generic car = engine.find(Car.class);
		Generic power = engine.find(Power.class);
		Generic carColor = engine.find(CarColor.class);
		Generic color = engine.find(Color.class);
		Generic red = color.setInstance("Red");
		Generic black = color.setInstance("Black");
		Generic green = color.setInstance("Green");
		color.setInstance("Blue");
		color.setInstance("Orange");
		color.setInstance("White");
		color.setInstance("Yellow");
		Generic audiS4 = car.setInstance("Audi S4");
		audiS4.setHolder(power, 333);
		audiS4.setLink(carColor, "Audi S4 Green", green);
		Generic bmwM3 = car.setInstance("BMW M3");
		bmwM3.setHolder(power, 450);
		bmwM3.setLink(carColor, "BMW M3 Red", red);
		Generic ferrariF40 = car.setInstance("Ferrari F40");
		ferrariF40.setHolder(power, 478);
		ferrariF40.setLink(carColor, "Ferrari F40 red", red);
		Generic miniCooper = car.setInstance("Mini Cooper");
		miniCooper.setHolder(power, 175);
		miniCooper.setLink(carColor, "Mini Cooper", black);
		car.setInstance("Audi A4 3.0 TDI").setHolder(power, 233);
		car.setInstance("Peugeot 106 GTI").setHolder(power, 120);
		car.setInstance("Peugeot 206 S16").setHolder(power, 136);
	}

	@Override
	protected void initChildren() {
		HtmlDiv<AppModel> div = new HtmlDiv<AppModel>(this).addStyleClass("gsapp");
		{
			new AppHeaderHtml(div);
			new TypeSelectHtml<>(div).select(AppModel::getTypeListModel);
			new TitleTypeListHtml<>(div).select(AppModel::getTitleTypeListModel);
			new TypeTableHtml<>(div, g -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g) + "(s) Management").setAttributesExtractor(type -> attributes).select(AppModel::getTypeTableModel);
			// new TitleTypeTableHtml<>(div).select(AppModel::getTitleTypeTableModel);
			// new InsertTitleTypeTableHtml<>(div).select(AppModel::getInsertableTitleTypeTableModel);
			// new InsertTitleTypeTableHtml<>(div).select(AppModel::getColorsInsertableTitleTypeTableModel);
			new AppFooterHtml(div);
		}
	}
}
