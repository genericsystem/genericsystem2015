package org.genericsystem.distributed.cacheonserver.ui.exemple;

import io.vertx.core.http.ServerWebSocket;

import java.util.Arrays;
import java.util.stream.Collectors;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Car;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.CarColor;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Color;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Power;
import org.genericsystem.distributed.cacheonserver.ui.list.GSCompositeHtml.GSTitleCompositeHtml;
import org.genericsystem.distributed.cacheonserver.ui.list.GSSelectHtml;
import org.genericsystem.distributed.cacheonserver.ui.table.TypeTableHtml;
import org.genericsystem.distributed.ui.CompositeModel;
import org.genericsystem.distributed.ui.CompositeModel.StringExtractor;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.distributed.ui.components.HtmlDiv;
import org.genericsystem.kernel.Engine;

public class AppHtml extends HtmlApp<AppModel> {

	private final Generic car;
	private final Engine engine;
	private final ObservableList<Generic> attributes;

	public AppHtml(Engine engine, ServerWebSocket webSocket) {
		super(new AppModel(engine), webSocket);
		this.engine = engine;
		car = engine.find(Car.class);
		attributes = FXCollections.observableArrayList(Arrays.asList(Power.class, CarColor.class).stream().map(engine::<Generic> find).collect(Collectors.toList()));
		runScript(engine);
	}

	@Override
	protected void initChildren() {
		HtmlDiv<AppModel> div = new HtmlDiv<AppModel>(this).addStyleClass("gsapp");
		{
			new AppHeaderHtml(div);
			new GSSelectHtml<>(div).select(AppModel::getTypeListModel, StringExtractor.MANAGEMENT, () -> engine);
			new GSTitleCompositeHtml<>(div).select(AppModel::getTitleTypeListModel, StringExtractor.MANAGEMENT, () -> car);
			((TypeTableHtml<CompositeModel>) new TypeTableHtml<CompositeModel>(div).select(AppModel::getTypeTableModel, StringExtractor.MANAGEMENT, () -> car)).setAttributesExtractor(instance -> attributes);
			// new TitleTypeTableHtml<>(div).select(AppModel::getTitleTypeTableModel);
			// new InsertTitleTypeTableHtml<>(div).select(AppModel::getInsertableTitleTypeTableModel);
			// new InsertTitleTypeTableHtml<>(div).select(AppModel::getColorsInsertableTitleTypeTableModel);
			new AppFooterHtml(div);
		}
	}

	void runScript(Engine engine) {
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
}
