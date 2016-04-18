package org.genericsystem.distributed.cacheonserver.ui.exemple;

import io.vertx.core.http.ServerWebSocket;
import javafx.collections.FXCollections;

import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Car;
import org.genericsystem.distributed.cacheonserver.ui.exemple.model.Power;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.distributed.ui.components.HtmlDiv;
import org.genericsystem.kernel.Engine;

public class AppHtml extends HtmlApp {

	public AppHtml(Engine engine, ServerWebSocket webSocket) {
		super(new AppModel(engine, engine.find(Car.class), FXCollections.observableArrayList(engine.find(Power.class))), webSocket);
	}

	@Override
	protected void initChildren() {

		HtmlDiv div = new HtmlDiv(this);
		{
			new AppHeaderHtml(this);
			// HtmlSection carapp = new HtmlSection(div).setStyleClass("todoapp");
			// {
			// HtmlHeader header = new HtmlHeader(carapp).setStyleClass("header");
			// {
			// new HtmlH1(header).setText("Cars");
			// new HtmlInputText(header).setStyleClass("new-todo").bindTextBidirectional(AppModel::getCarString);
			// new HtmlInputText(header).setStyleClass("new-todo").bindAction(AppModel::create).bindTextBidirectional(AppModel::getPowerString);
			//
			// }
			// }

			new TypeTableHtml(div).select(AppModel::getTypeModel);

			// HtmlFooter footer = new HtmlFooter(carapp).setStyleClass("footer");
			// {
			// HtmlSpan span = new HtmlSpan(footer).setStyleClass("todo-count");
			// {
			// new HtmlStrong(span);
			// new HtmlSpan(span);
			// }
			//
			// HtmlUl filters = new HtmlUl(footer).setStyleClass("filters");
			// {
			// new HtmlHyperLink(new HtmlLi(filters), "Flush", AppModel::flush);
			// new HtmlHyperLink(new HtmlLi(filters), "Cancel", AppModel::cancel);
			// }
			//
			// }
		}
	}
}
