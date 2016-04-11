package org.genericsystem.distributed.cacheonserver.todocarmvc;

import io.vertx.core.http.ServerWebSocket;

import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.distributed.ui.components.HtmlButton;
import org.genericsystem.distributed.ui.components.HtmlCheckBox;
import org.genericsystem.distributed.ui.components.HtmlDiv;
import org.genericsystem.distributed.ui.components.HtmlFooter;
import org.genericsystem.distributed.ui.components.HtmlH1;
import org.genericsystem.distributed.ui.components.HtmlHeader;
import org.genericsystem.distributed.ui.components.HtmlHyperLink;
import org.genericsystem.distributed.ui.components.HtmlInputText;
import org.genericsystem.distributed.ui.components.HtmlLabel;
import org.genericsystem.distributed.ui.components.HtmlLi;
import org.genericsystem.distributed.ui.components.HtmlSection;
import org.genericsystem.distributed.ui.components.HtmlSpan;
import org.genericsystem.distributed.ui.components.HtmlStrong;
import org.genericsystem.distributed.ui.components.HtmlUl;
import org.genericsystem.kernel.Engine;

public class CarApp extends HtmlApp {

	public CarApp(Engine engine, ServerWebSocket webSocket) {
		super(new CarListModel(engine), webSocket);

	}

	@Override
	protected void initChildren() {

		HtmlDiv div = new HtmlDiv(this);
		{
			HtmlSection carapp = new HtmlSection(div).setStyleClass("todoapp");
			{
				HtmlHeader header = new HtmlHeader(carapp).setStyleClass("header");
				{
					new HtmlH1(header).setText("MyCars");
					new HtmlInputText(header).setStyleClass("new-todo").bindAction(CarListModel::create).bindTextBidirectional(CarListModel::getName);
				}
				HtmlSection main = new HtmlSection(carapp).setStyleClass("main");
				{
					HtmlUl carlist = new HtmlUl(main).setStyleClass("todo-list");

					HtmlLi li = new HtmlLi(carlist).forEach(CarListModel::getFiltered).bindOptionalStyleClass(CarModel::getCompleted, "completed");
					{
						HtmlDiv todoDiv = new HtmlDiv(li).setStyleClass("view");
						{
							new HtmlCheckBox(todoDiv).setStyleClass("toggle").bindCheckedBidirectional(CarModel::getCompleted);
							new HtmlLabel(todoDiv).bindText(CarModel::getTodoString);
							new HtmlButton(todoDiv).setStyleClass("destroy").bindAction(CarModel::remove);
						}
					}
				}
			}
			HtmlFooter footer = new HtmlFooter(carapp).setStyleClass("footer").bindOptionalStyleClass(CarListModel::getHasNoCarModel, "hide");
			{
				HtmlSpan span = new HtmlSpan(footer).setStyleClass("todo-count");
				{
					new HtmlStrong(span).bindText(CarListModel::getActiveCount);
					new HtmlSpan(span).bindText(CarListModel::getItems);
				}

				HtmlUl filters = new HtmlUl(footer).setStyleClass("filters");
				{
					new HtmlHyperLink(new HtmlLi(filters), "All", CarListModel::showAll).bindOptionalStyleClass(CarListModel::getAllMode, "selected");
					new HtmlHyperLink(new HtmlLi(filters), "Actives", CarListModel::showActive).bindOptionalStyleClass(CarListModel::getActiveMode, "selected");
					new HtmlHyperLink(new HtmlLi(filters), "Completes", CarListModel::showCompleted).bindOptionalStyleClass(CarListModel::getCompletedMode, "selected");
				}
				new HtmlButton(footer).bindAction(CarListModel::removeCompleted).bindText(CarListModel::getClearCompleted).setStyleClass("clear-completed").bindOptionalStyleClass(CarListModel::getHasNoCompleted, "hide");

			}
		}
	}
}
