package org.genericsystem.distributed.cacheonserver.ui.exemple;

import io.vertx.core.http.ServerWebSocket;
import org.genericsystem.distributed.ui.components.HtmlApp;
import org.genericsystem.distributed.ui.components.HtmlButton;
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
					HtmlUl inputs = new HtmlUl(header).setStyleClass("inputs");
					{
						HtmlLi li = new HtmlLi(inputs);
						{
							new HtmlInputText(li).setStyleClass("new-todo").bindTextBidirectional(CarListModel::getCarString);
						}
						HtmlLi li2 = new HtmlLi(inputs);
						{
							new HtmlInputText(li2).setStyleClass("new-todo").bindAction(CarListModel::create).bindTextBidirectional(CarListModel::getPowerString);

						}
					}
				}

				HtmlSection main = new HtmlSection(carapp).setStyleClass("main");
				{
					HtmlUl carlist = new HtmlUl(main).setStyleClass("todo-list");

					HtmlLi li = new HtmlLi(carlist).forEach(CarListModel::getCarModels);
					{
						HtmlDiv carDiv = new HtmlDiv(li).setStyleClass("view");
						{
							// new HtmlCheckBox(todoDiv).setStyleClass("toggle");
							new HtmlLabel(carDiv).bindText(CarModel::getCarString);

							new HtmlLabel(carDiv).bindText(PowerValueModel::getPowerValueString).select(CarModel::getPowerValueModel);

							new HtmlButton(carDiv).setStyleClass("destroy").bindAction(CarModel::remove);
						}
					}
				}

			}
			HtmlFooter footer = new HtmlFooter(carapp).setStyleClass("footer");
			{
				HtmlSpan span = new HtmlSpan(footer).setStyleClass("todo-count");
				{
					new HtmlStrong(span);
					new HtmlSpan(span);
				}

				HtmlUl filters = new HtmlUl(footer).setStyleClass("filters");
				{
					new HtmlHyperLink(new HtmlLi(filters), "Flush", CarListModel::flush);
					new HtmlHyperLink(new HtmlLi(filters), "Cancel", CarListModel::cancel);
				}

			}
		}
	}
}
