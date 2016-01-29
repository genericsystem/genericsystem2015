package org.genericsystem.gsadmin;

import javafx.scene.Group;
import javafx.scene.control.ScrollPane;

import org.genericsystem.gsadmin.GSCrud.GSEngineCrud;
import org.genericsystem.ui.Model;
import org.genericsystem.ui.components.GSApplication;
import org.genericsystem.ui.components.GSSCrollPane;
import org.genericsystem.ui.components.GSVBox;

public class GSAdmin extends GSApplication {

	public GSAdmin(Model model, Group parentNode) {
		super(model, parentNode);
	}

	@Override
	protected void initChildren() {
		GSVBox mainPanel = new GSVBox(this).setPrefHeight(GenericWindow::getHeight).setPrefWidth(GenericWindow::getWidth);
		{
			GSSCrollPane scrollPane = new GSSCrollPane(mainPanel).setStyleClass("scrollable");
			{
				scrollPane.addBoot(ScrollPane::fitToWidthProperty, true);
				scrollPane.addBoot(ScrollPane::fitToHeightProperty, true);

				GSVBox leftTables = new GSVBox(scrollPane).setMinHeight(GenericWindow::getContentHeight).setMinWidth(GenericWindow::getContentWidth);
				{
					new GSEngineCrud(leftTables).select(GenericWindow::getFirstCrud);

					new GSCrud(leftTables).select(GenericWindow::getSecondCrud);

					// GSHBox commandPanel = new GSHBox(leftTables).setSpacing(5);
					// {
					// new GSButton(commandPanel, "Flush", GenericWindow::flush);
					// new GSButton(commandPanel, "Cancel", GenericWindow::cancel);
					// new GSButton(commandPanel, "Mount", GenericWindow::mount);
					// new GSButton(commandPanel, "Unmount", GenericWindow::unmount);
					// new GSButton(commandPanel, "ShiftTs", GenericWindow::shiftTs);
					// new GSLabel(commandPanel, GenericWindow::getCacheLevel);// .setObservableTextProperty(GenericWindow::getCacheLevel);
					// }
				}
			}
		}
	}
}
