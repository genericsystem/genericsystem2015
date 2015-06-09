package org.genericsystem.javafx;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import javafx.collections.ObservableList;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.util.StringConverter;

import org.genericsystem.admin.UiFunctions;
import org.genericsystem.admin.UiFunctions.AttributeUiFunctions;
import org.genericsystem.mutability.Generic;

/**
 * @author Nicolas Feybesse
 *
 * @param <G>
 */
public class AddContextMenu<G> extends ContextMenu {

	public AddContextMenu(G base, G attribute, AttributeUiFunctions<G> attFunctions, int axe, Supplier<ObservableList<G>> items, ContextMenu aboveMenu) {
		String text = "Add New " + attribute.toString() + " on : " + base.toString();
		final MenuItem addItem = new MenuItem(text, new ImageView(new Image(getClass().getResourceAsStream("ok.png"))));
		addItem.setOnAction(e -> new AddDialog<G>(base, attribute, attFunctions.converter, axe, items.get(), text, attFunctions.genericComponents, attFunctions.genericSubInstances, attFunctions.addAction).showAndWait());
		getItems().add(addItem);
		aboveMenu.getItems().forEach(item -> {
			MenuItem newItem = new MenuItem(item.getText(), new ImageView(((ImageView) item.getGraphic()).getImage()));
			getItems().add(newItem);
			newItem.onActionProperty().bind(item.onActionProperty());
		});
	}

	public AddContextMenu(G type, UiFunctions<G> gsFunctions, Supplier<ObservableList<G>> items) {
		String text = "Add New " + type.toString();
		final MenuItem addItem = new MenuItem(text, new ImageView(new Image(getClass().getResourceAsStream("ok.png"))));
		addItem.setOnAction(e -> new AddDialog<>(null, type, gsFunctions.attributeConverter.apply(type), -1, items.get(), text, gsFunctions.genericComponents, gsFunctions.genericSubInstances, gsFunctions.attributeAddAction.apply(type)).showAndWait());
		getItems().add(addItem);
	}

	@FunctionalInterface
	public interface TriFunction<T, U, V, R> {
		R apply(T t, U u, V v);
	}

	public static class AddDialog<G> extends Dialog<G> {

		public AddDialog(G base, G type, StringConverter<Serializable> converter, int axe, ObservableList<G> tableItems, String headerText, Function<G, List<G>> typeComponents, Function<G, ObservableList<G>> typeComponentSubInstances,
				BiFunction<Serializable, List<G>, G> addAction) {

			setTitle("Add an instance");
			setHeaderText(headerText);
			setResizable(true);

			GridPane grid = new GridPane();
			Label valueLabel = new Label("Value : ");
			TextField valueField = new TextField();
			grid.add(valueLabel, 0, 0);
			grid.add(valueField, 1, 0);
			boolean visibility = ((Generic) type).getInstanceValueGenerator() == null;
			valueLabel.setVisible(visibility);
			valueField.setVisible(visibility);
			int i = 0;
			List<ComboBox<G>> combos = new ArrayList<>();
			for (G typeComponent : typeComponents.apply(type)) {
				Label label = new Label("" + typeComponent);
				ComboBox<G> combo = new ComboBox<>();
				combo.setItems(typeComponentSubInstances.apply(typeComponent));
				combos.add(combo);
				if (i == axe) {
					combo.getSelectionModel().select(base);
					combo.setDisable(true);
				}
				grid.add(label, 0, i + 1);
				grid.add(combo, 1, i + 1);
				i++;
			}
			getDialogPane().setContent(grid);
			getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
			setResultConverter(buttonType -> {
				if (buttonType == ButtonType.OK) {
					List<G> components = combos.stream().map(combo -> combo.getSelectionModel().getSelectedItem()).collect(Collectors.toList());
					G generic = addAction.apply(converter.fromString(valueField.getText()), components);
					tableItems.add(generic);
					return generic;
				}
				return null;
			});
		}

	}
}