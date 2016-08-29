package org.genericsystem.defaults.tools;

import javafx.beans.binding.Binding;

/**
 * @author Nicolas Feybesse
 *
 */
public class BindingsTools {
	public static <U extends Binding<V>, V> U transmitSuccessiveInvalidations(U binding) {
		binding.addListener((o, v, nv) -> {
		});
		return binding;
	}
}
