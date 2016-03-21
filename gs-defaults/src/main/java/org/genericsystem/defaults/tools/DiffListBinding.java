package org.genericsystem.defaults.tools;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.binding.Binding;
import javafx.beans.binding.ListExpression;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.ReadOnlyBooleanPropertyBase;
import javafx.beans.property.ReadOnlyIntegerProperty;
import javafx.beans.property.ReadOnlyIntegerPropertyBase;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

import com.sun.javafx.binding.BindingHelperObserver;
import com.sun.javafx.binding.ListExpressionHelper;

public abstract class DiffListBinding<E> extends ListExpression<E> implements Binding<ObservableList<E>> {

	private final ListChangeListener<E> listChangeListener = new ListChangeListener<E>() {
		@Override
		public void onChanged(Change<? extends E> change) {
			invalidateProperties();
			onInvalidating();
			ListExpressionHelper.fireValueChangedEvent(helper, change);
		}
	};

	private ObservableList<E> value;
	private boolean valid = false;
	private BindingHelperObserver observer;
	private ListExpressionHelper<E> helper = null;

	private SizeProperty size0;
	private EmptyProperty empty0;

	@Override
	public ReadOnlyIntegerProperty sizeProperty() {
		if (size0 == null) {
			size0 = new SizeProperty();
		}
		return size0;
	}

	private class SizeProperty extends ReadOnlyIntegerPropertyBase {
		@Override
		public int get() {
			return size();
		}

		@Override
		public Object getBean() {
			return DiffListBinding.this;
		}

		@Override
		public String getName() {
			return "size";
		}

		@Override
		protected void fireValueChangedEvent() {
			super.fireValueChangedEvent();
		}
	}

	@Override
	public ReadOnlyBooleanProperty emptyProperty() {
		if (empty0 == null) {
			empty0 = new EmptyProperty();
		}
		return empty0;
	}

	private class EmptyProperty extends ReadOnlyBooleanPropertyBase {

		@Override
		public boolean get() {
			return isEmpty();
		}

		@Override
		public Object getBean() {
			return DiffListBinding.this;
		}

		@Override
		public String getName() {
			return "empty";
		}

		@Override
		protected void fireValueChangedEvent() {
			super.fireValueChangedEvent();
		}
	}

	@Override
	public void addListener(InvalidationListener listener) {
		helper = ListExpressionHelper.addListener(helper, this, listener);
	}

	@Override
	public void removeListener(InvalidationListener listener) {
		helper = ListExpressionHelper.removeListener(helper, listener);
	}

	@Override
	public void addListener(ChangeListener<? super ObservableList<E>> listener) {
		helper = ListExpressionHelper.addListener(helper, this, listener);
	}

	@Override
	public void removeListener(ChangeListener<? super ObservableList<E>> listener) {
		helper = ListExpressionHelper.removeListener(helper, listener);
	}

	@Override
	public void addListener(ListChangeListener<? super E> listener) {
		helper = ListExpressionHelper.addListener(helper, this, listener);
	}

	@Override
	public void removeListener(ListChangeListener<? super E> listener) {
		helper = ListExpressionHelper.removeListener(helper, listener);
	}

	/**
	 * Start observing the dependencies for changes. If the value of one of the dependencies changes, the binding is marked as invalid.
	 *
	 * @param dependencies
	 *            the dependencies to observe
	 */
	protected final void bind(Observable... dependencies) {
		if ((dependencies != null) && (dependencies.length > 0)) {
			if (observer == null) {
				observer = new BindingHelperObserver(this);
			}
			for (final Observable dep : dependencies) {
				if (dep != null) {
					dep.addListener(observer);
				}
			}
		}
	}

	/**
	 * Stop observing the dependencies for changes.
	 *
	 * @param dependencies
	 *            the dependencies to stop observing
	 */
	protected final void unbind(Observable... dependencies) {
		if (observer != null) {
			for (final Observable dep : dependencies) {
				if (dep != null) {
					dep.removeListener(observer);
				}
			}
			observer = null;
		}
	}

	/**
	 * A default implementation of {@code dispose()} that is empty.
	 */
	@Override
	public void dispose() {
	}

	/**
	 * A default implementation of {@code getDependencies()} that returns an empty {@link javafx.collections.ObservableList}.
	 *
	 * @return an empty {@code ObservableList}
	 */
	@Override
	public ObservableList<?> getDependencies() {
		return FXCollections.emptyObservableList();
	}

	/**
	 * Returns the result of {@link #computeValue()}. The method {@code computeValue()} is only called if the binding is invalid. The result is cached and returned if the binding did not become invalid since the last call of {@code get()}.
	 *
	 * @return the current value
	 */
	@Override
	public final ObservableList<E> get() {
		if (!valid) {
			ObservableList<E> oldValue = value;
			value = computeValue();
			valid = true;
			if (value != oldValue) {
				if (oldValue != null) {
					System.out.println("REMOVE LISTENER");
					oldValue.removeListener(listChangeListener);
				}
				if (value != null) {
					System.out.println("ADD LISTENER");
					value.addListener(listChangeListener);
				} else
					throw new IllegalStateException();
			}
		}
		return value;
		// if (!valid) {
		// value = computeValue();
		// valid = true;
		// if (value != null) {
		// value.addListener(listChangeListener);
		// }
		// }
		// return value;
	}

	/**
	 * The method onInvalidating() can be overridden by extending classes to react, if this binding becomes invalid. The default implementation is empty.
	 */
	protected void onInvalidating() {

	}

	private void invalidateProperties() {
		if (size0 != null) {
			size0.fireValueChangedEvent();
		}
		if (empty0 != null) {
			empty0.fireValueChangedEvent();
		}
	}

	@Override
	public final void invalidate() {
		if (valid) {
			// if (value != null) {
			// value.removeListener(listChangeListener);
			// }
			valid = false;
			invalidateProperties();
			onInvalidating();
			ListExpressionHelper.fireValueChangedEvent(helper);
		}
	}

	@Override
	public final boolean isValid() {
		return valid;
	}

	/**
	 * Calculates the current value of this binding.
	 * <p>
	 * Classes extending {@code ListBinding} have to provide an implementation of {@code computeValue}.
	 *
	 * @return the current value
	 */
	protected abstract ObservableList<E> computeValue();

	/**
	 * Returns a string representation of this {@code ListBinding} object.
	 * 
	 * @return a string representation of this {@code ListBinding} object.
	 */
	@Override
	public String toString() {
		return valid ? "ListBinding [value: " + get() + "]" : "ListBinding [invalid]";
	}

}
