package org.genericsystem.spring;

import java.util.ArrayList;
import java.util.List;

import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

//@ApplicationScoped
@Component
@Lazy
public class UserClassesProvider {

	List<Class<?>> userClasses = new ArrayList<>();

	public void addUserClasse(Class<?> userClasse) {
		userClasses.add(userClasse);
	}

	public Class<?>[] getUserClassesArray() {
		return userClasses.toArray(new Class[userClasses.size()]);
	}
}
