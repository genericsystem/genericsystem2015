package org.genericsystem.spring;

import java.io.IOException;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.type.classreading.CachingMetadataReaderFactory;
import org.springframework.core.type.classreading.MetadataReader;
import org.springframework.core.type.classreading.MetadataReaderFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.ClassUtils;
import org.springframework.util.SystemPropertyUtils;

//@ApplicationScoped
@Component
public class StartupBean /* implements Extension */{

	private final Logger log = LoggerFactory.getLogger(StartupBean.class);

	private void findMyTypes(ApplicationContext applicationContext) throws IOException, ClassNotFoundException {

		UserClassesProvider userClasses = applicationContext.getBean(UserClassesProvider.class);

		String basePackage = "org.genericsystem.spring";

		System.out.println("Base Package :" + basePackage);
		ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();
		MetadataReaderFactory metadataReaderFactory = new CachingMetadataReaderFactory(resourcePatternResolver);

		// List<Class> candidates = new ArrayList<Class>();
		String packageSearchPath = ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX + resolveBasePackage(basePackage) + "/" + "**/*.class";
		Resource[] resources = resourcePatternResolver.getResources(packageSearchPath);
		for (Resource resource : resources) {
			if (resource.isReadable()) {
				MetadataReader metadataReader = metadataReaderFactory.getMetadataReader(resource);
				try {
					Class c = Class.forName(metadataReader.getClassMetadata().getClassName());
					if (c.getAnnotation(SystemGeneric.class) != null) {
						log.info("Generic System: providing " + Class.forName(metadataReader.getClassMetadata().getClassName()));
						userClasses.addUserClasse(Class.forName(metadataReader.getClassMetadata().getClassName()));
					}
				} catch (Throwable e) {
				}

			}
		}
		// return candidates;
	}

	private String resolveBasePackage(String basePackage) {
		return ClassUtils.convertClassNameToResourcePath(SystemPropertyUtils.resolvePlaceholders(basePackage));
	}

	@EventListener
	public void onApplicationEvent(ApplicationEvent event) throws ClassNotFoundException, IOException {
		System.out.println("ContextRefreshedEvent Start");
		if (event instanceof ContextRefreshedEvent) {
			System.out.println("ContextRefreshedEvent Received");
			ApplicationContext applicationContext = ((ContextRefreshedEvent) event).getApplicationContext();
			findMyTypes(applicationContext);
			// now you can do applicationContext.getBean(...)
			// ...
		}

	}

	// public void onStartup(@Observes AfterDeploymentValidation event, BeanManager beanManager) {
	// log.info("------------------start initialization-----------------------");
	// UserClassesProvider userClasses = getBean(UserClassesProvider.class, beanManager);
	// @SuppressWarnings("serial")
	// Set<Bean<?>> beans = beanManager.getBeans(Object.class, new AnnotationLiteral<Any>() {
	// });
	// for (Bean<?> bean : beans) {
	// Type clazz = bean.getBeanClass();
	// if (clazz instanceof Class) {
	// Class<?> classToProvide = (Class<?>) clazz;
	// if (classToProvide.getAnnotation(SystemGeneric.class) != null) {
	// log.info("Generic System: providing " + classToProvide);
	// userClasses.addUserClasse(classToProvide);
	// }
	// }
	// }
	// // Start Engine after deployment
	// getBean(Engine.class, beanManager);
	// // EventLauncher eventLauncher = getBean(EventLauncher.class, beanManager);
	// // eventLauncher.launchStartEvent();
	// log.info("-------------------end initialization------------------------");
	// }
	//
	// @SuppressWarnings("unchecked")
	// public static <T extends Object> T getBean(Class<T> clazz, BeanManager beanManager) {
	// Bean<?> bean = beanManager.resolve(beanManager.getBeans(clazz));
	// return (T) beanManager.getReference(bean, clazz, beanManager.createCreationalContext(bean));
	// }
}
