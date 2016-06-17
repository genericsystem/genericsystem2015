package org.genericsystem.examplespring.controller;

import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.examplespring.bean.CarBean;
import org.genericsystem.examplespring.bean.CarBeanManager;
import org.genericsystem.examplespring.bean.ColorBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class GsSpringController {

	@Lazy
	@Autowired
	CarBeanManager carBeanManager;

	@Lazy
	@Autowired
	ColorBean colorBean;

	@RequestMapping(value = "/")
	public ModelAndView addCar(@RequestParam(required = false) String action, CarBean carBeanModel) {
		ModelAndView mv = new ModelAndView("index");

		if (action != null && action.equals("Add"))
			carBeanManager.addCar(carBeanModel);

		List<Generic> cars = carBeanManager.getCars();
		mv.addObject("sCars", cars);
		mv.addObject("sCarBeanManager", carBeanManager);
		mv.addObject("sColorBean", colorBean);
		mv.addObject("sCar", new CarBean());
		return mv;
	}

	@RequestMapping(value = "/update", method = RequestMethod.GET)
	public @ResponseBody String updateCar(@RequestParam(required = false) Long id, @RequestParam(required = false) String value, String action) {
		switch (action) {
		case "color":
			carBeanManager.getColor(id).setValue(value);
			break;
		case "power":
			carBeanManager.getPower(id).setValue(value);
			break;
		case "save":
			carBeanManager.flush();
			break;
		case "cancel":
			carBeanManager.clear();
			break;
		case "remove":
			carBeanManager.deleteCarById(id);
			break;
		}

		return "#";
	}

}