package org.exp.cc.controller;

import org.exp.cc.annotation.ExceptionHandler;
import org.exp.cc.model.controller.Response;
import org.exp.cc.model.service.Result;
import org.exp.cc.service.PersonService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Set;

/**
 * Person controller.
 */
@RestController
@RequestMapping("/person")
public class PersonController {
    private final PersonService personService;

    public PersonController(final PersonService personService) {
        this.personService = personService;
    }

    @GetMapping
    @ExceptionHandler
    public Response getPerson(@RequestParam Set<Object> id) {
        final Result results = this.personService.getPerson(id);
        return new Response(results.getResult(), results.getSummary());
    }

}
