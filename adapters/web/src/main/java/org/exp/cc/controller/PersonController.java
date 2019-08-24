package org.exp.cc.controller;

import org.exp.cc.annotation.ExceptionLogHandler;
import org.exp.cc.model.controller.Response;
import org.exp.cc.model.service.Result;
import org.exp.cc.service.PersonService;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.Map;
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
    @ExceptionLogHandler
    public Response getPerson(@RequestParam(value = "demographic_id") Set<Object> demographicId) {
        final Result results = this.personService.getPerson(demographicId);
        return new Response(results.getResult(), results.getSummary());
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PostMapping(path = "/queue", consumes = MediaType.APPLICATION_JSON_VALUE)
    @ExceptionLogHandler
    public void sendToQueue(@RequestBody Map<String, Object> data) {
        this.personService.sendToPersonQueue(data);
    }

}
