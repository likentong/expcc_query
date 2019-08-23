package org.exp.cc.controller;

import org.exp.cc.annotation.ExceptionLogHandler;
import org.exp.cc.model.controller.Response;
import org.exp.cc.model.controller.demographic.DemographicRequest;
import org.exp.cc.model.service.Result;
import org.exp.cc.model.service.demographic.DemographicQuery;
import org.exp.cc.service.DemographicService;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Demographic controller.
 */
@RestController
@RequestMapping("/demographic")
public class DemographicController {
    private final DemographicService demographicService;

    public DemographicController(final DemographicService demographicService) {
        this.demographicService = demographicService;
    }

    @PostMapping(path = "/id/person_count", consumes = MediaType.APPLICATION_JSON_VALUE)
    @ExceptionLogHandler
    public Response getIDsWithPersonCount(@RequestBody final DemographicRequest demographicRequest) {
        final Result demographicResult = this.demographicService.getIDWithPersonCount(new DemographicQuery(demographicRequest.getQuery()));

        return new Response(demographicResult.getResult(), demographicResult.getSummary());
    }

}
