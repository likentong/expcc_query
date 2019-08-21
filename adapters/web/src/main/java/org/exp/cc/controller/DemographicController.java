package org.exp.cc.controller;

import org.exp.cc.annotation.ExceptionHandler;
import org.exp.cc.model.demographic.controller.DemographicRequest;
import org.exp.cc.model.demographic.controller.DemographicResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
    private static final Logger logger = LoggerFactory.getLogger(DemographicController.class);

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @ExceptionHandler
    public DemographicResponse getIDsWithPersonCount(@RequestBody final DemographicRequest demographicRequest) {
        logger.info("demographic Request: {}", demographicRequest);
        throw new UnsupportedOperationException();
    }

}
