package org.exp.cc.controller;

import org.exp.cc.model.demographic.controller.DemographicRequest;
import org.exp.cc.model.demographic.controller.DemographicResponse;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Demographic controller.
 */
@RestController
@RequestMapping("/demographic")
public class DemographicController {

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public DemographicResponse getIDsWithPersonCount(final DemographicRequest demographicRequest) {
       throw new UnsupportedOperationException();
    }

}
