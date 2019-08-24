package org.exp.cc.service;

import org.exp.cc.model.service.Result;

import java.util.Map;
import java.util.Set;

/**
 * Person service.
 */
public interface PersonService {
    /**
     * Get person by given set of demographic id.
     * @param id id
     * @return result
     */
    Result getPerson(Set<Object> id);

    /**
     * Get person count by given set of demographic id.
     * @param id id
     * @return result
     */
    Result getPersonCountGroupByDemographicID(Set<Object> id);

    /**
     * Publish person data to queue.
     * @param person person
     */
    void sendToPersonQueue(Map<String, Object> person);
}
