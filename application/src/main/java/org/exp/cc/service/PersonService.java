package org.exp.cc.service;

import org.exp.cc.model.service.Result;

import java.util.Set;

/**
 * Person service.
 */
public interface PersonService {
    /**
     * Get person by given set of id.
     * @param id set of id
     * @return result
     */
    Result getPerson(Set<Object> id);
}
