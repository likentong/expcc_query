package org.exp.cc.datastore.dao.person;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Person DAO.
 */
public interface PersonDAO {
    /**
     * Get person count by Demographic id.
     * @param id list of id
     * @return results
     */
    List<Map<String, Object>> getPersonCountByDemographicId(Set<Object> id);

    /**
     * Get person by Demographic Id.
     * @param id list of id
     * @return results
     */
    List<Map<String, Object>> getPersonByDemographicId(Set<Object> id);
}
