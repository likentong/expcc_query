package org.exp.cc.datastore.dao.person;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Person DAO.
 */
public interface PersonDAO {
    /**
     * Get person count by id.
     * @param id list of id
     * @return results
     */
    List<Map<String, Object>> getPersonCountById(Set<Object> id);

    /**
     * Get person by id.
     * @param id list of id
     * @return results
     */
    List<Map<String, Object>> getPersonById(Set<Object> id);
}
