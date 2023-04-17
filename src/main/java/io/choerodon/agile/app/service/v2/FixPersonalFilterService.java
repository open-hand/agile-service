package io.choerodon.agile.app.service.v2;

import java.util.Set;

/**
 * @author superlee
 * @since 2022-11-30
 */
public interface FixPersonalFilterService {

    /**
     * 2.3.0修复个人筛选数据
     */
    void fix(Set<String> typeCodes);

    String convertJsonToAdvancedFilterJson(String typeCode, String json);
}
